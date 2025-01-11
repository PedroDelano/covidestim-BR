#' Estimated IFR for US counties and states
#'
#' Look in `data-raw/` for details on how this is generated.
#'
#' @param region A string with the state name, or the FIPS code
#'
#' @return A data.frame with two variables: [date, value], where value is
#'   the odds ratio on VALUE HERE for that particular day. If \code{region}
#'   is specified incorrectly, an error will be thrown.
#'
#' @examples
#' \dontrun{
#'   get_ifr_raw('Connecticut')
#'   get_ifr_raw('09009')
#' }
get_ifr_raw <- function(region) {
  found_state <- FALSE
  region_is_county <- any(region %in% ifr_county$fips)
  
  # Branch for case when 'region' is a state
  if (region %in% ifr_state$State)
    found_state <- ifr_state$ifr_OR[ifr_state$State==region]
  # Branch for case when 'region' is not a state and is probably a county
  else if (region_is_county)
    found_state <- local({
      state_name <- dplyr::filter(ifr_county, fips == region)$State[[1]]

      ifr_state$ifr_OR[ifr_state$State==state_name]
    })
  else
    stop("`region` was neither a US state name or a character FIPS code")

  successful_state_find <- function(candidate) is.numeric(candidate)

  if (!successful_state_find(found_state))
    stop(glue::glue("Could not find state-level IFR data for region {region}!"))

  if (!region_is_county)
    return(found_state)

  found_county <- dplyr::filter(ifr_county, fips == region)

  if (nrow(found_county) > 1)
    stop(glue::glue("Expected `found_county` to be scalar, but matched more than 1 row"))

  # Check to be sure we found county IFR data
  if (nrow(found_county) == 0)
    stop(glue::glue("Could not find county-level IFR information for county {region}"))

  # Multiply the enclosing state's `value` column by the `comorb_OR` scalar
  found_state * found_county$ifr_OR
}

#' Generate IFR adjustments for Brazilian municipalities
#' @param first_date The start date of the analysis period
#' @param N_weeks_before Number of weeks before first date to model
#' @param municipality_id The 6-digit IBGE code for the municipality
#' @return List containing IFR adjustment parameters
#' @export
gen_ifr_adjustments <- function(first_date, N_weeks_before, municipality_id) {

  logger::log_info("Using custom IFR function for Brazil")

  # Start date for IFR calculations
  ifr_adj_start <- first_date - lubridate::weeks(N_weeks_before)
  
  # Get municipality-specific factors (age structure, healthcare access, etc)
  # TODO: Replace with actual Brazilian demographic/healthcare data
  ifr_adj_fixed <- 1.0  # Placeholder - should be calculated based on municipality characteristics
  
  # Generate time-based IFR adjustment vector
  # This models potential improvements in treatment over time
  ifr_adj_df <- tibble::tibble(
    date = seq.Date(ifr_adj_start, lubridate::ymd('2024-12-31'), by = '1 week'),
    value = 1 - pnorm(
      as.numeric(seq.Date(ifr_adj_start, lubridate::ymd("2024-12-31"), by = '1 week')),
      lubridate::ymd("2021-1-1"),  # Adjust this date based on Brazilian context
      21
    )
  )
  
  # Create adjustment vector
  ifr_adj <- dplyr::pull(ifr_adj_df, value)
  
  list(
    ifr_adj_fixed = ifr_adj_fixed,
    ifr_adj = ifr_adj,
    N_ifr_adj = length(ifr_adj)
  )
}