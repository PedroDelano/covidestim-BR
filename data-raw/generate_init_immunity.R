imm_state  <- readr::read_csv(
  'data-raw/imm-data/state-imm-init.csv',
  col_types = 'cnn'
)

imm_county <- readr::read_csv(
  'data-raw/imm-data/fips-imm-init.csv',
  col_types = 'cnn'
)

POPULATION_DATASET <- readxl::read_excel(
  "data-raw/pop-data/POP2024_20241230.xls", 
  skip=1, 
  sheet=2
)
