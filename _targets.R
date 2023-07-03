# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with your custom functions:
source("2-code/0-packages.R")
#source("2-code/0b-initial_processing.R")
source("2-code/1-functions_processing.R")

# Replace the target list below with your own:
list(
  tar_target(sample_key_file, "1-data/sample_key.csv", format = "file"),
  tar_target(sample_key, read.csv(sample_key_file)),
  tar_target(sample_weights_file, "1-data/sample_weights.csv", format = "file"),
  tar_target(sample_weights, read.csv(sample_weights_file)),
  
  # optode
  tar_target(optode_map, read_sheet("1eFDvH17jjYRsDIx_J8ep_QzPAOO5LBXiGnFZOoA9fSM") %>% mutate_all(as.character)),
  tar_target(optode_data, import_optode_data("1-data/optodes")),
  tar_target(optode_data_processed, process_optode_data(optode_data, optode_map, sample_key))
  
  
)
