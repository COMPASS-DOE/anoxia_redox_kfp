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
source("2-code/2-functions_analysis.R")

# Replace the target list below with your own:
list(
  tar_target(sample_key_file, "1-data/sample_key.csv", format = "file"),
  tar_target(sample_key, read.csv(sample_key_file)),
  tar_target(sample_weights_file, "1-data/sample_weights.csv", format = "file"),
  tar_target(sample_weights, read.csv(sample_weights_file)),
  tar_target(dry_weights, compute_dry_weights(sample_weights)),
  
  # optode
  tar_target(optode_map, read_sheet("1eFDvH17jjYRsDIx_J8ep_QzPAOO5LBXiGnFZOoA9fSM") %>% mutate_all(as.character)),
  tar_target(optode_data, import_optode_data("1-data/raw_data/optodes")),
  tar_target(optode_processed, process_optode_data(optode_data, optode_map, sample_key)),
  
  # TOC
  tar_target(weoc_data, import_weoc_data(FILEPATH = "1-data/raw_data/doc")),
  tar_target(weoc_processed, process_weoc(weoc_data, sample_key, dry_weights)),
  
  # Iron - ferrozine
  tar_target(ferrozine_map, import_iron(FILEPATH = "1-data/raw_data/microplate-iron")$ferrozine_map),
  tar_target(ferrozine_data, import_iron(FILEPATH = "1-data/raw_data/microplate-iron")$ferrozine_data),
  tar_target(iron_processed, process_iron(ferrozine_map, ferrozine_data, dry_weights)),
  
  # Sulfide
  tar_target(sulfide_map, import_sulfide(FILEPATH = "1-data/raw_data/microplate-sulfide")$sulfide_map),
  tar_target(sulfide_data, import_sulfide(FILEPATH = "1-data/raw_data/microplate-sulfide")$sulfide_data),
  tar_target(sulfide_processed, process_sulfide(sulfide_map, sulfide_data, dry_weights)),
  
  # IC ions
  tar_target(ions_data, import_ions(FILEPATH = "1-data/raw_data/ions")),
  tar_target(ions_ic_processed, process_ions(ions_data, dry_weights)),
  
  # ORP-pH-DO
  tar_target(orp, googlesheets4::read_sheet("1xo-PzO0yxztvpFxlcaXbW5TEdAgz25ToN3R1qVORf8I")),
  tar_target(ghg, googlesheets4::read_sheet("1uFvGIBwdrwK2nmvcpnkD9nubjlnLj973qr8qPuv7S7E")),
  tar_target(ghg_processed, process_ghg(ghg, dry_weights)),
  
  tar_target(combined_data, combine_data(iron_processed, ions_ic_processed, weoc_processed, sulfide_processed)),
  
  # graphs
  tar_target(gg_optodes, make_optode_graphs(optode_processed, orp, sample_key)),
  tar_target(gg_chemistry, make_chemistry_graphs(combined_data, orp, ghg_processed, sample_key)),
  
  tar_render(report, path = "3-reports/anoxia-redox-report.Rmd")
)
