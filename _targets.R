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


# Replace the target list below with your own:
list(
  tar_target(
    name = data,
    command = tibble(x = rnorm(100), y = rnorm(100))
#   format = "feather" # efficient storage of large data frames # nolint
  ),
  tar_target(
    name = model,
    command = coefficients(lm(y ~ x, data = data))
  )
)
