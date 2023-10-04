# ANOXIA AND REDOX - 2023

## 0b-initial_processing.R
## Use this script to download data and metadata files from Google Drive.

## KFP, July 2023

######################## ####
######################## ####

# load packages -----------------------------------------------------------
source("2-code/0-packages.R")



## INITIAL DOC CONCENTRATIONS:
## initial concentration of stock DOC extract
ahorizon = mean(c(8.578, 8.094, 8.176)) * 3
bhorizon = mean(c(3.870, 3.756, 3.831)) * 3


#
# download sample metadata files ----------------------------------------------
## sample key
# load and clean sample key
sample_key = read_sheet("1A72s9JH1E91M_1IfvrBkz6rQdwnT0z6hFmL5ELYw8y8") %>% 
  mutate_all(as.character) %>% 
  dplyr::select(sample_label, site, treatment, timepoint, location, replicate)

# export
sample_key %>% write.csv("1-data/sample_key.csv", row.names = FALSE, na = "")


## sample weights
# load and clean sample weight
sample_weights = read_sheet("17rHP_e6ZNZ7jxNaEEZTquTd3PMhLOoAv-uYZlyI_rbQ") %>% mutate_all(as.character)
# export
sample_weights %>% write.csv("1-data/sample_weights.csv", row.names = F, na = "")
