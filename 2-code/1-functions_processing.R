# ANOXIA AND REDOX - 2023

## 1-functions_processing.R
## Use this script to import and process data files.

## KFP, July 2023

######################## ####
######################## ####


# process data - optodes --------------------------------------------------
import_optode_data = function(FILEPATH){
  filePaths_spectra <- list.files(path = FILEPATH,pattern = c("results", ".csv"), full.names = TRUE)
  spectra_dat <- do.call(rbind, lapply(filePaths_spectra, function(path) {
    df <- read.csv(path, header=TRUE, skip = 4, check.names = F)
    df %<>%
      rownames_to_column("timestep") %>% 
      force()
    df[["source"]] <- rep(path, nrow(df))
    df}))
}

optode_data = import_optode_data(FILEPATH = "1-data/optode")

process_optode_data = function(optode_data, optode_map, sample_key){
  
  optode_long = 
    optode_data %>% 
    mutate(start_date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
           start_date = lubridate::ymd(start_date),
           time_interval = str_extract(source, "[0-9]{1,2}min"),
           time_interval = parse_number(time_interval),
           time_minutes = as.numeric(timestep) * time_interval) %>% 
    dplyr::select(-c(source, timestep, time_interval)) %>% 
    pivot_longer(-c(start_date, time_minutes), names_to = "optode_disc_number", values_to = "do_mg_L")
  
  # sample_key = read_sheet("1A72s9JH1E91M_1IfvrBkz6rQdwnT0z6hFmL5ELYw8y8")
  # optode_map = read_sheet("1eFDvH17jjYRsDIx_J8ep_QzPAOO5LBXiGnFZOoA9fSM") %>% mutate_all(as.character)
  
  optode_processed = 
    optode_long %>% 
    left_join(optode_map %>% dplyr::select(start_datetime, optode_disc_number, sample_label) %>% 
                mutate(start_datetime = lubridate::ymd_hms(start_datetime))) %>% 
    left_join(sample_key) 
  
  # optode_processed %>% 
  #   arrange(time_minutes) %>% 
  #   ggplot(aes(x = time_minutes, y = do_mg_L, color = optode_disc_number))+
  #   geom_line()+
  #   facet_wrap(~location)
  
  
  optode_processed
}

#