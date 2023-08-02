# ANOXIA AND REDOX - 2023

## 1-functions_processing.R
## Use this script to import and process data files.

## KFP, July 2023

######################## ####
######################## ####


# process data - optodes --------------------------------------------------
import_optode_data = function(FILEPATH){
  filePaths_spectra <- list.files(path = FILEPATH,pattern = c("results", ".csv"), full.names = TRUE, recursive = TRUE)
  spectra_dat <- do.call(bind_rows, lapply(filePaths_spectra, function(path) {
    df <- read.csv(path, header=TRUE, skip = 4, check.names = F)
    df %<>% 
      drop_na() %>% 
      rownames_to_column("timestep") %>% 
      force()
    df[["source"]] <- rep(path, nrow(df))
    df}))
}

optode_data = import_optode_data(FILEPATH = "1-data/optodes")

process_optode_data = function(optode_data, optode_map, sample_key){
  
  optode_long = 
    optode_data %>% 
    mutate(start_date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
           start_date = lubridate::ymd(start_date),
           time_interval = str_extract(source, "[0-9]{1,3}min"),
           time_interval = parse_number(time_interval),
           time_minutes = as.numeric(timestep) * time_interval,
           time_minutes = case_when(time_interval == 3 ~ time_minutes,
                                    time_interval == 15 ~ time_minutes + 60,
                                    time_interval == 480 ~ time_minutes + (60*24),
                                    time_interval == 720 ~ time_minutes + (60*24))) %>% 
    dplyr::select(-c(source, timestep, time_interval)) %>% 
    pivot_longer(-c(start_date, time_minutes, datetime), names_to = "optode_disc_number", values_to = "do_mg_L") %>% 
    filter(!is.na(do_mg_L))
  
  # sample_key = read_sheet("1A72s9JH1E91M_1IfvrBkz6rQdwnT0z6hFmL5ELYw8y8")
  # optode_map = read_sheet("1eFDvH17jjYRsDIx_J8ep_QzPAOO5LBXiGnFZOoA9fSM") %>% mutate_all(as.character)
  
  optode_processed = 
    optode_long %>% 
    mutate(datetime = case_match(datetime, "" ~ NA_character_, .default = datetime)) %>% 
    group_by(start_date, optode_disc_number) %>% 
    dplyr::mutate(datetime = mdy_hm(datetime),
           datetime_elapsed = as.integer(difftime(datetime, min(datetime, na.rm = T), units = "mins")),
           time_minutes2 = if_else(!is.na(datetime_elapsed), datetime_elapsed, time_minutes)) %>% 
    left_join(optode_map %>% dplyr::select(start_datetime, optode_disc_number, sample_label) %>% 
                mutate(start_datetime = lubridate::ymd_hms(start_datetime))) %>% 
    left_join(sample_key) %>% 
    filter(sample_label != "redox_042")
  
   optode_processed %>% 
     filter(!is.na(treatment)) %>% 
   #  filter(optode_disc_number == "4038") %>% 
     arrange(time_minutes) %>% 
     ggplot(aes(x = time_minutes2/60/24, y = do_mg_L, color = sample_label))+
     geom_line(linewidth = 0.5,
               show.legend = F)+
     geom_point(size = 2, show.legend = F)+
     facet_wrap(~treatment + location)+
     labs(x = "time, day")+
     #xlim(0,2)+
     NULL
  
   optode_processed %>% 
     #  filter(optode_disc_number == "4038") %>% 
     filter(!is.na(treatment)) %>% 
     arrange(time_minutes2) %>% 
     ggplot(aes(x = time_minutes2/60/24, y = do_mg_L, #group = sample_label, 
                color = treatment))+
    # geom_smooth(linewidth = 2, se = F)+
     geom_line(aes(group = sample_label), alpha = 0.5, size = 1)+
     labs(x = "time, day")+
     facet_wrap(~location)+
     #xlim(0,2)+
     NULL
   
   
  optode_processed
}

# optode_processed %>% write.csv("1-data/optode_processed_water_2wk_first24hr.csv", row.names = F, na = "")
# 

# process Shimadzu --------------------------------------------------------


import_weoc_data = function(FILEPATH){
  
  filePaths_weoc <- list.files(path = FILEPATH, pattern = "*.txt", full.names = TRUE)
  weoc_dat <- do.call(bind_rows, lapply(filePaths_weoc, function(path) {
    df <- read_tsv(path, skip = 10)
    df}))
  
}

weoc_data = import_weoc_data(FILEPATH = "1-data/doc")
process_weoc = function(weoc_data, sample_key, dry_weight){
  
  npoc_processed = 
    weoc_data %>% 
    # remove skipped samples
    filter(!`Sample ID` %in% "skip") %>% 
    # keep only relevant columns and rename them
    dplyr::select(`Sample Name`, `Result(NPOC)`) %>% 
    rename(sample_label = `Sample Name`,
           npoc_mgL = `Result(NPOC)`) %>% 
    # keep only sample rows 
    filter(grepl("redox", sample_label)) %>% 
    mutate(sample_label = str_replace(sample_label, "-", "_")) %>% 
    # join the analysis key to get the sample_label
    left_join(sample_key) %>% 
    filter(!is.na(location))
    # do blank/dilution correction
 #  mutate(blank_mgL = case_when(sample_name == "blank-filter" ~ npoc_mgL)) %>% 
 #  fill(blank_mgL, .direction = c("up")) %>% 
 #  mutate(NPOC_dilution = as.numeric(NPOC_dilution),
 #         npoc_corr_mgL = (npoc_mgL-blank_mgL) * NPOC_dilution) %>% 
 #  # join gwc and subsampling weights to normalize data to soil weight
 #  left_join(dry_weight) %>% 
 #  mutate(npoc_ug_g = npoc_corr_mgL * ((water_g + soilwater_g)/od_g),
 #         npoc_ug_g = round(npoc_ug_g, 2)) %>% 
 #  dplyr::select(sample_name, npoc_corr_mgL, npoc_ug_g) %>% 
 #  force()
  
  npoc_processed
  
  npoc_processed %>% 
    ggplot(aes(x = timepoint, y = npoc_mgL, color = location))+
    geom_point(position = position_dodge(width = 0.5))+
    facet_wrap(~treatment)
  
  
}

#

