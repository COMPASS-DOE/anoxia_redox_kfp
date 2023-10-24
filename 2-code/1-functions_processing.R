# ANOXIA AND REDOX - 2023

## 1-functions_processing.R
## Use this script to import and process data files.

## KFP, July 2023

######################## ####
######################## ####


# initial setup -----------------------------------------------------------

gwc = tribble(
  ~location, ~gwc_percent,
  "upland-A", 49.85,
  "upland-B", 28.10,
  "transition-A", 101.57
)


reorder_factors = function(dat){
  dat %>% 
    mutate(location = factor(location, levels = c("upland-A", "transition-A", "upland-B")),
           timepoint = factor(timepoint, levels = c("time-zero", "12-hr", "24-hr", "2-wk", "4-wk")),
           treatment = factor(treatment, levels = c("water", "carbon")))
}

compute_dry_weights = function(sample_weights){
#  x =   
    sample_weights %>% 
    left_join(gwc) %>% 
    mutate_at(vars(starts_with("wt_")), as.numeric) %>% 
    mutate(wt_dry_soil_g = wt_field_moist_soil_g/((gwc_percent/100) + 1),
           vol_water_mL = wt_tube_optodes_soil_water_g - wt_dry_soil_g - wt_tube_g) %>% 
    dplyr::select(sample_label, wt_dry_soil_g, vol_water_mL)
  
  
}

#
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
   
  optode_processed
}

#
# process Shimadzu --------------------------------------------------------


import_weoc_data = function(FILEPATH){
  
  filePaths_weoc <- list.files(path = FILEPATH, pattern = "*.txt", full.names = TRUE)
  weoc_dat <- do.call(bind_rows, lapply(filePaths_weoc, function(path) {
    df <- read_tsv(path, skip = 10)
    df}))
  
}
process_weoc = function(weoc_data, sample_key, dry_weights){
  
  weoc_processed = 
    weoc_data %>% 
    # remove skipped samples
    filter(!`Sample ID` %in% "skip") %>% 
    # keep only relevant columns and rename them
    dplyr::select(`Sample Name`, `Sample ID`, `Result(NPOC)`) %>% 
    rename(sample_label = `Sample Name`,
           dilution = `Sample ID`,
           npoc_mgL = `Result(NPOC)`) %>% 
    # keep only sample rows 
    filter(grepl("redox", sample_label)) %>% 
    mutate(sample_label = str_replace(sample_label, "-", "_")) %>% 
    # do blank/dilution correction
    mutate(dilution = parse_number(dilution),
           dilution = replace_na(dilution, 1),
           npoc_corr_mgL = npoc_mgL * dilution) %>% 
    #  # join gwc and subsampling weights to normalize data to soil weight
    left_join(dry_weights) %>% 
    mutate(npoc_ugg = npoc_corr_mgL * (vol_water_mL/wt_dry_soil_g),
           npoc_ugg = round(npoc_ugg, 2),
           analysis = "WEOC") %>% 
    dplyr::select(sample_label, analysis, npoc_corr_mgL, npoc_ugg) %>% 
    force()
  
  weoc_processed

}

#
# process iron ------------------------------------------------------------

import_iron = function(FILEPATH = "1-data/microplate-iron"){
  
  # import map
  ferrozine_map = read_sheet("1VZ2F1Mg9LSdbJV1iV-3GN5Vv-JkUBP8qbIVNrK0A4mw", sheet = "ferrozine", col_types = "c") %>% mutate_all(as.character)

  # import data files (plate reader)
  filePaths_ferrozine <- list.files(path = FILEPATH, pattern = "csv", full.names = TRUE, recursive = TRUE)
  ferrozine_data <- do.call(bind_rows, lapply(filePaths_ferrozine, function(path) {
    df <- read.csv(path, header = TRUE, skip = 32) %>% mutate_all(as.character) %>% janitor::clean_names()
    df = df %>% mutate(source = basename(path))
    df}))
  
  list(ferrozine_map = ferrozine_map,
       ferrozine_data = ferrozine_data)
  
}
process_iron = function(ferrozine_map, ferrozine_data, dry_weights){
  
  # clean the map
  map_processed = 
    ferrozine_map %>% 
    filter(!skip %in% "skip") %>% 
    mutate(date = mdy(date)) %>% 
    fill(date, tray) %>% 
    pivot_longer(-c(date, tray, analysis, dilution, letter), names_to = "number", values_to = "sample_label") %>% 
    mutate_at(vars(c(dilution, number)), as.numeric) %>% 
    mutate(well_position = paste0(letter, number),
           dilution = if_else(is.na(dilution), 1, dilution)) %>% 
    drop_na() %>% 
    arrange(date, tray, number, letter) %>% 
    mutate(sample_type = case_when(grepl("mM", sample_label) ~ "standard",
                                   grepl("redox", sample_label) ~ "sample")) %>% 
    dplyr::select(date, tray, analysis, dilution, well_position, sample_label, sample_type)
  
  
  # clean the data
  data_processed = 
    ferrozine_data %>% 
    mutate_all(na_if,"") %>% 
    #dplyr::select(-x) %>% 
    fill(x) %>% 
    filter(x_1 == "562") %>% 
    dplyr::select(-x_1) %>% 
    pivot_longer(-c(source, x), values_to = "absorbance_562") %>% 
    mutate(name = str_remove(name, "x"),
           well_position = paste0(x, name),
           #region = str_extract(source, regex("cb|wle", ignore_case = TRUE)),
           #region = toupper(region),
           date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
           date = ymd(date),
           tray = str_extract(source, "tray[1-9][a-z]?"),
           tray = str_remove(tray, "tray"),
           # tray = parse_number(tray),
           absorbance_562 = as.numeric(absorbance_562)) %>% 
    dplyr::select(date, tray, well_position, absorbance_562) %>% 
    right_join(map_processed, by = c("date", "tray", "well_position")) %>% 
    filter(!grepl("skip", sample_label))
  
  calibrate_ferrozine_data = function(data_processed){
    # now do the calibrations
    # standards are in mM
    # 2 mM = 2 * 55 mg Fe in 1 L solution = 110 mg Fe in 1 L solution
    # therefore 2 mM = 110 mg/L or 110 ppm
    
    standards = 
      data_processed %>% 
      filter(grepl("standard", sample_type)) %>% 
      mutate(standard_mM = parse_number(sample_label),
             standard_ppm = standard_mM * 110/2) %>% 
      dplyr::select(date, tray, absorbance_562, standard_ppm) %>% 
      mutate(standard_ppm = as.numeric(standard_ppm))
    
    standards %>% 
      ggplot(aes(x = standard_ppm, y = absorbance_562, color = as.character(tray)))+
      geom_point()+
      geom_smooth(method = "lm", se = F)+
      facet_wrap(~date + tray)
    
    calibration_coef = 
      standards %>% 
      drop_na() %>% 
      dplyr::group_by(date) %>% 
      dplyr::summarize(slope = lm(absorbance_562 ~ standard_ppm)$coefficients["standard_ppm"], 
                       intercept = lm(absorbance_562 ~ standard_ppm)$coefficients["(Intercept)"])
    
    # y = mx + c
    # abs = m*ppm + c
    # ppm = abs-c/m
    
    # data_processed2 = 
    data_processed %>% 
      left_join(calibration_coef, by = c("date")) %>% 
      mutate(ppm_calculated = ((absorbance_562 - intercept) / slope))
    
  }
  
  samples = 
    calibrate_ferrozine_data(data_processed) %>% 
    filter(grepl("redox", sample_label)) %>% 
    mutate(#analysis = recode(analysis, "Fe2 (water only)" = "Fe2", "total-Fe (ascorbic)" = "Fe_total"),
           ppm_calculated = ppm_calculated * dilution) %>% 
    dplyr::select(sample_label, analysis, ppm_calculated) %>% 
    mutate(extract_type = str_extract(sample_label, "water|HCl"),
           sample_label = str_replace(sample_label, "redox-", "redox_"),
           sample_label = str_extract(sample_label, "redox_[0-9]{3}")) %>% 
    filter(!is.na(ppm_calculated)) %>% 
    group_by(sample_label, extract_type) %>% 
    pivot_wider(names_from = "analysis", values_from = "ppm_calculated") %>% 
    mutate(across(where(is.numeric), round, 2)) %>% 
    mutate(Fe3 = Fe_total - Fe2,
           Fe3 = Fe3 * 100/80,  # correct for 80% reduction efficiency of ascorbic acid
           Fe_total = Fe2 + Fe3 # re-calculate total-Fe based on corrected Fe3
           ) %>% 
    rename(Fe2_ppm = Fe2,
           Fe3_ppm = Fe3,
           FeTotal_ppm = Fe_total) %>% 
    ungroup() %>% 
    filter(extract_type == "HCl") %>% 
    dplyr::select(-extract_type) %>% 
    pivot_longer(cols = starts_with("Fe"), names_to = "analyte", values_to = "value") %>% 
    mutate(value = as.numeric(value))
    
  
  
##  samples_with_key = 
##    samples %>% 
##    left_join(sample_key)
##  
##  samples_with_key %>% 
##    filter(extract_type == "HCl") %>% 
##    ggplot(aes(x = location, y = Fe2_ppm/Fe3_ppm, color = timepoint))+
##    geom_point()+
##    facet_wrap(~treatment)
  
  samples2 = 
    samples %>% 
    rename(mgL = value) %>% 
    mutate(analyte = str_remove(analyte, "_ppm")) %>% 
    left_join(dry_weights) %>% 
    mutate(ugg = mgL * (30/wt_dry_soil_g), # used 30 mL HCl for extraction 
           ugg = round(ugg, 2)) %>% 
    dplyr::select(sample_label, analyte, mgL, ugg) %>% 
    pivot_longer(-c(sample_label, analyte)) %>% 
    mutate(name = paste0(analyte, "_", name)) %>% 
    dplyr::select(-analyte) %>% 
    filter(!grepl("blank", sample_label)) %>% 
    pivot_wider() %>% 
    mutate(analysis = "iron") %>% 
    dplyr::select(sample_label, analysis, starts_with("Fe"))
  
  samples2
}

#
# process sulfide ---------------------------------------------------------

import_sulfide = function(FILEPATH = "1-data/raw_data/microplate-sulfide"){
  
  # import map
  sulfide_map = read_sheet("1VZ2F1Mg9LSdbJV1iV-3GN5Vv-JkUBP8qbIVNrK0A4mw", sheet = "sulfide", col_types = "c") %>% mutate_all(as.character)
  
  # import data files (plate reader)
  filePaths_sulfide <- list.files(path = FILEPATH, pattern = "csv", full.names = TRUE, recursive = TRUE)
  sulfide_data <- do.call(bind_rows, lapply(filePaths_sulfide, function(path) {
    df <- read.csv(path, header = TRUE, skip = 32) %>% mutate_all(as.character) %>% janitor::clean_names()
    df = df %>% mutate(source = basename(path))
    df}))
  
  list(sulfide_map = sulfide_map,
       sulfide_data = sulfide_data)
  
}
process_sulfide = function(sulfide_map, sulfide_data, dry_weights){
  
  # clean the map
  map_processed = 
    sulfide_map %>% 
  #  filter(!skip %in% "skip") %>% 
    mutate(date = ymd(date)) %>% 
    fill(date, tray) %>% 
    pivot_longer(-c(date, tray, letter), names_to = "number", values_to = "sample_label") %>% 
    mutate_at(vars(c(number)), as.numeric) %>% 
    mutate(well_position = paste0(letter, number)) %>% 
    drop_na() %>% 
    arrange(date, tray, number, letter) %>% 
    mutate(sample_type = case_when(grepl("mM", sample_label) ~ "standard",
                                   grepl("redox", sample_label) ~ "sample")) %>% 
    dplyr::select(date, tray, well_position, sample_label, sample_type)
  
  
  # clean the data
  data_processed = 
    sulfide_data %>% 
    mutate_all(na_if,"") %>% 
    #dplyr::select(-x) %>% 
    fill(x) %>% 
    filter(x_1 == "670") %>% 
    dplyr::select(-x_1) %>% 
    pivot_longer(-c(source, x), values_to = "absorbance_670") %>% 
    mutate(name = str_remove(name, "x"),
           well_position = paste0(x, name),
           #region = str_extract(source, regex("cb|wle", ignore_case = TRUE)),
           #region = toupper(region),
           date = str_extract(source, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
           date = ymd(date),
           tray = str_extract(source, "Plate[1-9][a-z]?"),
           tray = str_remove(tray, "Plate"),
           # tray = parse_number(tray),
           absorbance_670 = as.numeric(absorbance_670)) %>% 
    dplyr::select(date, tray, well_position, absorbance_670) %>% 
    right_join(map_processed, by = c("date", "tray", "well_position")) %>% 
    filter(!is.na(absorbance_670))
  
  calibrate_sulfide_data = function(data_processed){
    # now do the calibrations
    # standards are in mM
    # 2.5 mM = 2.5 * 32.06 mg S in 1 L solution = 80.15 mg Fe in 1 L solution
    # therefore 2.5 mM = 80.15 mg/L or 80.15 ppm
    
    standards = 
      data_processed %>% 
      filter(grepl("standard", sample_type)) %>% 
      mutate(standard_mM = parse_number(sample_label),
             standard_ppm = standard_mM * 80.15/2.5) %>% 
      dplyr::select(date, tray, absorbance_670, standard_mM, standard_ppm) %>% 
      mutate(standard_ppm = as.numeric(standard_ppm)) %>% 
      filter(!is.na(absorbance_670))
    
    standards %>% 
      filter(standard_ppm < 9) %>% 
      ggplot(aes(x = standard_ppm, y = absorbance_670, color = as.character(tray)))+
      geom_point()+
      geom_smooth(method = "lm", se = F)+
      facet_wrap(~date + tray)
    
    calibration_coef = 
      standards %>% 
      filter(standard_ppm < 9) %>% 
      drop_na() %>% 
      dplyr::group_by(date) %>% 
      dplyr::summarize(slope = lm(absorbance_670 ~ standard_ppm)$coefficients["standard_ppm"], 
                       intercept = lm(absorbance_670 ~ standard_ppm)$coefficients["(Intercept)"])
    
    # y = mx + c
    # abs = m*ppm + c
    # ppm = abs-c/m
    
    # data_processed2 = 
    data_processed %>% 
      left_join(calibration_coef, by = c("date")) %>% 
      mutate(ppm_calculated = ((absorbance_670 - intercept) / slope))
    
  }
  
  samples = 
    calibrate_sulfide_data(data_processed) %>% 
    filter(grepl("redox|blank", sample_label)) %>% 
 #   mutate(#analysis = recode(analysis, "Fe2 (water only)" = "Fe2", "total-Fe (ascorbic)" = "Fe_total"),
 #      ppm_calculated = ppm_calculated * dilution) %>% 
    dplyr::select(sample_label, absorbance_670, ppm_calculated) %>% 
    mutate(
           sample_label = str_replace(sample_label, "redox-", "redox_"),
           #sample_label = str_extract(sample_label, "redox_[0-9]{3}")
           ) %>% 
    filter(!is.na(ppm_calculated)) %>% 
    mutate(ppm_calculated = as.numeric(ppm_calculated))
  
  samples2 = 
    samples %>% 
    rename(mgL = ppm_calculated) %>% 
    left_join(dry_weights) %>% 
    mutate(ugg = mgL * (vol_water_mL/wt_dry_soil_g),
           ugg = round(ugg, 2)) %>% 
    dplyr::select(sample_label, mgL, ugg) %>% 
    rename(sulfide_mgL = mgL,
           sulfide_ugg = ugg) %>% 
#    filter(!grepl("blank", sample_label)) %>% 
    mutate(analysis = "sulfide") 
  
  samples2
}

#
# process IC ions ---------------------------------------------------------

import_ions = function(FILEPATH){
  
  anions <- 
    list.files(path=FILEPATH, pattern = c("anion", ".csv"), full.names = TRUE) %>% 
    lapply(read_csv, skip = 5, id = "source") %>% 
    bind_rows %>% 
    rename(sample_label = "...3") %>% 
    mutate_all(as.character)
  
  cations <- 
    list.files(path=FILEPATH, pattern = c("cation", ".csv"), full.names = TRUE) %>% 
    lapply(read_csv, skip = 5, id = "source") %>% 
    bind_rows %>% 
    rename(sample_label = "...3") %>% 
    mutate_all(as.character)
  
  ions <- 
    bind_rows(cations, anions)
  
  ions
}
process_ions = function(ions_data, dry_weights){
  
  ions = 
    ions_data %>% 
    mutate_all(as.character) %>% 
    dplyr::select(-starts_with("...")) %>% 
    pivot_longer(-c(sample_label, source)) %>% 
    mutate(date_run = str_extract(source, "[0-9]{8}"),
           date_run = lubridate::ymd(date_run)) %>% 
    # remove "Nitrate" and "Nitrite" because we will use the UV-detector versions
    filter(!name %in% c("Nitrate", "Nitrite")) %>% 
    mutate(value = recode(value, "n.a." = "0"),
           name = str_remove(name, " UV"),
           value = as.numeric(value)) %>% 
    filter(!is.na(value)) %>% 
    rename(ppm = value,
           ion = name) %>% 
    filter(!ion %in% c("Lithium", "Nitrite"))
  
  blanks = 
    ions %>% 
    filter(grepl("redox", sample_label, ignore.case = T)) %>% 
    filter(grepl("blank", sample_label, ignore.case = T))
  
  samples = 
    ions %>% 
    filter(grepl("redox", sample_label, ignore.case = T)) %>% 
    filter(!grepl("blank", sample_label, ignore.case = T)) %>% 
    mutate(dilution_factor = parse_number(str_extract(sample_label, "_[0-9]x")),
           dilution_factor = if_else(is.na(dilution_factor), 1, dilution_factor),
           ppm_corrected = ppm * dilution_factor,
           sample_label = str_remove(sample_label, "_[0-9]x"),
           sample_label = tolower(sample_label))
    
  checking_dilutions = function(samples){
    # the same dilution may not work for all ions
    # so, plot the samples with multiple dilutions to see if there's an easy way to decide
    # unfortunately, this must be done manually.

    samples_dilution = 
      samples %>% 
      group_by(sample_label, ion) %>% 
      dplyr::mutate(n = n()) %>% 
      filter(n > 1)
    
    samples_dilution %>% 
      ggplot(aes(x = sample_label, y = ppm, color = as.character(dilution_factor)))+
      geom_point()+
      facet_wrap(~ion, scales = "free")
  
    # so far: for sulfate, use dilution = 2x/4x, whichever is higher
    # for all others, use dilution = 1  
    
  }
  
  samples_nonSO4 = 
    samples %>% 
    filter(!ion == "Sulfate") %>% 
    filter(dilution_factor == 1)
  
  samples_SO4 = 
    samples %>% 
    filter(ion == "Sulfate") %>% 
    group_by(sample_label) %>% 
    dplyr::mutate(max = dilution_factor == max(dilution_factor)) %>% 
    filter(max) %>% 
    dplyr::select(-max)

  samples_corrected = 
    bind_rows(samples_nonSO4, samples_SO4) %>% 
    dplyr::select(sample_label, ion, ppm_corrected) %>% 
    mutate(ion = tolower(ion),
           ion = paste0(ion, "_ppm")) %>% 
    rename(analyte = ion,
           value = ppm_corrected) %>% 
    filter(analyte %in% c("sodium_ppm", "calcium_ppm", "chloride",
                          "ammonium_ppm", "nitrate_ppm",
                          "sulfate_ppm", "phosphate_ppm"))
  
  
  samples_corrected_ugg = 
    samples_corrected %>% 
    rename(mgL = value) %>% 
    mutate(analyte = str_remove(analyte, "_ppm")) %>% 
    left_join(dry_weights) %>% 
    mutate(ugg = mgL * (vol_water_mL/wt_dry_soil_g),
           ugg = round(ugg, 2)) %>% 
    dplyr::select(sample_label, analyte, mgL, ugg) %>% 
    pivot_longer(-c(sample_label, analyte)) %>% 
    mutate(name = paste0(analyte, "_", name)) %>% 
    dplyr::select(-analyte) %>% 
    filter(!grepl("blank", sample_label)) %>% 
    pivot_wider() %>% 
    mutate(analysis = "IC") %>% 
    dplyr::select(sample_label, analysis, ends_with("mgL"), ends_with("ugg"))
  
  samples_corrected_ugg
}

#
# process GHG -------------------------------------------------------------

process_ghg = function(ghg, dry_weights){
  ghg_processed = 
    ghg %>% 
    filter(!is.na(treatment)) %>%
    left_join(dry_weights) %>% 
    mutate(CO2_ugg = CO2_post * vol_water_mL/wt_dry_soil_g,
           CH4_ugg = (CH4_post/1000) * vol_water_mL/wt_dry_soil_g) %>% 
    reorder_factors() 
  
}

#
# combined data -----------------------------------------------------------

combine_data = function(iron_processed, ions_ic_processed, weoc_processed, sulfide_processed){
  
  combined = 
    bind_rows(
      iron_processed, ions_ic_processed, weoc_processed, sulfide_processed
    ) %>% 
    pivot_longer(cols = -c(sample_label, analysis), names_to = "analyte") %>% 
    drop_na()
  
  combined
}
