# ANOXIA AND REDOX - 2023

## 2-functions_analysis.R
## Use this script for data analysis and visualization.

## KFP, October 2023

######################## ####
######################## ####


  
make_optode_graphs = function(optode_processed, orp, sample_key){
  
  optode_processed = 
    optode_processed %>% 
    mutate(treatment = factor(treatment, levels = c("water", "carbon")))
  
  # plot only optode data
  optode_ts = 
    optode_processed %>% 
    filter(!is.na(treatment)) %>% 
    #  filter(optode_disc_number == "4038") %>% 
    arrange(time_minutes) %>% 
    ggplot(aes(x = time_minutes2/60/24, y = do_mg_L, color = location, group = sample_label))+
    geom_line(linewidth = 0.5,
              show.legend = F)+
    geom_point(size = 1, show.legend = F)+
    facet_wrap(~treatment + location)+
    labs(x = "time, day")+
    #xlim(0,2)+
    NULL

  # plot optode + overlay Firesting data
  optode_final_datetime = 
    optode_processed %>% 
    group_by(sample_label) %>% 
    dplyr::summarise(time_minutes2 = max(time_minutes2))
  
  firesting_do = 
    orp %>% 
    dplyr::select(sample_label, DO_mgL) %>% 
    left_join(optode_final_datetime) %>% 
    left_join(sample_key) %>% 
    drop_na() %>% 
    mutate(treatment = factor(treatment, levels = c("water", "carbon")))
  
  optode_firesting = 
    optode_processed %>% 
    filter(!is.na(treatment)) %>% 
    #  filter(optode_disc_number == "4038") %>% 
    arrange(time_minutes) %>% 
    ggplot(aes(x = time_minutes2/60/24, y = do_mg_L, color = location, group = sample_label))+
    geom_line(linewidth = 0.5,
              show.legend = F)+
    geom_point(size = 1, show.legend = F)+
    facet_wrap(~treatment + location)+
    labs(x = "time, day")+
    geom_point(data = firesting_do, aes(y = DO_mgL), size = 3, color = "black", shape = 1)+
    #xlim(0,2)+
    NULL
  
  list(optode_ts = optode_ts,
       optode_firesting = optode_firesting)
}


make_chemistry_graphs = function(combined_data, orp, ghg_processed, sample_key){
  
  combined_key = 
    combined_data %>% 
    left_join(sample_key) %>% 
    reorder_factors()
  
  ## all variables
  combined_key %>% 
    ggplot(aes(x = location, y = value, shape = treatment, color = timepoint))+
    geom_point(size = 3, stroke = 1,
               position = position_dodge(width = 0.3))+
    scale_shape_manual(values = c(1, 16))+
    facet_wrap(~analyte , scales = "free_y")
  
  combined_key %>% 
    separate(analyte, sep = "_", into = c("analyte")) %>% 
    ggplot(aes(x = timepoint, y = value, shape = treatment, color = treatment))+
    geom_point(size = 3, stroke = 1,
               position = position_dodge(width = 0.3))+
    scale_shape_manual(values = c(1, 16))+
    #  facet_grid(analyte ~ location, scales = "free_y")+
    facet_wrap(~analyte + location, scales = "free_y")+
    labs(y = "mg/L")
  
  
  # weoc
  gg_weoc = 
    combined_key %>% 
    filter(analysis == "WEOC" & grepl("_ugg", analyte)) %>% 
    ggplot(aes(x = location, y = value, color = timepoint))+
    geom_point(size = 3,
               position = position_dodge(width = 0.2))+
    facet_grid(analyte ~ treatment)
  
  # iron  
  iron = 
    combined_key %>% 
    filter(analysis == "iron") %>% 
    pivot_wider(names_from = "analyte", values_from = "value") %>% 
    mutate(Fe23 = Fe2_ugg/Fe3_ugg)
  
  gg_iron = 
    combined_key %>% 
    filter(analysis == "iron" & grepl("_ugg", analyte)) %>% 
    ggplot(aes(x = location, y = value, color = timepoint))+
    geom_point(size = 3,
               position = position_dodge(width = 0.2))+
    facet_grid(analyte ~ treatment)
  
  gg_iron_ratio = 
    iron %>% 
    ggplot(aes(x = location, y = Fe23, color = timepoint))+
    geom_point(size = 3,
               position = position_dodge(width = 0.2))+
    facet_grid(. ~ treatment)+
    labs(y = "Fe2/Fe3")
  
  # DIN
  nitrogen = 
    combined_key %>% 
    filter(analyte %in% c("ammonium_ugg", "nitrate_ugg")) %>% 
    pivot_wider(names_from = "analyte", values_from = "value") %>% 
    mutate(amm_nitr = ammonium_ugg/nitrate_ugg)
  
  gg_nitrogen = 
    combined_key %>% 
    filter(analyte %in% c("ammonium_ugg", "nitrate_ugg")) %>% 
    ggplot(aes(x = location, y = value, color = timepoint))+
    geom_point(size = 3,
               position = position_dodge(width = 0.2))+
    facet_grid(analyte ~ treatment, scales = "free_y")
  
  gg_nitrogen_ratio = 
    nitrogen %>% 
    ggplot(aes(x = location, y = amm_nitr, color = timepoint))+
    geom_point(size = 3,
               position = position_dodge(width = 0.2))+
    facet_wrap(~treatment)+
    labs(y = "ammonium/nitrate")
  
  
  # sulfur
  sulfur = 
    combined_key %>% 
    filter(analyte %in% c("sulfate_ugg", "sulfide_ugg")) %>% 
    dplyr::select(-analysis) %>% 
    pivot_wider(names_from = "analyte", values_from = "value") %>% 
    mutate(sulfide_sulfate = sulfide_ugg/sulfate_ugg)
  
  gg_sulfur =
    combined_key %>% 
    filter(analyte %in% c("sulfate_ugg", "sulfide_ugg")) %>% 
    ggplot(aes(x = location, y = value, color = timepoint))+
    geom_point(size = 3,
               position = position_dodge(width = 0.2))+
    facet_grid(analyte ~ treatment, scales = "free_y")
  
  gg_sulfur_ratio = 
    sulfur %>% 
    ggplot(aes(x = location, y = sulfide_sulfate, color = timepoint))+
    geom_point(size = 3,
               position = position_dodge(width = 0.2))+
    facet_wrap(~treatment)+
    labs(y = "sulfide/sulfate")
  
  # ORP
  orp_long = 
    orp %>% 
    dplyr::select(sample_label, orp_mV, pH, EC_mScm, DO_mgL) %>% 
    pivot_longer(-sample_label) %>% 
    left_join(sample_key) %>% 
    mutate(value = as.numeric(value)) %>% 
    reorder_factors() %>%  
    drop_na()
  
  gg_orp = 
    orp_long %>% 
    ggplot(aes(x = location, y = value, color = timepoint))+
    geom_point(size = 3, stroke = 1,
               position = position_dodge(width = 0.3))+
    scale_shape_manual(values = c(1, 16))+
    facet_grid(name~treatment, scales = "free_y")
  
  # ghg
  ghg_long = 
    ghg_processed %>% 
    dplyr::select(sample_label, CO2_ugg, CH4_ugg) %>% 
    pivot_longer(cols = -sample_label) %>% 
    left_join(sample_key) %>% 
    reorder_factors()
  
  gg_ghg_ratio = 
    ghg_processed %>% 
    ggplot(aes(x = location, y = CO2_ugg/(CH4_ugg), color = timepoint))+
    geom_point(size = 3, stroke = 1,
               position = position_dodge(width = 0.3))+
    scale_shape_manual(values = c(1, 16))+
    facet_wrap(~treatment)+
    labs(y = "CO2 ugg / CH4 ugg")
  
  gg_ghg = 
    ghg_long %>% 
    ggplot(aes(x = location, y = value, color = timepoint))+
    geom_point(size = 3, stroke = 1,
               position = position_dodge(width = 0.3))+
    scale_shape_manual(values = c(1, 16))+
    facet_grid(name ~ treatment, scales = "free_y")

  
  list(gg_weoc = gg_weoc,
       gg_iron = gg_iron,
       gg_iron_ratio = gg_iron_ratio,
       gg_nitrogen = gg_nitrogen,
       gg_nitrogen_ratio = gg_nitrogen_ratio,
       gg_sulfur = gg_sulfur,
       gg_sulfur_ratio = gg_sulfur_ratio,
       gg_ghg = gg_ghg,
       gg_ghg_ratio = gg_ghg_ratio,
       gg_orp = gg_orp
  )
  
  
  }