# ANOXIA AND REDOX - 2023

## 2-functions_analysis.R
## Use this script for data analysis and visualization.

## KFP, October 2023

######################## ####
######################## ####


reorder_factors = function(dat){
  dat %>% 
    mutate(location = factor(location, levels = c("upland-A", "transition-A", "upland-B")),
           timepoint = factor(timepoint, levels = c("time-zero", "12-hr", "24-hr", "2-wk", "4-wk")),
           treatment = factor(treatment, levels = c("water", "carbon")))
}
  
make_optode_graphs = function(optode_processed, orp){
  
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


make_chemistry_graphs = function(combined_data, sample_key){
  
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
  
  
  
  #  gg_iron = 
  combined_key %>% 
    filter(analysis == "iron") %>% 
    ggplot(aes(x = location, y = value, color = timepoint))+
    geom_point(size = 3,
               position = position_dodge(width = 0.2))+
    facet_grid(analyte ~ treatment)
  
  iron = 
    combined_key %>% 
    filter(analysis == "iron") %>% 
    pivot_wider(names_from = "analyte", values_from = "value") %>% 
    mutate(Fe23 = Fe2_ppm/Fe3_ppm)
  
  iron %>% 
    ggplot(aes(x = location, y = Fe23, color = timepoint))+
    geom_point(size = 3,
               position = position_dodge(width = 0.2))+
    facet_grid(. ~ treatment)+
    labs(y = "Fe2/Fe3")

    
  combined_key %>% 
    filter(analyte %in% c("ammonium_ppm", "nitrate_ppm")) %>% 
    ggplot(aes(x = location, y = value, color = timepoint))+
    geom_point(size = 3,
               position = position_dodge(width = 0.2))+
    facet_grid(analyte ~ treatment, scales = "free_y")
  
  nitrogen = 
      combined_key %>% 
      filter(analyte %in% c("ammonium_ppm", "nitrate_ppm")) %>% 
      pivot_wider(names_from = "analyte", values_from = "value") %>% 
      mutate(amm_nitr = ammonium_ppm/nitrate_ppm)
    
    
    nitrogen %>% 
      ggplot(aes(x = location, y = amm_nitr, color = timepoint))+
      geom_point(size = 3,
                 position = position_dodge(width = 0.2))+
      facet_wrap(~treatment)+
      labs(y = "ammonium/nitrate")

    

    
    
    orp_long = 
      orp %>% 
      dplyr::select(sample_label, orp_mV, pH, EC_mScm, DO_mgL) %>% 
      pivot_longer(-sample_label) %>% 
      left_join(sample_key) %>% 
      mutate(value = as.numeric(value)) %>% 
      reorder_factors() %>%  
      drop_na()
    
    orp_long %>% 
      ggplot(aes(x = location, y = value, color = timepoint))+
      geom_point(size = 3, stroke = 1,
                 position = position_dodge(width = 0.3))+
      scale_shape_manual(values = c(1, 16))+
      facet_grid(name~treatment, scales = "free_y")
    
    
    ghg_processed = 
      ghg %>% 
      filter(!is.na(treatment)) %>% 
      reorder_factors() 
    
    
    ghg_processed %>% 
      ggplot(aes(x = location, y = CO2_post/(CH4_post/1000), color = timepoint))+
      geom_point(size = 3, stroke = 1,
                 position = position_dodge(width = 0.3))+
      scale_shape_manual(values = c(1, 16))+
      facet_wrap(~treatment)+
      labs(y = "CO2 ppm / CH4 ppm")
    
    ghg_long = 
      ghg_processed %>% 
      dplyr::select(sample_label, CO2_post, CH4_post) %>% 
      pivot_longer(cols = -sample_label) %>% 
      left_join(sample_key) %>% 
      reorder_factors()
    
    ghg_long %>% 
      ggplot(aes(x = location, y = value, color = timepoint))+
      geom_point(size = 3, stroke = 1,
                 position = position_dodge(width = 0.3))+
      scale_shape_manual(values = c(1, 16))+
      facet_grid(name ~ treatment, scales = "free_y")
}