# ANOXIA AND REDOX - 2023

## 2-functions_analysis.R
## Use this script for data analysis and visualization.

## KFP, October 2023

######################## ####
######################## ####

make_chemistry_graphs = function(combined_data, sample_key){
  
  combined_key = 
    combined_data %>% 
    left_join(sample_key)
  
  iron = 
    combined_key %>% 
    filter(analysis == "iron") %>% 
    pivot_wider(names_from = "analyte", values_from = "value") %>% 
    mutate(Fe2_3 = Fe2_ppm/Fe3_ppm)
  
#  gg_iron = 
    iron %>% 
    ggplot(aes(x = location, y = Fe2_3, color = timepoint))+
    geom_point(position = position_dodge(width = 0.2))+
      facet_wrap(~treatment)
    
    
    
    nitrogen = 
      combined_key %>% 
      filter(analyte %in% c("ammonium_ppm", "nitrate_ppm")) %>% 
      pivot_wider(names_from = "analyte", values_from = "value") %>% 
      mutate(amm_nitr = ammonium_ppm/nitrate_ppm)
    
    
    nitrogen %>% 
      ggplot(aes(x = location, y = amm_nitr, color = timepoint))+
      geom_point(position = position_dodge(width = 0.2))+
      facet_wrap(~treatment)

    
    combined_key %>% 
      ggplot(aes(x = location, y = value, color = timepoint, shape = treatment))+
      geom_point(size = 2, position = position_dodge(width = 0.2))+
      scale_shape_manual(values = c(1, 16))+
      facet_wrap(analyte ~ treatment, scales = "free_y")
    
    
    
    
    
    }