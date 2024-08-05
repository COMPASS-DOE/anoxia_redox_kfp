library(tidyverse)
library(googlesheets4)
theme_set(theme_bw(base_size = 16))

# fibox_tests_gcw = read_sheet("https://docs.google.com/spreadsheets/d/1b5L4VcpZ4dVe86b9IUQRAJTTF_mlXO7LlkLenCOniUs/edit#gid=1122649204", 
#                          sheet = "combined") %>% filter(site == "GCW")
# 
# fibox_tests_owc = read_sheet("https://docs.google.com/spreadsheets/d/1b5L4VcpZ4dVe86b9IUQRAJTTF_mlXO7LlkLenCOniUs/edit#gid=1122649204", 
#                              sheet = "combined") %>% filter(site == "OWC")
# fibox_tests_owc2 = 
#   fibox_tests_owc %>% 
#   mutate_all(as.character) %>%
#   janitor::clean_names() %>% 
#   janitor::remove_empty("cols")
  

fibox_tests_dat = read_sheet("https://docs.google.com/spreadsheets/d/1b5L4VcpZ4dVe86b9IUQRAJTTF_mlXO7LlkLenCOniUs/edit#gid=1122649204", 
                         sheet = "combined")

fibox_tests = 
  fibox_tests_dat %>% 
  pivot_longer(cols = -c(elapsed_days, site), names_to = "treatment2", values_to = "do_mgL") %>% 
  drop_na() %>% 
  mutate(elapsed_days = as.numeric(elapsed_days),
         do_mgL = as.numeric(do_mgL),
         treatment = str_extract(treatment2, "water|salt|glucose|acetate|nitrate|sulfate")) %>% 
  arrange(site, treatment, elapsed_days)
  
  
fibox_tests %>% 
  filter(site == "OWC") %>% 
  ggplot(aes(x = elapsed_days, y = do_mgL, color = treatment))+
  geom_line(aes(group = treatment), linewidth = 1)+
  geom_point(size = 3)+
  facet_wrap(~site, scales = "free_x")+
  labs(x = "Elapsed Days",
       y = "Dissolved Oxygen, mg/L")

fibox_tests %>% 
  mutate(site = factor(site, levels = c("OWC", "GCW"))) %>% 
  filter(treatment %in% c("water", "glucose", "acetate")) %>% 
  ggplot(aes(x = elapsed_days, y = do_mgL, color = treatment))+
  geom_line(aes(group = treatment), linewidth = 1)+
  geom_point(size = 4)+
  scale_color_manual(values = rev(soilpalettes::soil_palette("redox2", 3)))+
  facet_wrap(~site, scales = "free_x")+
  labs(x = "Elapsed Days",
       y = "Dissolved Oxygen, mg/L")

fibox_tests %>% 
  filter(site == "GCW" & treatment %in% c("water", "sulfate", "nitrate", "salt")) %>% 
  ggplot(aes(x = elapsed_days, y = do_mgL, color = treatment))+
  geom_line(aes(group = treatment), linewidth = 1)+
  geom_point(size = 3)+
  facet_wrap(~site, scales = "free_x")
