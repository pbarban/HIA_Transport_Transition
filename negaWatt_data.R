pacman::p_load(readr,
               dplyr,
               tidyr)

raw_data <- read_csv(file = "nw_data.csv",
                     col_names = c("type" ,paste0("year_",rep(2015:2050))))%>%
  mutate(across(everything(), as.character)) 


#########################
####     nw data     ####
#########################

# Data in km/hab/an
# by year divided by type: walk, cycle, and e_cycle

nw_data = raw_data %>% 
  slice(-1) %>% 
  pivot_longer(cols = !type,
               names_to = "year") %>% 
  na.omit() %>% 
  pivot_wider(names_from = "type", values_from = "value") %>% 
  mutate(walk = as.numeric(Marche),
         all_cycle = as.numeric(Velo),
         e_cycle = all_cycle * as.numeric(gsub("%", "", VAE))/100,
         cycle = all_cycle - e_cycle) %>% 
  select(-c(Marche, Velo, VAE, all_cycle)) %>% 
  pivot_longer(!year, names_to = "type", values_to = "value")%>% 
  mutate(year = as.numeric(sub("year_", "", year)),
          value = as.numeric(value)) %>% 
  filter(year %in% c(2020:2050))






