#read functions
source("functions.R")

#packages
pacman::p_load(dplyr,
               tidyr,
               imputeTS)

#data cleaning
Pop.proj = download_INSEE("SP19") 

Mortality.rate = download_INSEE("SD01") 

All_data = Pop.proj %>% 
  merge(Mortality.rate, by = c("age", "sexe", "year"), all = T) %>% 
  mutate(pop = ifelse(is.na(pop), 0, pop)) %>%
  group_by(sexe, age) %>%
  mutate(deaths = na_kalman(deaths, model = "auto.arima")) %>%  # Estimate nbr of deaths in 2050
  mutate(MR = deaths/pop)

INSEE_data = All_data %>% 
  ungroup() %>% 
  mutate(age_grp = age_grp(age),
         MR = ifelse(MR > 1 | pop == 0, 1, MR)) # Some MR are >1, not exactly sure why...

rm(Pop.proj, Mortality.rate, All_data)

