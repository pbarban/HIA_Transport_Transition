source("functions.R")

pacman::p_load(dplyr,
               tidyr)

# Denmark data
pop = pop_pyramid("Denmark", 2020) %>% 
  pivot_longer(!Age, names_to = "sexe", values_to = "pop")

# Travel Data
travel_rawdata = read.table(file = "distrib_Dan.txt", sep = ";", head=T, dec = "," ) %>% 
  filter(Period==2) %>% 
  filter(minRespAgeCorrect != 10) %>% # exclude 10-15y
  mutate(sexe = ifelse(RespSex == 1, "M", "F"),
         Age = age_grp(minRespAgeCorrect))

Denmark_data = travel_rawdata %>% 
  merge(pop, by = c("Age", "sexe")) %>% 
  mutate(cycling = BicLen_pers_day,
         walking = WalkLen_pers_day) %>% 
  select(Age, sexe, cycling,walking, pop) %>%
  group_by(Age) %>% 
  bind_rows(summarise(.,
                      across(pop, sum),
                      across(cycling:walking, mean),
                      across(where(is.character), ~"Both"))) %>% 
  pivot_longer(c(cycling,walking),names_to = "mode", values_to = "distance") %>% 
  mutate(sexe = case_when(sexe == "F" ~ "Women",
                          sexe == "M" ~ "Men",
                          TRUE ~ as.character(sexe) )) %>% 
  rename("age_grp" = "Age")

rm(list=ls()[! ls() %in% c("Denmark_data")])
