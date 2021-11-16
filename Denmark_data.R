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

### now need to expand to have all age years
dan_av_year = data.frame(age = 15:84)
dan_av_year$group = rep(3:16, each = 5)
mm = match(dan_av_year$group, dan_av_both$AgeGroup)
dan_av_year$prop_walk = dan_av_both$WalkLen_pers_day[mm]
dan_av_year$rho_walk = dan_av_year$prop_walk/dan_av_year$prop_walk[1]

dan_av_year$prop_cycle = dan_av_both$BicLen_pers_day[mm]
dan_av_year$rho_cycle = dan_av_year$prop_cycle/ dan_av_year$prop_cycle[1]



### now need to expand to have all age years
vec_age = c(0:14, 85:99)
tmp = cbind(vec_age,rep(0, length(vec_age)),  rep(0, length(vec_age)), rep(0, length(vec_age)), rep(0, length(vec_age)), rep(0, length(vec_age)) )
colnames(tmp) = colnames(dan_av_year)
dan_av_year = rbind(dan_av_year, tmp)
rm(tmp)

dan_av_year = dan_av_year[order(dan_av_year$age),]
