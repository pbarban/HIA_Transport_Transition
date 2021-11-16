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

all_data = travel_rawdata %>% 
  merge(pop, by = c("Age", "sexe")) %>% 
  mutate(Bike_dist = BicLen_pers_day,
         Walk_dist = WalkLen_pers_day) %>% 
  select(Age, sexe, Bike_dist,Walk_dist, pop) %>% 
  pivot_wider( names_from = sexe, values_from = c(pop, Bike_dist, Walk_dist)) %>% 
  rowwise() %>% 
  mutate(Bike_dist_Both = sum(across(starts_with("Bike"))),
         Walk_dist_Both = sum(across(starts_with("Walk"))),
         pop_Both = sum(across(starts_with("pop")))) %>% 
  pivot_longer(!Age) %>% 
 mutate( sexe = sub("[^.]*\\_", "", name),
         type = sub("_[^_]*$", "", name)) %>% 
  mutate(age_grp = Age) %>% 
  select(-c(name, Age)) %>% 
  pivot_wider(values_from = value, names_from = type) %>% 
  mutate(sexe = case_when(sexe == "F" ~ "Women",
                          sexe == "M" ~ "Men",
                          TRUE ~ as.character(sexe) ))
  
  
  
### To continue

dan = merge(x = dan16, y=pyr_piv, by=c("AgeGroup", "sex"))
dan = dan[order(dan$AgeGroup),]

dan_totF =sum(dan$pop[dan$sex=="F"]); dan_totF
dan_totM =sum(dan$pop[dan$sex=="M"]); dan_totM

danF = dan %>% filter(sex == "F"); dim(danF)
danM = dan %>% filter(sex == "M"); dim(danM)


#average men and women
dan_av = danF  # just copy the form
dan_av$WalkLen_pers_day = dan_av$BicLen_pers_day = dan_av$pop = 0  
dan_av$sex = "Both"
dan_av$pop = danF$pop + danM$pop
dan_av$WalkLen_pers_day = (danF$WalkLen_pers_day*danF$pop + danM$WalkLen_pers_day*danM$pop)/(danF$pop+danM$pop) #moyenne homme femme pondérée
dan_av$BicLen_pers_day = (danF$BicLen_pers_day*danF$pop + danM$BicLen_pers_day*danM$pop)/(danF$pop+danM$pop)

colnames(dan_av)
dan_av_both = dan_av %>% select(AgeGroup, WalkLen_pers_day, BicLen_pers_day, pop); dim(dan_av_both)

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
