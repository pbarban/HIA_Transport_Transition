# Check if the packages that we need are installed
pacman::p_load(ggplot2) 



source("Denmark_data.R")
source("INSEE.R")
source("negaWatt_data.R")


#### First add lines corresponding to 0:14 and 85:100 in Denmark data
dim(Denmark_data)
den = Denmark_data %>% filter(sexe == "Both") ; dim(den)
colnames(den)

vec_age = rep(c(0:14, 85:100), 2)
tmp = cbind(vec_age, rep(0, length(vec_age)),  rep(0, length(vec_age)), 
            c(rep("cycling", length(vec_age)/2), rep("walking", length(vec_age)/2)), 
              rep(0, length(vec_age)), rep(0, length(vec_age)) ) 
colnames(tmp) = colnames(den)
den  = rbind(den , tmp)
rm(tmp)
dim(den)
den$age = as.numeric(den$age)
den = den %>% arrange(mode, age)
#Pierre, n'hésite pas à déplacer et traduire en dplyr si tu souhaites




#############################################################################################
###############################
# set parameters
###############################
#############################################################################################
walk_Ref_volume <- 168
walk_speed <- 4.8
RR_walk = 0.89

cycle_Ref_volume <- 100
cycle_speed <- 14
RR_cycle = 0.90

# pour l'instant, on utilise les memes parametres VAE/vélo pour retrouver les résultats précédents
vae_speed <-cycle_speed  # valeur de Bouscasse et al : 18
MET_ratio <- 1# valeur de Bouscasse et al : 4.5/5.8

#############################################################################################
#############################################################################################
#############################################################################################

#### merge nw_data and INSEE_data
INSEE_data$year = as.numeric(INSEE_data$year)
nw = select(INSEE_data, year, p_tot) %>% 
  right_join(nw_data, by = "year") %>% 
  rename(km_pp_y = value) %>% 
  mutate(total_km_y = p_tot*km_pp_y,
         speed = rep(c(walk_speed, cycle_speed, vae_speed), length.out = nrow(nw)),
         min_pp_w = (60*km_pp_y /speed) / (365.25/7))  # weekly minutes

