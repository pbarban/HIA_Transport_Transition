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
den = den %>% 
  arrange(mode, age) %>% 
  rename(type = mode) 
den$type = gsub("cycling", "cycle", den$type)
den$type = gsub("walking", "walk", den$type)
#Pierre, n'hésite pas à déplacer et traduire en dplyr si tu souhaites




#############################################################################################
###############################
# set parameters
###############################
#############################################################################################
walk_Ref_volume <- 168
walk_speed <- 4.8
walk_RR = 0.89

cycle_Ref_volume <- 100
cycle_speed <- 14
cycle_RR = 0.90

# pour l'instant, on utilise les memes parametres VAE/vélo pour retrouver les résultats précédents
eCycle_Ref_volume <- 100
eCycle_speed <-18  # valeur de Bouscasse et al : 18
eCycle_RR = cycle_RR
METeCycle_ratio <- 4.5/5.8# valeur de Bouscasse et al : 4.5/5.8
eCycle_RR = 1-((1-cycle_RR)*METeCycle_ratio)




#############################################################################################
###############################
# prepare data
###############################
#############################################################################################

#### merge nw_data and INSEE_data
INSEE_data = INSEE_data %>% 
  filter(sexe == "Both") # Filter by both, to fix at one point

nw = select(INSEE_data, year, p_tot) %>% 
  right_join(nw_data, by = "year") %>% 
  rename(km_pp_y = value) %>% 
  mutate(total_km_y = p_tot*km_pp_y,
         speed = case_when(type == "walk" ~ walk_speed,
                           type == "cycle" ~ cycle_speed,
                           type == "e_cycle" ~ eCycle_speed),
         minute_pp_w = (60*km_pp_y /speed) / (365.25/7))  # weekly minutes



#############################################################################################
###############################
# prepare data
###############################
#############################################################################################
n_prev = function(data, RR, Ref_volume){
  res = (1-RR)*(data$minute_pp_w/Ref_volume)*data$MR*data$pop
  return(res)
}

# 
# df_demo = INSEE_data
# df_acti = nw
# target_distri = den
  
impact_per_type = function(df_demo, # demographic data frame
                           df_acti, # data frame of physical activity
                           target_distri, # data frame with the target age-distribution of physical activity
                           type_eval = "cycle", # has to be "walk", "cycle" or "e_cycle"
                           RR = cycle_RR, 
                           Ref_volume = cycle_Ref_volume,
                           speed = cycle_speed){
  
  acti =  df_acti %>% filter(type == type_eval)
  
  type_target = ifelse(type_eval == "e_cycle" , "cycle", type_eval)# if e_bike, use the target distrib of classic bike
  target = target_distri %>% filter(type == type_target)
  
  ####### 
  ##in S1, the scenario assessed, calculate the km_w per person
  S1tab = df_demo
  S1tab = S1tab %>% arrange(year)
  S1tab$rho = as.numeric(target$rho[match(S1tab$age, target$age)])
  S1tab$total_km_y = acti$total_km_y[match(S1tab$year, acti$year)]
  
  # creat sum(rho*pop) for each year
  tmp = S1tab %>% group_by(year) %>% 
    mutate(rho_pop = rho*pop,
           sum_rho_pop = sum(rho_pop))
  S1tab$sum_rho_pop = tmp$sum_rho_pop[match(S1tab$year, tmp$year)] ; rm(tmp)       
  S1tab$km_pp_y = S1tab$total_km_y*S1tab$rho/S1tab$sum_rho_pop
  
  S1tab$minute_pp_w =  (60*S1tab$km_pp_y /speed) / (365.25/7)
  
  ####### 
  # create the reference scenario = 2020 volumes all along
  S0tab = S1tab
  n_rep = nrow(S0tab) / nrow(S1tab[S1tab$year==2021,])
  S0tab$minute_pp_w = rep(S1tab$minute_pp_w[S1tab$year==2021], n_rep) 
  
  ### calculated number prevented
  S1tab$n_prev = n_prev(S1tab, RR=RR, Ref_volume=Ref_volume)
  S1tab$yll_prev = S1tab$n_prev*S1tab$yll
  
  S0tab$n_prev = n_prev(S0tab, RR=RR, Ref_volume=Ref_volume)
  S0tab$yll_prev = S0tab$n_prev*S1tab$yll
  
  S1tab$n_prev_wo_S0 = S1tab$n_prev - S0tab$n_prev 
  S1tab$yll_prev_wo_S0 = S1tab$yll_prev - S0tab$yll_prev 
  
  li = list(S1 = S1tab, S0 = S0tab)
  return(li)
}




##### test
res_walk = impact_per_type(df_demo = INSEE_data,
                            df_acti = nw,
                            target_distri = den,
                            type_eval = "walk", 
                            RR = walk_RR, 
                            Ref_volume = walk_Ref_volume,
                            speed = walk_speed)


res_cycle = impact_per_type(df_demo = INSEE_data,
                            df_acti = nw,
                            target_distri = den,
                            type_eval = "cycle", 
                            RR = cycle_RR, 
                            Ref_volume = cycle_Ref_volume,
                            speed = cycle_speed)

res_ecycle = impact_per_type(df_demo = INSEE_data,
                            df_acti = nw,
                            target_distri = den,
                            type_eval = "e_cycle", 
                            RR = eCycle_RR, 
                            Ref_volume = eCycle_Ref_volume,
                            speed = eCycle_speed)

tot_walk =sum(res_walk$S1$n_prev_wo_S0, na.rm = T); tot_walk
tot_cycle = sum(res_cycle$S1$n_prev_wo_S0, na.rm = T); tot_cycle
tot_ecycle = sum(res_ecycle$S1$n_prev_wo_S0, na.rm = T); tot_ecycle
tot_cycle_eCycle =  sum(res_cycle$S1$n_prev_wo_S0, na.rm = T) +
  sum(res_ecycle$S1$n_prev_wo_S0, na.rm = T) ; tot_cycle_eCycle
tot_nw = tot_walk + tot_cycle + tot_ecycle; tot_nw


########################################
##  graph evolution per age category  ##
########################################
res = res_cycle

evo_milage = function(res){ # res is a resultat table provided by the impact_per_type() function
  
  evo = data_frame(res$S1) %>% 
    mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
           age_grp = as.character(age_grp.FACTOR), 
           age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
           age_grp = gsub(",", "-", age_grp),
           post = sub(".*-","",age_grp),
           age_grp = sub("-.*", "", age_grp),
           age_grp = paste0(age_grp,"-", as.numeric(post)-1),
           order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
    group_by(age_grp, order, year) %>% 
    summarise(km_pp_y = mean(km_pp_y),
              minute_pp_w = mean(minute_pp_w))
  return(evo)
}
evo_test = evo_milage(res_cycle)
y_vec = c(2021, 2030, 2040, 2050)
evo_test_yy = evo_test[evo_test$year %in% y_vec, ]
max(evo_test_yy$minute_pp_w)
