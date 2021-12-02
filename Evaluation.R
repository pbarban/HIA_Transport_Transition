library(ggplot2)
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


tot_table = impact_all_types(df_demo = INSEE_data,
                             df_acti = nw,
                             target_distri = den)
sum(tot_table$tot_S1$n_prev_wo_S0) # we find the same results as when doing each type separately, good !


########################################
##  graph evolution per age category  ##
########################################
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




evo_cycle = evo_milage(res_cycle)
evo_ecycle = evo_milage(res_ecycle)
# pour représenter les évolutions, combiner vélo et VAE
evo_all_cycle = evo_cycle
evo_all_cycle$km_pp_y = evo_cycle$km_pp_y + evo_ecycle$km_pp_y
evo_all_cycle$minute_pp_w = evo_cycle$minute_pp_w + evo_ecycle$minute_pp_w
max(evo_all_cycle$minute_pp_w)

y_vec = c(2021, 2030, 2040, 2050)
evo_cycle_years = evo_all_cycle[evo_all_cycle$year %in% y_vec, ] %>% 
  filter (order>14 & order<85) %>% 
  ungroup()
max(evo_cycle_years$minute_pp_w)
evo_cycle_years$year = as.factor(evo_cycle_years$year)

# tracer histogram de l'évolution des temps parcourus
evo_cycle_years %>%  
  ggplot() + geom_bar(aes(age_grp,
                        y = minute_pp_w,
                        fill = year),
                    stat = "identity",
                    position = "dodge2", 
                    width = 0.7) +
  scale_y_continuous(name = "Weekly cycling duration (minutes)")+
  theme_minimal() +
  xlab("Age group") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 60, hjust=1))
ggsave("Evo_cycling_volumes.png", plot = last_plot())


evo_walk = evo_milage(res_walk)
evo_walk_years = evo_walk[evo_walk$year %in% y_vec, ] %>% 
  filter (order>14 & order<85) %>% 
  ungroup()
evo_walk_years$year = as.factor(evo_walk_years$year)


#same with walking
evo_walk_years %>%  
  ggplot() + geom_bar(aes(age_grp,
                          y = minute_pp_w,
                          fill = year),
                      stat = "identity",
                      position = "dodge2", 
                      width = 0.7) +
  scale_y_continuous(name = "Weekly walking duration (minutes)")+
  theme_minimal() +
  xlab("Age group") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 60, hjust=1))
ggsave("Evo_walking_volumes.png", plot = last_plot())