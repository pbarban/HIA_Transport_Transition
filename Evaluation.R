library(ggplot2)
library(ggpubr)
library(forcats)
source("Denmark_data.R")
source("INSEE.R")
source("negaWatt_data.R")
source("functions.R")


#############################################################################################
###############################
# set baseline parameters
###############################
#############################################################################################
walk_Ref_volume <- 168
walk_speed <- 4.8
walk_RR = 0.89

cycle_Ref_volume <- 100
cycle_speed <- 14.9 # valeur de Egiguren
cycle_RR = 0.90

cycle_RR_zhao = 0.91^(11.25/5)

# pour l'instant, on utilise les memes parametres VAE/vélo pour retrouver les résultats précédents
eCycle_Ref_volume <- 100
eCycle_speed <-18.1  # valeur de Bouscasse et al : 18
eCycle_RR = cycle_RR
METeCycle_ratio <- 0.9 # valeur de Egiguren et al : 4.5/5.8
eCycle_RR = 1-((1-cycle_RR)*METeCycle_ratio)

eCycle_RR_zhao = 1-((1-cycle_RR_zhao)*METeCycle_ratio)
# value Zhao




###### upload monetarisation YLL
monetarisation = read.csv2("monetarization_yll.csv", dec=".")
plot(monetarisation$year, monetarisation$euro_yll, type = 'l')
# values are not the right one now, will update this



###### plot nW data
prop.eBike_year = data.frame(year = unique(nw_data$year), prop = NA)
for (i in 1: length(unique(nw_data$year))){
  yy = unique(nw_data$year)[i]
  prop.eBike_year$prop[i]= nw_data[nw_data$type=="e_cycle" & nw_data$year == yy, "value"] / 
    (nw_data[nw_data$type=="tot_cycle" & nw_data$year == yy, "value"])
}
prop.eBike_year$prop = as.numeric(prop.eBike_year$prop)
prop.eBike_year =prop.eBike_year %>% 
  mutate(average_speed = cycle_speed*(1-prop) + eCycle_speed*prop)


str(nw_data)
nw_data2 = nw_data
nw_data2$type <- factor(nw_data$type, levels=c("walk","tot_cycle","cycle", "e_cycle"), 
                        labels=c("Walk", "Total cycle","Bike", "E-bike" ))
#nw_data2 = nw_data2 %>%  filter(year>2021)
# labels=c("Marche", "Total vélo","vélo", "VAE" ))
nw_data2 = nw_data2 %>% 
  mutate(speed = case_when(type == "Walk" ~ walk_speed,
                    type == "Bike" ~ cycle_speed,
                    type == "E-bike" ~ eCycle_speed))
nw_data2$speed[is.na(nw_data2$speed)] =   prop.eBike_year$average_speed
nw_data2 = nw_data2 %>% 
  mutate(min_pp_w = (60*value/speed) /(365.25/7),
         km_pp_w = value/(365.25/7))  # weekly minutes

p1 = nw_data2 %>% 
  ggplot() +  
  geom_line(aes(x = year, y = value/(365.25/7), group = type, col = type, linetype=type, size= type))+
  ylab("km/inhab/week") +
  xlab("") +
 # labs(title = "Trends in active transportation mileage, negaWatt scenario") +
 # labs(title = "Tendances dans les transports actifs, scénario negaWatt", subtitle = "En km/semaine/hab") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(colour = "#595a5c", size = 12),
        legend.position="top",
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=12),
        axis.text=element_text(size=12),
        axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))+
  scale_linetype_manual(values=c("solid", "solid","dashed", "dashed"))+
  scale_color_manual(values=c("#1f78b4", "#b2df8a", "#33a02c", "#fb9a99")) +
  scale_size_manual(values = c(1.5,1.5, 1, 1))
  
plot(p1)

tiff("Trends_mileage-SnW.tiff", units="in", width = 9*1.4, height= 5*1.4, res=190)
plot(p1)
dev.off()

p2 = nw_data2 %>% 
  ggplot() +  
  geom_line(aes(x = year, y = min_pp_w, group = type, col = type, linetype=type, size= type))+
  ylab("min/inhab/week") +
  xlab("") +
  #labs(title = "Trends in active transportation duration, negaWatt scenario") +
  # labs(title = "Tendances dans les transports actifs, scénario negaWatt", subtitle = "En km/semaine/hab") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(colour = "#595a5c", size = 12),
        legend.position="top",
        legend.key.size = unit(1, 'cm'),
        legend.text = element_text(size=12),
        axis.text=element_text(size=12),
        axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))+
  scale_linetype_manual(values=c("solid", "solid","dashed", "dashed"))+
  scale_color_manual(values=c("#1f78b4", "#b2df8a", "#33a02c", "#fb9a99")) +
  scale_size_manual(values = c(1.5,1.5, 1, 1))

plot(p2)

P1et2 = ggarrange(p1, p2, ncol = 2)
tiff("evolution in km and min.tiff", units="in", width = 5*2.2, height= 2.5*2.2, res=190)
plot(P1et2)
dev.off()



##########################################################################################
######################################################
#### #### #### #### #### #### #### #### #### #### 
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


<<<<<<< HEAD
##### test
res_walk = impact_per_type(df_demo = INSEE_data,
                            df_acti = nw,
                            target_distri = den,
                            type_eval = "walk", 
                            RR = walk_RR, 
                            Ref_volume = walk_Ref_volume,
                            speed = walk_speed)

<<<<<<< HEAD
##### test
=======

>>>>>>> 6019b4baa764d8fbbef1de4b7b1fb4b6ace2f943
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



###############
### calculate gain in life expectancy

life.expectancy(tot_table, 2021)
life.expectancy(tot_table, 2022)
life.expectancy(tot_table, 2030)
life.expectancy(tot_table, 2040)
life.expectancy(tot_table, 2050)

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
=======
>>>>>>> e602f1d854c308bad95749dbef8a5d639c799a0c



##########################################################################################
######################################################
####################################
################## 
# HERE use the v2 functions : just check consistency with v1 functions first

##################
impact = impact_all_types (df_demo= INSEE_data, # demographic data frame
                               df_acti= nw_data, # data frame of aggregated active transport volume
                               target_distri=den, # data frame with the target age-distribution of physical activity
                               walk_speed=4.8,
                               cycle_speed = cycle_speed,
                               eCycle_speed = eCycle_speed,
                              obj_delta = 6.7, #targeted age diff btw classical and eBike users
                              #obj_delta = 0,
                               coef_delta = 1, #coef to give the relative importance of criteria delta
                               coef_rho=5,
                               walk_RR = 0.89,
                               walk_Ref_volume= 168,
                               cycle_RR = 0.90, 
                               cycle_Ref_volume = 100,
                              #eCycle_RR = 0.90,
                              eCycle_RR= eCycle_RR,
                               eCycle_Ref_volume =100,
                               age_min = 20, # minimal age to consider health benefits
                               age_max = 84)
sum(impact$impact_tot_S1$n_prev_wo_S0_tot)
res = impact


impact_per_type = res$S1 %>% 
  group_by(year, type) %>% 
  summarise(n_tot = sum(n_prev_wo_S0),
            yll_tot = sum(yll_prev_wo_S0 ))

tot_25 = sum(impact_per_type$n_tot[impact_per_type$year==2025]); tot_25
perc_walk = impact_per_type$n_tot[impact_per_type$year==2025 & impact_per_type$type == "Walk"] / tot_25
perc_bike = impact_per_type$n_tot[impact_per_type$year==2025 & impact_per_type$type == "Bike"]/ tot_25
perc_ebike = impact_per_type$n_tot[impact_per_type$year==2025 & impact_per_type$type == "E-bike"]/ tot_25

tot_35 = sum(impact_per_type$n_tot[impact_per_type$year==2035]); tot_35
perc_walk = impact_per_type$n_tot[impact_per_type$year==2035 & impact_per_type$type == "Walk"] / tot_35
perc_bike = impact_per_type$n_tot[impact_per_type$year==2035 & impact_per_type$type == "Bike"]/ tot_35
perc_ebike = impact_per_type$n_tot[impact_per_type$year==2035 & impact_per_type$type == "E-bike"]/ tot_35

tot_45 = sum(impact_per_type$n_tot[impact_per_type$year==2045]); tot_45
perc_walk = impact_per_type$n_tot[impact_per_type$year==2045 & impact_per_type$type == "Walk"] / tot_45
perc_bike = impact_per_type$n_tot[impact_per_type$year==2045 & impact_per_type$type == "Bike"]/ tot_45
perc_ebike = impact_per_type$n_tot[impact_per_type$year==2045 & impact_per_type$type == "E-bike"]/ tot_45


tot_overall = sum(impact_per_type$n_tot)
perc_overall_walk = sum(impact_per_type$n_tot[impact_per_type$type == "Walk"])/tot_overall;perc_overall_walk
perc_overall_bike = sum(impact_per_type$n_tot[impact_per_type$type == "Bike"])/tot_overall;perc_overall_bike
perc_overall_Ebike = sum(impact_per_type$n_tot[impact_per_type$type == "E-bike"])/tot_overall;perc_overall_Ebike

yll_overall = sum(impact_per_type$yll_tot)
perc_overall_walk_yll = sum(impact_per_type$yll_tot[impact_per_type$type == "Walk"])/yll_overall;perc_overall_walk_yll
perc_overall_bike_yll = sum(impact_per_type$yll_tot[impact_per_type$type == "Bike"])/yll_overall;perc_overall_bike_yll
perc_overall_Ebike_yll = sum(impact_per_type$yll_tot[impact_per_type$type == "E-bike"])/yll_overall;perc_overall_Ebike_yll


deathprer_type = ggplot(data=impact_per_type)+
  geom_bar(aes(x = year, y = n_tot, fill = type),stat = "identity") +
  ylab("Deaths prevented")+
  xlab("")+
  theme_minimal()
plot(deathprer_type)
yllprer_type = ggplot(data=impact_per_type)+
  geom_bar(aes(x = year, y = yll_tot, fill = type),stat = "identity") +
  ylab("YLL prevented")+
  xlab("")+
  theme_minimal()
plot(yllprer_type)

p_impact_type = ggarrange(deathprer_type, yllprer_type,ncol = 1)
tiff("impact per type.tiff", units="in",width = 5*1.4, height= 5.5*1.4, res=190)
plot(p_impact_type)
dev.off()






#########################################
####### plot the evolution of mileage
#########################################
y_vec_3 = c(2025, 2035, 2045)

res_evo = evo_milage(res)
evo_walk = res_evo %>% filter(type =="Walk")
evo_cycle = res_evo %>% filter(type =="Bike")
evo_Ecycle = res_evo %>% filter(type =="E-bike")

# create evo_tot_cycle
evo_tot_cycle = evo_cycle %>% 
  mutate(type = "Total cycle")
evo_tot_cycle$km_pp_y = evo_cycle$km_pp_y + evo_Ecycle$km_pp_y
evo_tot_cycle$minute_pp_w = evo_cycle$minute_pp_w + evo_Ecycle$minute_pp_w

#### plot evolutions
p_evo_walk = plot_evo_milage(evo = evo_walk,y_vec = y_vec_3,
                             scale_y_lab = "min/week/inh")

p_evo_cycle = plot_evo_milage(evo = evo_cycle,y_vec = y_vec_3,
                              scale_y_lab = "min/week/inh")
# scale_y_lab = "Weekly cycling duration (minutes)")

p_evo_ecycle = plot_evo_milage(evo = evo_Ecycle, y_vec = y_vec_3,
                               scale_y_lab = "min/week/inh")
# scale_y_lab = "Weekly e-cycling duration (minutes)")
p_evo_totcycle = plot_evo_milage(evo = evo_tot_cycle, y_vec = y_vec_3,
                               scale_y_lab = "min/week/inh")


p_evo_walk
p_evo_totcycle
p_evo_cycle
p_evo_ecycle


p2 = ggarrange(p_evo_walk, p_evo_totcycle, p_evo_cycle, p_evo_ecycle,
               labels = c("A: Walk", "B: Total bike", "C: Bike", "D: E-bike"),
               label.x = 0.74,
               ncol = 1)

tiff("evolution volumes per age.tiff", units="in", width = 5*1.4, height= 8*1.4, res=190)
plot(p2)
dev.off()


##############################
#### plot updtaed impact by year
##############################
RR_walk = 0.89
RR_cycle = 0.90
RR_cycle_low = 0.94
RR_cycle_sup = 0.87
RR_walk_low = 0.96
RR_walk_sup = 0.83

METeCycle_ratio <- 0.9# valeur de Bouscasse et al : 4.5/5.8
eCycle_RR = 1-((1-RR_cycle)*METeCycle_ratio)
eCycle_RR_low = 1-((1-RR_cycle_low)*METeCycle_ratio)
eCycle_RR_sup = 1-((1-RR_cycle_sup)*METeCycle_ratio)

impact = impact_all_types (df_demo= INSEE_data, # demographic data frame
                              df_acti= nw_data, # data frame of aggregated active transport volume
                              target_distri=den, # data frame with the target age-distribution of physical activity
                              walk_speed=4.8,
                              cycle_speed = cycle_speed,
                              eCycle_speed = eCycle_speed,
                              obj_delta = 6.7, #targeted age diff btw classical and eBike users
                              #obj_delta = 0,
                              coef_delta = 1, #coef to give the relative importance of criteria delta
                              coef_rho=5,
                              walk_RR = 0.89,
                              walk_Ref_volume= 168,
                              cycle_RR = 0.90,
                           #cycle_RR = cycle_RR_zhao,
                              cycle_Ref_volume = 100,
                              #eCycle_RR = 0.90,
                              eCycle_RR= eCycle_RR,
                           # eCycle_RR= eCycle_RR_zhao,
                              eCycle_Ref_volume =100,
                              age_min = 20, # minimal age to consider health benefits
                              age_max = 84)

plot(impact$life_exp$year, impact$life_exp$gain, type = "line")

impact_low = impact_all_types (df_demo= INSEE_data, # demographic data frame
                                  df_acti= nw_data, # data frame of aggregated active transport volume
                                  target_distri=den, # data frame with the target age-distribution of physical activity
                                  walk_speed=4.8,
                                  cycle_speed = cycle_speed,
                                  eCycle_speed = eCycle_speed,
                                  obj_delta = 6.7, #targeted age diff btw classical and eBike users
                                  #obj_delta = 0,
                                  coef_delta = 1, #coef to give the relative importance of criteria delta
                                  coef_rho=5,
                                  walk_RR = RR_walk_low,
                                  walk_Ref_volume= 168,
                                  cycle_RR = RR_cycle_low, 
                                  cycle_Ref_volume = 100,
                                  #eCycle_RR = 0.90,
                                  eCycle_RR= eCycle_RR_low,
                                  eCycle_Ref_volume =100,
                                  age_min = 20, # minimal age to consider health benefits
                                  age_max = 84)

impact_sup = impact_all_types (df_demo= INSEE_data, # demographic data frame
                                  df_acti= nw_data, # data frame of aggregated active transport volume
                                  target_distri=den, # data frame with the target age-distribution of physical activity
                                  walk_speed=4.8,
                                  cycle_speed = cycle_speed,
                                  eCycle_speed = eCycle_speed,
                                  obj_delta = 6.7, #targeted age diff btw classical and eBike users
                                  #obj_delta = 0,
                                  coef_delta = 1, #coef to give the relative importance of criteria delta
                                  coef_rho=5,
                                  walk_RR = RR_walk_sup,
                                  walk_Ref_volume= 168,
                                  cycle_RR = RR_cycle_sup, 
                                  cycle_Ref_volume = 100,
                                  #eCycle_RR = 0.90,
                                  eCycle_RR= eCycle_RR_sup,
                                  eCycle_Ref_volume =100,
                                  age_min = 20, # minimal age to consider health benefits
                                  age_max = 84)


res_per_year =cbind(age = impact$impact_tot_S1$age,
                    year = impact$impact_tot_S1$year,
                    death_prev = impact$impact_tot_S1$n_prev_wo_S0_tot,
                    death_prev_low = impact_low$impact_tot_S1$n_prev_wo_S0_tot,
                    death_prev_sup = impact_sup$impact_tot_S1$n_prev_wo_S0_tot,
                    yll = impact$impact_tot_S1$yll_prev_wo_S0_tot,
                    yll_low = impact_low$impact_tot_S1$yll_prev_wo_S0_tot,
                    yll_sup = impact_sup$impact_tot_S1$yll_prev_wo_S0_tot)
res_per_year_group = as.data.frame(res_per_year) %>% 
  group_by(year) %>% 
  summarise(death_prev = sum(death_prev),
            death_prev_low = sum(death_prev_low),
            death_prev_sup =sum(death_prev_sup),
            yll = sum(yll),
            yll_low = sum(yll_low),
            yll_sup = sum(yll_sup)) %>% 
  left_join(monetarisation, by = "year")  %>% 
  mutate(euro = euro_yll*yll,
         euro_low = euro_yll*yll_low,
         euro_sup = euro_yll*yll_sup)

sum(res_per_year_group$yll)
sum(res_per_year_group$yll_low)
sum(res_per_year_group$yll_sup)


deathplot = ggplot(data=res_per_year_group)+
  geom_bar(aes(x = year, y = death_prev),stat = "identity", fill="#a6cee3") +
  ylab("Premature deaths prevented")+
  xlab("")+
  geom_errorbar(aes(x = year,ymin = death_prev_low, ymax = death_prev_sup, width = 0.4)) +
  theme_minimal()
deathplot

yll_plot = ggplot(data=res_per_year_group)+
  geom_bar(aes(x = year, y = yll/1000),stat = "identity", fill="#1f78b4") +
  ylab("YLL prevented (thousands)")+
  xlab("Year")+
  geom_errorbar(aes(x = year,ymin = yll_low/1000, ymax = yll_sup/1000, width = 0.4)) +
  theme_minimal()
yll_plot

euros_plot =  ggplot(data=res_per_year_group)+
  geom_bar(aes(x = year, y = euro/1e9),stat = "identity", fill="#b2df8a") +
  ylab("Health benefits (billion €)")+
  xlab("Year")+
  geom_errorbar(aes(x = year,ymin = euro_low/1e9, ymax = euro_sup/1e9, width = 0.4)) +
  theme_minimal()
euros_plot

impact_plot = ggarrange (deathplot, yll_plot, euros_plot, ncol = 1) 
tiff("total impact per year.tiff", units="in", width = 5*1.4, height= 5.5*1.4, res=190)
plot(impact_plot)
dev.off()


#########################################
####### plot gain in life_exp
#########################################
impact_life_exp = cbind(impact$life_exp, gain_low = impact_low$life_exp$gain, gain_sup = impact_sup$life_exp$gain )

plot_life_exp = ggplot(data=impact_life_exp)+
  geom_line(aes(x=year, y = gain*12), col= "red")+
  geom_ribbon(aes(x= year, ymin = gain_low*12, ymax = gain_sup*12), alpha = 0.1)+
  ylab("Gain in life expectancy (months)")+
  xlab("Year")
plot(plot_life_exp)

tiff("life expectancy plot.tiff", units="in", width = 5*1.4, height= 3.2*1.4, res=190)
plot(plot_life_exp)
dev.off()

##############################
#### plot death prevented by year and age
##############################

res_per_year = as.data.frame(res_per_year)

evo_res_per_year = res_per_year %>% 
  mutate(age_grp.FACTOR = cut(age, breaks = seq(0,150, by = 5), include.lowest = T, right = F) , #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(death_prev = sum(death_prev),
            death_prev_low = sum(death_prev_low),
            death_prev_sup =sum(death_prev_sup),
            yll = sum(yll),
            yll_low = sum(yll_low),
            yll_sup = sum(yll_sup))



y_vec_3 = c(2025, 2035, 2045)
age_low = 14
age_sup = 84
scale_y_lab = "death prevented"
evo = evo_res_per_year[evo_res_per_year$year %in% y_vec_3, ] %>% 
  filter (order>age_low & order<=age_sup) %>% 
  ungroup()
evo$year = as.factor(evo$year)



plot_death_age = ggplot(data = evo, 
                        aes(x=age_grp, y = death_prev, fill = year, ymin = death_prev_low, ymax = death_prev_sup)) +
  geom_bar(position = position_dodge(), stat = "identity", width=0.7) + 
  geom_errorbar( position = position_dodge(width = 0.7), colour="black", width=0.4)+
  scale_y_continuous(name = "Deaths prevented")  +
  theme_minimal() +
  xlab("") +
  ylab("Deaths prevented") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 60, hjust=1))

plot_yll_age = ggplot(data = evo, 
                        aes(x=age_grp, y = yll, fill = year, ymin = yll_low, ymax = yll_sup)) +
  geom_bar(position = position_dodge(), stat = "identity", width=0.7) + 
  geom_errorbar( position = position_dodge(width = 0.7), colour="black", width=0.4)+
  scale_y_continuous(name = "YLL prevented")  +
  theme_minimal() +
  xlab("") +
  ylab("YLL prevented") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 60, hjust=1))


impact_per_age_plot = ggarrange (plot_death_age, plot_yll_age, ncol = 1) 
tiff("total impact per age and year.tiff", units="in", width = 5*1.4, height= 6.5*1.4, res=190)
plot(impact_per_age_plot)
dev.off()


tiff("deaths and yll per age and year.tiff", units="in", width = 5*2, height= 2.5*2, res=190)


dev.off()

plot(plot_yll_age)



##########################################
##################
# sensitivity analysis
RR_cycle = 0.90
RR_cycle_low = 0.94
RR_cycle_sup = 0.87
RR_walk = 0.89
RR_walk_low = 0.96
RR_walk_sup = 0.83

METeCycle_ratio <- 0.9 # valeur de Bouscasse et al : 4.5/5.8
eCycle_RR = 1-((1-RR_cycle)*METeCycle_ratio)
eCycle_RR_low = 1-((1-RR_cycle_low)*METeCycle_ratio)
eCycle_RR_sup = 1-((1-RR_cycle_sup)*METeCycle_ratio)

#
impact = impact_all_types (df_demo= INSEE_data, # demographic data frame
                           df_acti= nw_data, # data frame of aggregated active transport volume
                           target_distri=den, # data frame with the target age-distribution of physical activity
                           walk_speed=walk_speed,
                           cycle_speed = cycle_speed,
                           eCycle_speed = eCycle_speed,
                           obj_delta = 6.7, #targeted age diff btw classical and eBike users
                           #obj_delta = 0,
                           coef_delta = 1, #coef to give the relative importance of criteria delta
                           coef_rho=5,
                           walk_RR = 0.89,
                           walk_Ref_volume= 168,
                           cycle_RR = 0.90, 
                           cycle_Ref_volume = 100,
                           #eCycle_RR = 0.90,
                           eCycle_RR= eCycle_RR,
                           eCycle_Ref_volume =100,
                           age_min = 20, # minimal age to consider health benefits
                           age_max = 84)

# main analysis
agg_impact_IC()

# no_diff in age of bike vs E-bike users
agg_impact_IC(obj_delta = 0)

# MET ratio ebike/bike same as Egiguren
METeCycle_ratio_Bou <- 0.78# valeur de Bouscasse et al 
eCycle_RR_Bou = 1-((1-RR_cycle)*METeCycle_ratio_Bou)
eCycle_RR_low_Bou = 1-((1-RR_cycle_low)*METeCycle_ratio_Bou)
eCycle_RR_sup_Bou = 1-((1-RR_cycle_sup)*METeCycle_ratio_Bou)
agg_impact_IC(obj_delta = 6.7,
              eCycle_RR = eCycle_RR_Bou, 
              eCycle_RR_low = eCycle_RR_low_Bou,
              eCycle_RR_sup = eCycle_RR_sup_Bou)

# age max = 74
agg_impact_IC(age_max = 74)

# avalues of RR_cycle from Zhao 2021 : for 5 MET, RR= 0.91 [0.86-0.96]
ratio_zhao = 11.25/5 # Zhao RR is for 5 MET, the ref volume we consider represents 11.25 MET
RR_cycle_zhao = 0.91^ratio_zhao
RR_cycle_low_zhao =0.96^ratio_zhao
RR_cycle_sup_zhao =0.86^ratio_zhao
eCycle_RR_zhao = 1-((1-RR_cycle_zhao)*METeCycle_ratio)
eCycle_RR_low_zhao = 1-((1-RR_cycle_low_zhao)*METeCycle_ratio)
eCycle_RR_sup_zhao = 1-((1-RR_cycle_sup_zhao)*METeCycle_ratio)
agg_impact_IC(cycle_RR =RR_cycle_zhao,
              cycle_RR_low =RR_cycle_low_zhao,
              cycle_RR_sup =RR_cycle_sup_zhao,
              eCycle_RR = eCycle_RR_zhao, 
              eCycle_RR_low = eCycle_RR_low_zhao,
              eCycle_RR_sup = eCycle_RR_sup_zhao)



agg_impact_IC(cycle_speed = 15)
