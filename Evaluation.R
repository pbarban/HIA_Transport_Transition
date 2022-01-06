library(ggplot2)
library(ggpubr)
library(forcats)
source("Denmark_data.R")
source("INSEE.R")
source("negaWatt_data.R")
source("functions.R")


###### upload monetarisation YLL
monetarisation = read.csv2("monetarization_yll.csv", dec=".")
plot(monetarisation$year, monetarisation$euro_yll, type = 'l')
# values are not the right one now, will update this


###### plot nW data
str(nw_data)
nw_data2 = nw_data
nw_data2$type <- factor(nw_data$type, levels=c("walk","tot_cycle","cycle", "e_cycle"), 
                        labels=c("Walk", "Total cycle","Bike", "E-bike" ))
#nw_data2 = nw_data2 %>%  filter(year>2021)
# labels=c("Marche", "Total vélo","vélo", "VAE" ))


p1 = nw_data2 %>% 
  ggplot() +  
  geom_line(aes(x = year, y = value/52.1, group = type, col = type, linetype=type, size= type))+
  ylab("km/inhab/week") +
  xlab("") +
  labs(title = "Trends in active transportation mileage, negaWatt scenario") +
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

cycle_RR_zhao = 0.91^(11.25/5)

# pour l'instant, on utilise les memes parametres VAE/vélo pour retrouver les résultats précédents
eCycle_Ref_volume <- 100
eCycle_speed <-18  # valeur de Bouscasse et al : 18
eCycle_RR = cycle_RR
METeCycle_ratio <- 4.5/5.8# valeur de Bouscasse et al : 4.5/5.8
eCycle_RR = 1-((1-cycle_RR)*METeCycle_ratio)

eCycle_RR_zhao = 1-((1-cycle_RR_zhao)*METeCycle_ratio)
# value Zhao



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
                               cycle_speed = 14,
                               eCycle_speed = 18,
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
                             scale_y_lab = "min/week")

p_evo_cycle = plot_evo_milage(evo = evo_cycle,y_vec = y_vec_3,
                              scale_y_lab = "min/week")
# scale_y_lab = "Weekly cycling duration (minutes)")

p_evo_ecycle = plot_evo_milage(evo = evo_Ecycle, y_vec = y_vec_3,
                               scale_y_lab = "min/week")
# scale_y_lab = "Weekly e-cycling duration (minutes)")
p_evo_totcycle = plot_evo_milage(evo = evo_tot_cycle, y_vec = y_vec_3,
                               scale_y_lab = "min/week")


p_evo_walk
p_evo_cycle
p_evo_ecycle
p_evo_totcycle

p2 = ggarrange(p_evo_walk, p_evo_cycle, p_evo_ecycle,p_evo_totcycle,
               labels = c("A: Walk", "B: Bike", "C: E-bike", "D: Total bike"),
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

METeCycle_ratio <- 4.5/5.8# valeur de Bouscasse et al : 4.5/5.8
eCycle_RR = 1-((1-0.90)*METeCycle_ratio)
eCycle_RR_low = 1-((1-RR_cycle_low)*METeCycle_ratio)
eCycle_RR_sup = 1-((1-RR_cycle_sup)*METeCycle_ratio)

impact = impact_all_types (df_demo= INSEE_data, # demographic data frame
                              df_acti= nw_data, # data frame of aggregated active transport volume
                              target_distri=den, # data frame with the target age-distribution of physical activity
                              walk_speed=4.8,
                              cycle_speed = 14,
                              eCycle_speed = 18,
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
                                  cycle_speed = 14,
                                  eCycle_speed = 18,
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
                                  cycle_speed = 14,
                                  eCycle_speed = 18,
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


deathplot = ggplot(data=res_per_year_group)+
  geom_bar(aes(x = year, y = death_prev),stat = "identity", fill="#a6cee3") +
  ylab("Deaths prevented")+
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
  ylab("Annual benefits (billions)")+
  xlab("Year")+
  geom_errorbar(aes(x = year,ymin = euro_low/1e9, ymax = euro_sup/1e9, width = 0.4)) +
  theme_minimal()
euros_plot

impact_plot = ggarrange (deathplot, yll_plot, euros_plot, ncol = 1) 
tiff("total impact per year.tiff", units="in", width = 5*1.4, height= 5.5*1.4, res=190)
plot(impact_plot)
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

tiff("deaths per age and year.tiff", units="in", width = 5*2, height= 2.5*2, res=190)
plot(plot_death_age)
dev.off()




##########################################
##################
# sensitivity analysis
RR_cycle = 0.90
RR_cycle_low = 0.94
RR_cycle_sup = 0.87
RR_walk = 0.89
RR_walk_low = 0.96
RR_walk_sup = 0.83

METeCycle_ratio <- 4.5/5.8# valeur de Bouscasse et al : 4.5/5.8
eCycle_RR = 1-((1-0.90)*METeCycle_ratio)
eCycle_RR_low = 1-((1-RR_cycle_low)*METeCycle_ratio)
eCycle_RR_sup = 1-((1-RR_cycle_sup)*METeCycle_ratio)

#
impact = impact_all_types (df_demo= INSEE_data, # demographic data frame
                           df_acti= nw_data, # data frame of aggregated active transport volume
                           target_distri=den, # data frame with the target age-distribution of physical activity
                           walk_speed=4.8,
                           cycle_speed = 14,
                           eCycle_speed = 18,
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
METeCycle_ratio_Eg <- 0.9# valeur de Egiguren et al 
eCycle_RR_Eg = 1-((1-RR_cycle)*METeCycle_ratio_Eg)
eCycle_RR_low_Eg = 1-((1-RR_cycle_low)*METeCycle_ratio_Eg)
eCycle_RR_sup_Eg = 1-((1-RR_cycle_sup)*METeCycle_ratio_Eg)
agg_impact_IC(obj_delta = 6.7,
              eCycle_RR = eCycle_RR_Eg, 
              eCycle_RR_low = eCycle_RR_low_Eg,
              eCycle_RR_sup = eCycle_RR_sup_Eg)

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
