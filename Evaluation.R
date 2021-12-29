library(ggplot2)
library(ggpubr)
source("Denmark_data.R")
source("INSEE.R")
source("negaWatt_data.R")
source("functions.R")


###### upload monetarisation YLL
monetarisation = read.csv2("monetarization_yll.csv")
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
                            speed = walk_speed,
                           age_min = 20, # minimal age to consider health benefits
                           age_max = 84)


res_cycle = impact_per_type(df_demo = INSEE_data,
                            df_acti = nw,
                            target_distri = den,
                            type_eval = "cycle", 
                            RR = cycle_RR, 
                            Ref_volume = cycle_Ref_volume,
                            speed = cycle_speed,
                            age_min = 20, # minimal age to consider health benefits
                            age_max = 84)

res_ecycle = impact_per_type(df_demo = INSEE_data,
                            df_acti = nw,
                            target_distri = den,
                            type_eval = "e_cycle", 
                            RR = eCycle_RR, 
                            Ref_volume = eCycle_Ref_volume,
                            speed = eCycle_speed,
                            age_min = 20, # minimal age to consider health benefits
                            age_max = 84)

tot_walk =sum(res_walk$S1$n_prev_wo_S0, na.rm = T); tot_walk
tot_cycle = sum(res_cycle$S1$n_prev_wo_S0, na.rm = T); tot_cycle
tot_ecycle = sum(res_ecycle$S1$n_prev_wo_S0, na.rm = T); tot_ecycle
tot_cycle_eCycle =  sum(res_cycle$S1$n_prev_wo_S0, na.rm = T) +
  sum(res_ecycle$S1$n_prev_wo_S0, na.rm = T) ; tot_cycle_eCycle
tot_nw = tot_walk + tot_cycle + tot_ecycle; tot_nw


tot_table = impact_all_types(df_demo = INSEE_data,
                             df_acti = nw,
                             target_distri = den,
                             age_min = 20, # minimal age to consider health benefits
                             age_max = 84)
sum(tot_table$tot_S1$n_prev_wo_S0) # we find the same results as when doing each type separately, good !



###############
### calculate gain in life expectancy

life.expectancy(tot_table, 2021)
life.expectancy(tot_table, 2022)
life.expectancy(tot_table, 2030)
life.expectancy(tot_table, 2040)
life.expectancy(tot_table, 2045)
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


plot_evo_milage = function(evo, y_vec = c(2021, 2030, 2040, 2050), age_low = 14, age_sup=84,
                           scale_y_lab){
  #evo is an output of the evo_milage() function
  evo = evo[evo$year %in% y_vec, ] %>% 
    filter (order>age_low & order<=age_sup) %>% 
    ungroup()
  evo$year = as.factor(evo$year)
   pplot = evo %>%  
    ggplot() + geom_bar(aes(age_grp,
                            y = minute_pp_w,
                            fill = year),
                        stat = "identity",
                        position = "dodge2", 
                        width = 0.7) +
    scale_y_continuous(name = scale_y_lab)+
    theme_minimal() +
    xlab("Age group") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(size = 15),
          axis.text.x = element_text(angle = 60, hjust=1))
   return(pplot)
}

y_vec_3 = c(2025, 2035, 2045)
y_vec_6 = c(2025, 2030, 2035, 2040,2045, 2050)


evo_walk = evo_milage(res_walk)
p_evo_walk = plot_evo_milage(evo = evo_walk,y_vec = y_vec_3,
                             scale_y_lab = "")
                            #  scale_y_lab = "Weekly walking duration (minutes)")

evo_cycle = evo_milage(res_cycle)
p_evo_cycle = plot_evo_milage(evo = evo_cycle,y_vec = y_vec_3,
                              scale_y_lab = "")
                             # scale_y_lab = "Weekly cycling duration (minutes)")

evo_ecycle = evo_milage(res_ecycle)
p_evo_ecycle = plot_evo_milage(evo = evo_ecycle, y_vec = y_vec_3,
                               scale_y_lab = "")
                             # scale_y_lab = "Weekly e-cycling duration (minutes)")




# pour représenter les évolutions, combiner vélo et VAE
evo_all_cycle = evo_cycle
evo_all_cycle$km_pp_y = evo_cycle$km_pp_y + evo_ecycle$km_pp_y
evo_all_cycle$minute_pp_w = evo_cycle$minute_pp_w + evo_ecycle$minute_pp_w
p_evo__all_cycle = plot_evo_milage(evo = evo_all_cycle, y_vec = y_vec_3,
                                   scale_y_lab = "")
                               #scale_y_lab = "Weekly all cycling duration (minutes)")


p_evo_walk
p_evo_cycle
p_evo_ecycle
p_evo__all_cycle

p2 = ggarrange(p_evo_walk, p_evo_cycle, p_evo_ecycle,p_evo__all_cycle,
          labels = c("A", "B", "C", "D"),
          ncol = 1)

tiff("evolution volumes per age.tiff", units="in", width = 5*1.4, height= 8*1.4, res=190)
plot(p2)
dev.off()



##############################
#### plot impact by year
##############################
RR_cycle_low = 0.94
RR_cycle_sup = 0.87
RR_walk_low = 0.96
RR_walk_sup = 0.83

METeCycle_ratio <- 4.5/5.8# valeur de Bouscasse et al : 4.5/5.8
eCycle_RR_low = 1-((1-RR_cycle_low)*METeCycle_ratio)
eCycle_RR_sup = 1-((1-RR_cycle_sup)*METeCycle_ratio)



tot_table = impact_all_types(df_demo = INSEE_data,
                             df_acti = nw,
                             target_distri = den)
res_per_year = aggregate(tot_table$tot_S1$n_prev_wo_S0, by= list(tot_table$tot_S1$year), FUN = "sum")


tot_table_low = impact_all_types(df_demo = INSEE_data,
                                 df_acti = nw,
                                 target_distri = den,
                                 walk_RR =RR_walk_low ,
                                 cycle_RR =RR_cycle_low,
                                 eCycle_RR = eCycle_RR_low)
# sum(tot_table_low$tot_S1$n_prev_wo_S0)
# res_per_year_low = aggregate(tot_table_low$tot_S1$n_prev_wo_S0, by= list(tot_table$tot_S1$year), FUN = "sum")

tot_table_sup = impact_all_types(df_demo = INSEE_data,
                                 df_acti = nw,
                                 target_distri = den,
                                 walk_RR =RR_walk_sup ,
                                 cycle_RR =RR_cycle_sup,
                                 eCycle_RR = eCycle_RR_sup)
# res_per_year_sup = aggregate(tot_table_low$tot_S1$n_prev_wo_S0, by= list(tot_table$tot_S1$year), FUN = "sum")




res_per_year =cbind(age = tot_table$tot_S1$age,
                    year = tot_table$tot_S1$year,
                    death_prev = tot_table$tot_S1$n_prev_wo_S0,
                    death_prev_low = tot_table_low$tot_S1$n_prev_wo_S0,
                    death_prev_sup = tot_table_sup$tot_S1$n_prev_wo_S0,
                    yll = tot_table$tot_S1$yll_prev_wo_S0,
                    yll_low = tot_table_low$tot_S1$yll_prev_wo_S0,
                    yll_sup = tot_table_sup$tot_S1$yll_prev_wo_S0)

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