################
#### Test Kevin rdo velo nW
################

# Check if the packages that we need are installed
want = c("dplyr",
         "tidyr",
         "googlesheets4",
         "ggplot2",
         "readxl", 
         "ggpol",
         "forcats",
         "stringr",
         "rmarkdown",
         "stats", 
         "tidyverse",
         "zoo",
         "foreign",
         "survey",
         "gtsummary",
         "kableExtra",
         "writexl",
         "cowplot",
         "gridExtra")

have = want %in% rownames(installed.packages())

# Install the packages that we miss
if ( any(!have) ) { install.packages( want[!have] ) }

# Load the packages
junk <- lapply(want, library, character.only = T)

# Remove the objects we created
rm(have, want, junk)


##############################################################################
#######################################
#############
## Import nW scenario
#############
#######################################
##############################################################################

# negaWatt.data <- read.csv("C:/Users/Kevin JEAN/Desktop/Recherche/velo_sante/code_kevin/scenar_nW.csv", sep=";",
#                           dec = ",")

#negaWatt.data <-readRDS(file = "negaWatt.data.rds")
negaWatt.data <- read.csv(file = "negawatt.data.v2.csv", h=T, dec =",", sep = ";")
head(negaWatt.data)
summary(negaWatt.data)
negaWatt.data$Year = gsub("2 050", "2050", negaWatt.data$Year)
negaWatt.data$Year = as.numeric(negaWatt.data$Year )


p1 = negaWatt.data %>% 
  ggplot() +  
  geom_line(aes(x = Year, y = value, group = Type, color = Type), size = 1)+
  ylab("") +
  xlab("") +
  labs(title = "negaWatt scenario", subtitle = "In km/inhab/year") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(colour = "#595a5c", size = 12),
        legend.title = element_blank(),
        legend.text = element_text( size = 10),
        legend.position="top",
        axis.text=element_text(size=10),
        axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))+
  ylim(0, 1250)

plot(p1)

##############################################################################
#######################################
#############
## demography & mortality
#############
#######################################
##############################################################################
temp <-  tempfile()

dataURL <- "https://www.insee.fr/fr/statistiques/fichier/2530035/projpop0550_SP19.xls"
download.file(dataURL, destfile=temp, mode='wb')

Pop.proj <- readxl::read_excel(temp, skip = 4, col_names = TRUE)


dataURL <- "https://www.insee.fr/fr/statistiques/fichier/2530048/projpop0550_SD01.xls"
download.file(dataURL, destfile=temp, mode='wb')
Mortality.rate <- readxl::read_excel(temp, skip = 4, col_names = TRUE)

unlink(temp)

Pop.proj = Pop.proj %>% 
  na.omit() %>% 
  mutate(sexe = ifelse(SEXE == "1", "Male", "Female")) %>% 
  rename( "age" = starts_with("AGE")) %>% 
  mutate(across(.cols = c(everything(), - sexe),  as.numeric),
         age = ifelse(is.na(age)== T, 106, age)) %>% 
  select(-SEXE) %>% 
  pivot_longer(!c(age, sexe), names_to = "year", values_to = "Pop") 

Mortality.rate = Mortality.rate %>% 
  na.omit() %>% 
  mutate(sexe = ifelse(SEXE == "1", "Male", "Female")) %>% 
  rename( "age" = starts_with("AGE")) %>% 
  mutate(across(.cols = c(everything(), - sexe),  as.numeric),
         age = ifelse(is.na(age)== T, 106, age)) %>% 
  select(-SEXE) %>% 
  pivot_longer(!c(age, sexe), names_to = "year", values_to = "Deaths") %>% 
  na.omit()


Pop.proj = Pop.proj %>% 
  merge(Mortality.rate, by = c("age", "sexe", "year")) %>% 
  mutate(Mortality.rate = Deaths/Pop) %>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F),
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) 



#recalculate for both sexs
Pop.proj.both = Pop.proj %>% filter(sexe == "Male") ; dim(Pop.proj.both)
Pop.proj.both$sexe = "Both"
Pop.proj.both$Pop = Pop.proj.both$Deaths = Pop.proj.both$Mortality.rate = 0
head(Pop.proj.both)
Pop.proj.both$Pop = Pop.proj$Pop[Pop.proj$sexe=="Male"] + Pop.proj$Pop[Pop.proj$sexe=="Female"]
Pop.proj.both$Deaths = Pop.proj$Deaths[Pop.proj$sexe=="Male"] + Pop.proj$Deaths[Pop.proj$sexe=="Female"]
Pop.proj.both$Mortality.rate = Pop.proj.both$Deaths / Pop.proj.both$Pop
Pop.proj.both$Mortality.rate[Pop.proj.both$Mortality.rate>1] = 1
str(Pop.proj.both)


### age = 105+ coded as 106, re-code as 105+
Pop.proj.both$age[Pop.proj.both$age==106]= 105



# add year 2050
## add the year 2050
lines2050 = Pop.proj.both[Pop.proj.both$year == 2049,]; dim(lines2050)
lines2050$year = 2050
Pop.proj.both = rbind(Pop.proj.both, lines2050)
Pop.proj.both = Pop.proj.both[order(Pop.proj.both$year, Pop.proj.both$age),]





###################
### create yearly life expectancy table
###################
life_exp_tab = data.frame("year" = 2020:2050, "life_exp" = rep(NA, length(2020:2050)))

for (i in 1:nrow(life_exp_tab)){
  yy = life_exp_tab$year[i]
  
  tab = Pop.proj.both[Pop.proj.both$year == yy,]
  tab = tab[order(tab$age),]
  tab$Mortality.rate[tab$age == max(tab$age)] = 1
  
  prop_alive = c(1, cumprod((1 - tab$Mortality.rate) ))
  deaths <- -diff(prop_alive)
  life_exp = sum(deaths * 0:(max(tab$age)) ) 
  life_exp_tab$life_exp[i] = life_exp
}
plot(life_exp_tab$year, life_exp_tab$life_exp)

###################
###################



# for simplicity, delete all ages after 100
Pop.proj.both = Pop.proj.both %>% filter(age<100)



#   add a variable with the proportion of the pop
### 
head(Pop.proj.both)
pop_tot = Pop.proj.both %>% group_by(year) %>% 
  summarize(pop_tot_y = sum(Pop))
plot(pop_tot$year, pop_tot$pop_tot_y)


head(pop_tot)
mm = match(Pop.proj.both$year, pop_tot$year); length(mm)
Pop.proj.both$P_tot_year =  pop_tot$pop_tot_y[mm]
Pop.proj.both$p_prop = Pop.proj.both$Pop/ Pop.proj.both$P_tot_year

yy = 2020
sum(Pop.proj.both$p_prop[Pop.proj.both$year == yy])

Pop.proj.both[Pop.proj.both$year == yy,"age"]



###### add to Pop.proj.both a column with years of life left (= life expectancy - age)
mm = match(Pop.proj.both$year, life_exp_tab$year);length(mm)
Pop.proj.both$YL_left = life_exp_tab$life_exp[mm] - Pop.proj.both$age
Pop.proj.both$YL_left[Pop.proj.both$YL_left<0] = 0

####################################################
#####  Table présentation des données de pop   #####
####################################################

Pop.proj.both_table = Pop.proj.both %>% 
  filter(year %in% c(2020,2030,2050)) %>% 
  filter(order >= 20 & order <= 80) %>% 
  group_by(year, age_grp) %>% 
  summarise(Pop = sum(Pop),
            Mortality.rate = mean(Mortality.rate),
            YL_left = mean(YL_left)) %>% 
  mutate(Pop = round(Pop/1000),
         Mortality.rate = round(Mortality.rate * 100000),
         YL_left = round(YL_left)) %>% 
  select(age_grp, year, Pop, Mortality.rate, YL_left)




##############################################################################
#######################################
#############
## Danemark data
#############
#######################################
##############################################################################

getwd()
### impor age pyramid
pyr = read.csv(file = "Denmark-2018_pyramid.csv", h=T); head(pyr)
pyr_piv =pyr %>% pivot_longer(!Age,names_to = "sex", values_to = "pop")
pyr_piv$AgeGroup = rep(0:(nrow(pyr_piv)/2-1), each=2) # just to match age groups in distrib_Dan.txt
pyr_piv = pyr_piv %>% filter( !(Age %in% c("0-4", "5-9", "10-14", "85-89", "90-94", "95-99", "100+")) )


# import travel volumes
dan = read.table(file = "distrib_Dan.txt", sep = ";", head=T, dec = "," ); dim(dan)
head(dan)
summary(dan)
dan16 = dan %>% filter(Period==2) %>%  filter(minRespAgeCorrect != 10); dim(dan16) # exclude 10-15y
dan16$sex = ifelse(dan16$RespSex == 1, "M", "F")
summary(dan16)

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



############################################################################################################
####################################
#### Now combine Pop.proj.both with negaWatt scenar
head(negaWatt.data)
head(Pop.proj.both)
dim(Pop.proj.both)

nW_walk = negaWatt.data %>%  filter(Type=="Walking")
nW_cycle = negaWatt.data %>%  filter(Type=="Cycling")

nW_walk$pop_tot = pop_tot$pop_tot_y[match(nW_walk$Year, pop_tot$year)]
nW_walk$total_km = nW_walk$value*nW_walk$pop_tot

nW_cycle$pop_tot = pop_tot$pop_tot_y[match(nW_cycle$Year, pop_tot$year)]
nW_cycle$total_km = nW_cycle$value*nW_cycle$pop_tot





# walking/cycling is expressed in km/an/hab; need to translate into min/week
walk_Ref_volume <- 168
walk_speed <- 4.8
RR_walk = 0.89

cycle_Ref_volume <- 100
cycle_speed <- 14
RR_cycle = 0.90

nW_walk$minutes = (60*nW_walk$value /walk_speed) / (365.25/7)
nW_cycle$minutes = (60*nW_cycle$value /cycle_speed) / (365.25/7)

############################################################################################################
##### 
dim(Pop.proj.both)
tab = Pop.proj.both %>% select(age, year, Pop, p_prop, Deaths, Mortality.rate, YL_left) %>% filter(year>2019)
range(tab$age)
dim(tab)


data = tab
#--------------------------------------------------

n_prev = function(data, RR, Ref_volume){
  res = (1-RR)*(data$minute_pp/Ref_volume)*data$Mortality.rate*data$Pop
  return(res)
}


impact_per_type = function(data, type = "cycle", 
                           RR = RR_cycle, 
                           Ref_volume = cycle_Ref_volume,
                           speed = cycle_speed){
  if(type == "cycle"){
    nw_tab = nW_cycle
    rho_vec = dan_av_year$rho_cycle
    
  } else if(type == "walk"){
    nw_tab = nW_walk
    rho_vec = dan_av_year$rho_walk
  }
  
  ####### 
  ##in S1, nW scenario, calculate the km per person
  S1tab = data
  S1tab$rho = rho_vec[match(S1tab$age, dan_av_year$age)]
  S1tab$total_km = nw_tab$total_km[match(S1tab$year, nw_tab$Year)]
  
  # creat sum(rho*pop) for each year
  tmp = S1tab %>% group_by(year) %>% 
    mutate(rho_pop = rho*Pop) %>% 
    summarise(sum_rho_pop = sum(rho_pop))
  S1tab$sum_rho_pop = tmp$sum_rho_pop[match(S1tab$year, tmp$year)] ; rm(tmp)       
  S1tab$km_pp = S1tab$total_km*S1tab$rho/S1tab$sum_rho_pop
  
  # check = S1tab %>% group_by(year) %>% mutate(totkm_per_age = km_pp*Pop) %>%
  # summarise(km_tot = sum(totkm_per_age))
  # cbind(check$km_tot, nW_cycle$total_km[nW_cycle$Year>2019]) # ok !!
  
  S1tab$minute_pp =  (60*S1tab$km_pp /cycle_speed) / (365.25/7)
  
  # create the reference scenario = 2020 volumes all along
  ###### here : change 31 with the number of years. Start in 2021 instead ?
  S0tab = S1tab
  n_rep = nrow(S0tab) / nrow(S1tab[S1tab$year==2020,])
  S0tab$minute_pp = rep(S1tab$minute_pp[S1tab$year==2020], n_rep) 
  
  ### calculated number prevented
  S1tab$n_prev = n_prev(S1tab, RR=RR, Ref_volume=Ref_volume)
  S1tab$YLL = S1tab$n_prev*S1tab$YL_left
  
  S0tab$n_prev = n_prev(S0tab, RR=RR, Ref_volume=Ref_volume)
  S0tab$YLL = S0tab$n_prev*S1tab$YL_left
  
  S1tab$n_prev_wo_S0 = S1tab$n_prev - S0tab$n_prev 
  S1tab$YLL_wo_S0 = S1tab$YLL - S0tab$YLL 
  
  li = list(S1 = S1tab, S0 = S0tab)
  return(li)
}

res_cycle = impact_per_type(data =tab, type = "cycle", 
                            RR = RR_cycle, 
                            Ref_volume = cycle_Ref_volume,
                            speed = cycle_speed)
res_cycle$S1[res_cycle$S1$year == 2050,]


## graph deces evites par age

res_cycle_df = res_cycle$S1 %>% 
  filter(year %in% c(2030,2040,2050)) %>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  filter(order >= 20 & order <= 80) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(n_prev = sum(n_prev))

res_cycle_df %>% 
  ggplot() + geom_bar(aes(age_grp,
                          y = n_prev,
                          fill = year),
                      stat = "identity",
                      position = "dodge", 
                      width = 0.7) +
  scale_y_continuous(c(1000,2000), name = "Décès évités")  +
theme_minimal() +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 60, hjust=1))+
  ggtitle("Décès évités par categories d'âge")

ggsave("Deaths_age_velo.png", plot = last_plot())

res_walk_df = res_walk$S1 %>% 
  filter(year %in% c(2030,2040,2050)) %>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  filter(order >= 20 & order <= 80) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(n_prev = sum(n_prev))


res_both_df = res_cycle_df %>% 
  merge(res_walk_df, by = c("age_grp", "order", "year")) %>% 
  rowwise() %>% 
  mutate(n_prev = sum(n_prev.x, n_prev.y)) 

res_both_df %>% 
  ggplot() + geom_bar(aes(age_grp,
                          y = n_prev,
                          fill = year),
                      stat = "identity",
                      position = "dodge", 
                      width = 0.7) +
  scale_y_continuous(name = "Décès évités")  +
  theme_minimal() +
  xlab("") +
  ylab("Décès évités") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 60, hjust=1))

ggsave("Deaths_age_both.png", plot = last_plot())






tot_cycle = sum(res_cycle$S1$n_prev_wo_S0, na.rm = T); tot_cycle


res_walk = impact_per_type(data =tab, type = "walk", 
                           RR = RR_walk, 
                           Ref_volume = walk_Ref_volume,
                           speed = walk_speed)
tot_walk =sum(res_walk$S1$n_prev_wo_S0, na.rm = T); tot_walk

tot_prev = tot_cycle+tot_walk; tot_prev
tot_walk/tot_prev


########################################
##  graph evolution per age category  ##
########################################

evo_cycling = data_frame(res_cycle$S1) %>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(km_pp = mean(km_pp),
            minute_pp = mean(minute_pp)) %>% 
  group_by(age_grp) %>% 
  mutate(diff = (last(km_pp)-first(km_pp)),
         diff_num = diff,
         diff = paste("+",as.character(round(diff)), "km"),
         diff_percentage = ((last(km_pp)-first(km_pp))/first(km_pp)*100))

evo_cycling.plot = evo_cycling %>%
  filter(year %in% c(2020,2030,2050)) %>% 
  filter(order >= 20 & order <= 80) %>% 
  ggplot(aes(x= km_pp, y= age_grp)) +
  geom_line(aes(group = age_grp),color="grey") +
  geom_point(aes(color=year), size=4) +
  geom_text(color="black", size=2, hjust=-0.25,
            data = subset(evo_cycling, year == 2050 & order >= 20 & order <= 80),
            aes(x=km_pp, label=diff)) +
  theme_minimal() +
  ylab("") +
  xlab("Kilometers traveled per year")+
  theme(legend.position="top", 
        plot.title = element_text( size=14, face="bold.italic"),
        plot.subtitle = element_text(color="#545454", size=10, face="bold.italic"),
        axis.title.x = element_text(color="#545454", size=12, face="bold"),
        axis.text.x = element_text(face="bold"))+
  labs(title = "A. Cycling",color="Years:") +
  xlim(0,1900)
evo_cycling.plot

evo_cycling %>%
  filter(order >= 20 & order <= 80 & year > 2020) %>% 
  ungroup() %>% 
  summarise(mean(km_pp),
            mean(diff_percentage))




evo_walking = data_frame(res_walk$S1) %>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(km_pp = mean(km_pp),
            minute_pp = mean(minute_pp)) %>% 
  group_by(age_grp) %>% 
  mutate(diff = (last(km_pp)-first(km_pp)),
         diff_num = diff,
         diff = paste("+",as.character(round(diff)), "km"),
         diff_percentage = ((last(km_pp)-first(km_pp))/first(km_pp)*100))


evo_walking %>%
  filter(order >= 20 & order <= 80 & year > 2020) %>% 
  ungroup() %>% 
  summarise(mean(km_pp),
            mean(diff_percentage))


test = evo_walking %>%
  filter(order >= 20 & order <= 80) %>% 
  filter(year ==2020 | year ==2030 |year ==2050 )%>% 
  ungroup() %>% 
  mutate(km_pp = round(km_pp)) 


evo_walking %>%
  filter(order >= 20 & order <= 80 & year > 2020) %>% 
  ungroup() %>% 
  summarise(mean(km_pp))

evo_walking %>%
  filter(order >= 20 & order <= 80 & year > 2020) %>% 
  ungroup() %>% 
  summarise(mean(diff_num))

evo_walking %>%
  filter(order >= 20 & order <= 80 & year == 2050) %>% 
  ungroup() %>% 
  summarise(mean(km_pp))


evo_walking.plot = evo_walking %>%
  filter(year %in% c(2020,2030,2050)) %>% 
  filter(order >= 20 & order <= 80) %>% 
  ggplot(aes(x= km_pp, y= age_grp)) +
  geom_line(aes(group = age_grp),color="grey") +
  geom_point(aes(color=year), size=4) +
  geom_text(color="black", size=2, hjust=-0.25,
            data = subset(evo_walking, year == 2050 & order >= 20 & order <= 80),
            aes(x=km_pp, label=diff)) +
  theme_minimal() +
  ylab("") +
  xlab("")+
  theme(legend.position="top", 
        plot.title = element_text( size=14, face="bold.italic"),
        plot.subtitle = element_text(color="#545454", size=10, face="bold.italic"),
        axis.title.x = element_text(color="#545454", size=12, face="bold"),
        axis.text.x = element_text(face="bold"))+
  labs(title = "B. Walking", color="Years:") +
  xlim(300, 600)


evo_walking.plot
  

evo_plot = grid.arrange(evo_cycling.plot, evo_walking.plot, nrow = 1, widths = c(2,2))


ggsave("evo_plot.png", plot = evo_plot)

evo_cycling %>% 
  group_by(year) %>%
  filter(order >= 20 & order <= 80 & year > 2020) %>% 
  summarise(minute_pp = mean(minute_pp)) %>% 
  filter(year == 2050)

Pop.proj.both %>% 
  filter(year == 2050) %>% 
  summarize(Pop = sum(Pop),
            Mortality.rate = mean(Mortality.rate))



####################
# impact per year
####################
death_prev_cycle_year = aggregate(res_cycle$S1$n_prev_wo_S0, by= list(res_cycle$S1$year), FUN = "sum")
death_prev_cycle_year$Type = "Cycle"
death_prev_walk_year = aggregate(res_walk$S1$n_prev_wo_S0, by= list(res_walk$S1$year), FUN = "sum")
death_prev_walk_year$Type = "Walk"

death_prev = rbind(death_prev_cycle_year, death_prev_walk_year)
death_prev$Type = factor(death_prev$Type )
colnames(death_prev) = c("year", "death_prev", "Type")
death_prev$year = as.numeric(death_prev$year)

summary(death_prev)
s = ggplot(data=death_prev, aes(x = year, y = death_prev, fill=Type)) +
  geom_bar(stat = "identity")

plot(s)
ggsave("impact_by_year_DAn_distr.png", width = 9, height = 7, dpi = 300, units = "in")




####################
# impact in YLL per year
####################
YLL_prev_cycle_year = aggregate(res_cycle$S1$YLL_wo_S0, by= list(res_cycle$S1$year), FUN = "sum")
YLL_prev_cycle_year$Type = "Cycle"
YLL_prev_walk_year = aggregate(res_walk$S1$YLL_wo_S0, by= list(res_walk$S1$year), FUN = "sum")
YLL_prev_walk_year$Type = "Walk"
YLL_prev = rbind(YLL_prev_cycle_year, YLL_prev_walk_year)
YLL_prev$Type = factor(YLL_prev$Type )
colnames(YLL_prev) = c("year", "YLL_prev", "Type")
YLL_prev$year = as.numeric(YLL_prev$year)
summary(YLL_prev)

s = ggplot(data=YLL_prev, aes(x = year, y = YLL_prev, fill=Type)) +
  geom_bar(stat = "identity")

plot(s)
ggsave("impact_by_year_YLL_Dan_distr.png", width = 9, height = 7, dpi = 300, units = "in")

YLL_prev %>% 
  summarise(sum(YLL_prev))


############ add error bars
## ref : https://pubmed.ncbi.nlm.nih.gov/25344355/
RR_cycle_low = 0.94
RR_cycle_sup = 0.87
RR_walk_low = 0.96
RR_walk_sup = 0.83

summary(death_prev)
res_cycle_low = impact_per_type(data =tab, type = "cycle", 
                                RR = RR_cycle_low, 
                                Ref_volume = cycle_Ref_volume,
                                speed = cycle_speed)
death_prev_cycle_year_low = aggregate(res_cycle_low$S1$n_prev_wo_S0, by= list(res_cycle_low$S1$year), FUN = "sum")

res_walk_low = impact_per_type(data =tab, type = "walk", 
                               RR = RR_walk_low, 
                               Ref_volume = walk_Ref_volume,
                               speed = walk_speed)
death_prev_walk_year_low = aggregate(res_walk_low$S1$n_prev_wo_S0, by= list(res_walk_low$S1$year), FUN = "sum")
death_prev$death_prev_low = c(death_prev_cycle_year_low$x, death_prev_walk_year_low$x)

res_cycle_sup = impact_per_type(data =tab, type = "cycle", 
                                RR = RR_cycle_sup, 
                                Ref_volume = cycle_Ref_volume,
                                speed = cycle_speed)
death_prev_cycle_year_sup = aggregate(res_cycle_sup$S1$n_prev_wo_S0, by= list(res_cycle_sup$S1$year), FUN = "sum")

res_walk_sup = impact_per_type(data =tab, type = "walk", 
                               RR = RR_walk_sup, 
                               Ref_volume = walk_Ref_volume,
                               speed = walk_speed)
death_prev_walk_year_sup = aggregate(res_walk_sup$S1$n_prev_wo_S0, by= list(res_walk_sup$S1$year), FUN = "sum")
death_prev$death_prev_sup = c(death_prev_cycle_year_sup$x, death_prev_walk_year_sup$x)


###

res_cycle_df_low = res_cycle_low$S1 %>% 
  filter(year %in% c(2030,2040,2050)) %>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  filter(order >= 20 & order <= 80) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(n_prev = sum(n_prev)) %>% 
  rename("low" = "n_prev") %>% 

res_cycle_df_sup = res_cycle_sup$S1 %>% 
  filter(year %in% c(2030,2040,2050)) %>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  filter(order >= 20 & order <= 80) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(n_prev = sum(n_prev))%>% 
  rename("sup" = "n_prev")

res_walk_df_low = res_walk_low$S1 %>% 
  filter(year %in% c(2030,2040,2050)) %>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  filter(order >= 20 & order <= 80) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(n_prev = sum(n_prev))%>% 
  rename("low" = "n_prev")

res_walk_df_sup = res_walk_sup$S1 %>% 
  filter(year %in% c(2030,2040,2050)) %>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  filter(order >= 20 & order <= 80) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(n_prev = sum(n_prev)) %>% 
  rename("sup" = "n_prev")


res_df_sup = res_cycle_df_sup %>% 
  merge(res_walk_df_sup, by = c("year", "order", "age_grp")) %>% 
  rowwise() %>% 
  mutate(sup = sum(n_prev.x, n_prev.y))

res_df_low = res_cycle_df_low %>% 
  merge(res_walk_df_low, by = c("year", "order", "age_grp")) %>% 
  rowwise() %>% 
  mutate(low = sum(n_prev.x, n_prev.y))




res_both_df %>% 
  merge(res_df_low, by = c("year", "order", "age_grp")) %>% 
  merge(res_df_sup, by = c("year", "order", "age_grp")) %>% 
  ggplot(aes( x = age_grp,
    y = n_prev,
          fill = year,
          ymin=low,
          ymax=sup
          )) +
  geom_bar(position = position_dodge(), stat = "identity", width=0.7) + 
  geom_errorbar( position = position_dodge(width = 0.6), colour="black", width=0.3)+
  scale_y_continuous(name = "Décès évités")  +
  theme_minimal() +
  xlab("") +
  ylab("Décès évités") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 60, hjust=1))

ggsave("Deaths_age_both.png", plot = last_plot())

### Graph premature deaths avoided
death_prev_plot = death_prev %>% 
  mutate(Type = ifelse(Type == "Walk", "Walking", "Cycling")) %>% 
  ggplot(aes(x = year, y = death_prev, fill = Type)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=death_prev_low, ymax=death_prev_sup), width=.2,
                position=position_dodge(.9)) + 
  facet_wrap(vars(Type), nrow = 2) +
  ylab("") +
  xlab("") +
  theme_minimal() + 
  theme(legend.position = "none",
        strip.text.x = element_text(
          size = 12, face = "bold"))

death_prev_plot

ggsave("death_prev_plot.png", plot = death_prev_plot)

death_prev %>% 
  group_by(Type) %>% 
  summarise(death_prev_low = mean(death_prev_low),
            death_prev_sup = mean(death_prev_sup)) %>% 
  mutate(graph = ifelse(Type == "Cycle", max(death_prev_low), min(death_prev_sup)), 
         all = "all", 
         proportion = round((graph/sum(graph))*100),2) %>% 
  ggplot(aes(x = all, y = graph, fill = Type)) + geom_col() +
  geom_text(aes(label = paste0(proportion, "%")),
            position = position_stack(vjust = 0.5)) 
  




death_prev_cycle = death_prev %>% 
  filter(Type=="Cycle")

cycling_death = ggplot(data=death_prev_cycle, aes(x = year, y = death_prev, fill=Type)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=death_prev_low, ymax=death_prev_sup), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  theme(legend.position = "",
        plot.title = element_text( size=18, face="bold"),
        plot.subtitle = element_text(color="#808080", size=10, face="bold.italic"),
        legend.title = element_text(face="bold"),
        text = element_text(size = 16)) +
  ylab("Décès prématurés évités")+
  xlab("")+
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Vélo")

cycling_death

walking_death = ggplot(data=death_prev_walk, aes(x = year, y = death_prev, fill=Type)) +
  geom_bar(stat = "identity", fill = "#08bcc4") +
  geom_errorbar(aes(ymin=death_prev_low, ymax=death_prev_sup), width=.2,
                position=position_dodge(.9)) +
  theme_minimal() +
  theme(legend.position = "",
        plot.title = element_text( size=18, face="bold"),
        plot.subtitle = element_text(color="#808080", size=10, face="bold.italic"),
        legend.title = element_text(face="bold"),
        text = element_text(size = 16)) +
  ylab("Décès prématurés évités")+
  xlab("")+
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Marche")

walking_death

Deaths_avoided = grid.arrange(cycling_death, walking_death,
                               ncol=1, nrow=2, widths=c(7), heights=c(4, 4))

ggsave("Deaths_avoided.png", plot = Deaths_avoided, height = 7 , width = 10)



death_prev_walk = death_prev %>% filter(Type=="Walk")
s = ggplot(data=death_prev_walk, aes(x = year, y = death_prev, fill=Type)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=death_prev_low, ymax=death_prev_sup), width=.2,
                position=position_dodge(.9)) 
plot(s)



# total aggregated
total = round(sum(death_prev$death_prev, na.rm=T))
total_low = round(sum(death_prev$death_prev_low, na.rm=T))
total_sup = round(sum(death_prev$death_prev_sup, na.rm=T))

print(paste0(total, " (95% CI: ", total_low, "-", total_sup, ")"))


write_xlsx(death_prev, "death_prev.xlsx")

########################################################################
##################    Graph YYL     ####################################
########################################################################

cycling_YLL = ggplot(data=YLL_prev_cycle_year, aes(x = Group.1, y = x, fill=Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "",
        plot.title = element_text( size=18, face="bold"),
        plot.subtitle = element_text(color="#808080", size=10, face="bold.italic"),
        legend.title = element_text(face="bold"),
        text = element_text(size = 16)) +
  ylab("Décès prématurés évités")+
  xlab("")+
  scale_x_discrete(breaks = seq(2020, 2050, by = 10))+
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Vélo")

cycling_YLL

walking_YLL = ggplot(data=YLL_prev_walk_year, aes(x = Group.1, y = x, fill=Type)) +
  geom_bar(stat = "identity", fill = "#08bcc4") +
  theme_minimal() +
  theme(legend.position = "",
        plot.title = element_text( size=18, face="bold"),
        plot.subtitle = element_text(color="#808080", size=10, face="bold.italic"),
        legend.title = element_text(face="bold"),
        text = element_text(size = 16)) +
  ylab("Décès prématurés évités")+
  xlab("")+
  scale_x_discrete(breaks = seq(2020, 2050, by = 10))+
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Marche")

walking_YLL

YLL_avoided = grid.arrange(cycling_YLL, walking_YLL,
                              ncol=1, nrow=2, widths=c(7), heights=c(4, 4))

ggsave("YLL_avoided.png", plot = YLL_avoided, height = 7 , width = 10)


##############################
########    TOTAL     ########
##############################

death_prev_cycle %>% 
  filter(year > 2020) %>% 
  summarise(mean(death_prev),
            mean(death_prev_low),
            mean(death_prev_sup))

death_prev_walk %>% 
  filter(year > 2020) %>% 
  summarise(mean(death_prev),
            mean(death_prev_low),
            mean(death_prev_sup))

death_prev_cycle %>% 
  filter(year > 2020) %>%
  filter(death_prev == max(death_prev))

death_prev_walk %>% 
  filter(year > 2020) %>%
  filter(death_prev == max(death_prev))


death_prev_cycle %>% 
  filter(year > 2020) %>% 
  summarise(sum(death_prev))

death_prev_walk %>% 
  filter(year > 2020) %>% 
  summarise(sum(death_prev))
  


##############################
##   Economic values        ##
##############################

Eco_value = readxl::read_excel("Eco_value.xls") %>% 
  pivot_longer(!VALUE, names_to = "year", values_to = "value") %>% 
  mutate(Type = VALUE) %>% 
  select(!VALUE)

Value_YLL = Eco_value %>% 
  filter(Type == "VYLL") %>% 
  merge(YLL_prev, by = "year") %>% 
  mutate(total = value*YLL_prev)


Value_YLL %>% 
  summarise( sum(total)/31)
  

Value_Death_prev = Eco_value %>% 
  filter(Type == "VSL") %>% 
  merge(death_prev, by = "year")

Value_Death_prev %>% 
  group_by(Type.y) %>% 
  summarise( sum(death_prev),
             sum( value*death_prev))




########################################################################################################################
##############################
#life_expectancy
##########################################################################################
dim(Pop.proj.both)
tab = Pop.proj.both %>% select(age, year, Pop, p_prop, Deaths, Mortality.rate, YL_left) %>% filter(year>2019)
dim(tab)
data = tab
# add the year 2050
# lines2050 = tab[tab$year == 2049,]; dim(lines2050)
# lines2050$year = 2050
# tab = rbind(tab, lines2050)
# tab = tab[order(tab$year, tab$age),]



life.expectancy = function(data, yy){
  
  res_cycle = impact_per_type(data =data, type = "cycle", 
                              RR = RR_cycle, 
                              Ref_volume = cycle_Ref_volume,
                              speed = cycle_speed)
  
  res_walk = impact_per_type(data =data, type = "walk", 
                             RR = RR_walk, 
                             Ref_volume = walk_Ref_volume,
                             speed = walk_speed)
  names(res_walk)
  
  dta = cbind(data, n_prev_cycle_wo = res_cycle$S1$n_prev_wo_S0, n_prev_walk_wo = res_walk$S1$n_prev_wo_S0)
  # cap mortality 
  dta$Mortality.rate[dta$Mortality.rate>1] = 1
  dta$Mortality.rate[dta$age==max(dta$age)] = 1
  range(dta$Mortality.rate)
  
  
  # keep year yy only
  tmp = dta%>%  filter(year == yy)
  tmp = tmp[order(tmp$age),]
  
  mortality.S1 =(tmp$Deaths - tmp$n_prev_cycle_wo - tmp$n_prev_walk_wo)/tmp$Pop
  tmp$Mortality.rate.S1 = ifelse( mortality.S1>=1,tmp$Mortality.rate,  mortality.S1 )
  tmp$Mortality.rate.S1[tmp$age==max(tmp$age)] = 1
  
  range(tmp$Mortality.rate.S1)
  mean(tmp$Mortality.rate)
  mean(tmp$Mortality.rate.S1)
  #why mortality rate S1 are higher ????
  tmp$diff = tmp$Mortality.rate - tmp$Mortality.rate.S1
  all(tmp$diff >= 0)
  # troubles arising after 100y
  
  prop_alive_S0 = c(1, cumprod((1 - tmp$Mortality.rate) ))
  deaths_S0 <- -diff(prop_alive_S0)
  life_exp_S0 = sum(deaths_S0 * 0:(max(tmp$age)) ) 
  
  prop_alive_S1 = c(1, cumprod((1 - tmp$Mortality.rate.S1) ))
  deaths_S1 <- -diff(prop_alive_S1)
  life_exp_S1 = sum(deaths_S1 * 0:(max(tmp$age)) ) 
  
  diff_exp = life_exp_S1 - life_exp_S0
  
  return(data.frame("life_exp_S0" = life_exp_S0, "life_exp_S1"=life_exp_S1, "difference"=diff_exp))
}

Data_list = list()
for (i in 2020:2050){
  output = life.expectancy(tab, i)
  Data_list[[length(Data_list)+1]] = output
}

life.expectancy.all = do.call(rbind.data.frame, Data_list) %>% 
  mutate(year = row_number()+2019) %>% 
  pivot_longer(life_exp_S0:life_exp_S1, names_to = "Scenario", values_to = "value")


##############################
##   Graph Life Expectancy  ##
##############################

graph.A = life.expectancy.all %>%
  mutate(Scenario = ifelse(Scenario == "life_exp_S0",
                           "Baseline",
                           "NegaWatt")) %>% 
  ggplot() + geom_line(aes(y = value, x = year, group = Scenario, color = Scenario))  + 
  theme_minimal() +
  xlab("") +
  ylab("")+
  theme(legend.position="bottom", 
       plot.title = element_text( size=14, face="bold.italic"),
       plot.subtitle = element_text(color="#808080", size=10, face="bold.italic"),
       legend.title = element_text(face="bold"))+
  labs(color="Scenario:") +
  ggtitle("B. Life Expectancy Projections",
        sub = "In years")

graph.A

legend.graph.A = get_legend(graph.A)
legend.graph.A

graph.A = graph.A +
  theme(legend.position = "none")

graph.B = life.expectancy.all %>% 
  mutate(difference.month = difference* 12) %>% 
  filter(Scenario == "life_exp_S0") %>% 
  ggplot(aes(x = year, y = difference.month)) + geom_bar(stat = "identity") +
  scale_y_continuous( breaks = c(seq(from = 0,to = 9,3)))+ 
  theme_minimal() +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text( size=14, face="bold.italic"),
        plot.subtitle = element_text(color="#808080", size=10, face="bold.italic"))+
  ggtitle("A. Difference between both Scenarios",
        sub = "In months")

library("gridExtra")
life.expectancy.graph <-grid.arrange(graph.B, graph.A,legend.graph.A,
             ncol=1, nrow=3, widths=c(4), heights=c(3, 7,1))

ggsave("life.expectancy.graph.png", plot = life.expectancy.graph)

###################################
##   Graph Life Expectancy en FR ##
###################################

graph.A = life.expectancy.all %>%
  mutate(Scenario = ifelse(Scenario == "life_exp_S0",
                           "Tendanciel",
                           "NegaWatt")) %>% 
  ggplot() + geom_line(aes(y = value, x = year, group = Scenario, color = Scenario))  + 
  theme_minimal() +
  xlab("") +
  ylab("")+
  theme(legend.position="bottom", 
        plot.title = element_text( size=14, face="bold.italic"),
        plot.subtitle = element_text(color="#808080", size=10, face="bold.italic"),
        legend.title = element_text(face="bold"))+
  labs(color="Scenario:") +
  ggtitle("B. Projections d'Espérance de Vie en France",
          sub = "En années")

graph.A

legend.graph.A = get_legend(graph.A)
legend.graph.A

graph.A = graph.A +
  theme(legend.position = "none")

graph.B = life.expectancy.all %>% 
  mutate(difference.month = difference* 12) %>% 
  filter(Scenario == "life_exp_S0") %>% 
  ggplot(aes(x = year, y = difference.month)) + geom_bar(stat = "identity") +
  scale_y_continuous( breaks = c(seq(from = 0,to = 9,3)))+ 
  theme_minimal() +
  xlab("") +
  ylab("") +
  theme(plot.title = element_text( size=14, face="bold.italic"),
        plot.subtitle = element_text(color="#808080", size=10, face="bold.italic"))+
  ggtitle("A. Difference entre les Deux Scénarios",
          sub = "En Mois")

library("gridExtra")
life.expectancy.graph <-grid.arrange(graph.B, graph.A,legend.graph.A,
                                     ncol=1, nrow=3, widths=c(4), heights=c(3, 7,1))

ggsave("life.expectancy.graph.png", plot = life.expectancy.graph)

##############################
### Life expectancy total  ###
##############################


life.expectancy.all %>% 
  filter(difference == max(difference)) %>% 
  mutate(round(difference*12))


####################################
### Graph representatif de HEAT  ###
####################################
Year = seq(2020,2050)
Tendance = rep(1, 31)
Scenario = seq(1,3,2/30)

Graph.HEAT = data.frame(Year, Baseline, Scenario) %>% 
  pivot_longer(!Year, names_to = "type")

g = 
  Graph.HEAT %>%  
  ggplot() + geom_bar(aes(x = Year,
                          y = value,
                          group = type,
                          fill = type),
                      width = 0.8,
                      stat="identity", position=position_dodge())+ 
  geom_curve(aes(x = 2051, y = 1, xend = 2051, yend = 3),
             size = 1.5,
             curvature = 0.1)+
  geom_text( aes( x=2053, y=2),
             label = "Bénéfice en Santé",
              size=6 , angle=270 ) +
  theme_minimal() +
  xlab("") +
  ylab("Volume d'activité physique") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_blank(), 
        text = element_text(size = 15))
g

ggsave("Graph_HEAT_Example.png", plot = g, height = 7 , width = 12)

####################################
### Graph en Français    ###
####################################

p1 = negaWatt.data %>% 
  mutate(Type = ifelse(Type == "Walking", "Marche", "Vélo")) %>% 
  ggplot() +  
  geom_line(aes(x = Year, y = value, group = Type, color = Type), size = 1.5)+
  ylab("") +
  xlab("") +
  labs(title = "Scénario negaWatt", subtitle = "En km/hab/an") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(colour = "#595a5c", size = 12),
        legend.text = element_text(size = 12),
        legend.position="top",
        axis.text=element_text(size=10),
        axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1),
        text = element_text(size = 16))+
  ylim(0, 1250) +
  labs(color="Mode de Transportation:")
p1

ggsave("Negawatt2.png", plot = p1, height = 7 , width = 12)




Data.Denmark.distribution = readRDS("Data.Denmark.distribution.rds")

Data.Denmark.distribution = Data.Denmark.distribution %>% 
  group_by(type, Period, age_grp) %>%
  summarise(value = sum(value)) %>% 
  group_by(type, Period) %>%
  mutate(freq = value/sum(value)) 

Data.Denmark.distribution %>% 
  group_by(type, Period) %>% 
  summarise(sum(freq))


#### Cycling
p1 =Data.Denmark.distribution %>%
  filter(type != "Walking" & Period == 2) %>%
  ggplot() + geom_bar(aes(x = age_grp, y = freq, fill = type), stat = "identity") +  
  ylab("") +
  xlab("") +
  labs(title = "Vélo") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top")+ 
  scale_y_continuous(limits = c(0,0.1),
                     expand = c(0, 0),
                     labels = function(x) paste0(x*100, "%"))+ 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1),
        legend.position = "") 
p1

#### Walking
p2 =Data.Denmark.distribution %>%
  filter(type == "Walking" & Period == 2)  %>%
  ggplot() + geom_bar(aes(x = age_grp, y = freq, fill = type), stat = "identity") +  
  ylab("") +
  xlab("") +
  labs(title = "Marche") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "top")+ 
  scale_y_continuous(limits = c(0,0.1), expand = c(0, 0),
                     labels = function(x) paste0(x*100, "%"))+  
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1),
        legend.position = "")
p2

Distri.Denmark2 = grid.arrange(p1, p2,
             ncol=1, nrow=2, widths=c(10), heights=c(4, 4))

ggsave("Distri.Denmark2.png", plot = Distri.Denmark2, height = 7 , width = 12)


###############################
## Example pour presentation ##
###############################


S0.cycling = as.data.frame(res_cycle[["S0"]]) %>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(km_pp = mean(km_pp),
            minute_pp = mean(minute_pp))

S0.walking = as.data.frame(res_walk[["S0"]])%>% 
  mutate(age_grp.FACTOR = cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F), #gather by age group
         age_grp = as.character(age_grp.FACTOR), 
         age_grp = gsub("\\[|\\]|\\(|\\)", "", age_grp),
         age_grp = gsub(",", "-", age_grp),
         post = sub(".*-","",age_grp),
         age_grp = sub("-.*", "", age_grp),
         age_grp = paste0(age_grp,"-", as.numeric(post)-1),
         order = as.numeric(substr(age_grp,1,regexpr("-",age_grp)-1))) %>% 
  group_by(age_grp, order, year) %>% 
  summarise(km_pp = mean(km_pp),
            minute_pp = mean(minute_pp),
            n_prev = mean(n_prev),
            YLL = mean(YLL))

S0.cycling %>% 
  filter(order == 50 & year == 2020)

S0.walking %>% 
  filter(order == 50 & year == 2030)

