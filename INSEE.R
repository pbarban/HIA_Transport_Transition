#read functions
source("functions.R")

#packages
pacman::p_load(dplyr,
               tidyr,
               imputeTS)

#data cleaning
Pop.proj = download_INSEE("SP19") 

Mortality.rate = download_INSEE("SD01") 

All_data = Pop.proj %>% 
  merge(Mortality.rate, by = c("age", "sexe", "year"), all = T) %>% 
  mutate(pop = ifelse(is.na(pop), 0, pop)) %>%
  group_by(sexe, age) %>%
  mutate(deaths = na_kalman(deaths, model = "auto.arima")) %>%  # Estimate nbr of deaths in 2050
  mutate(MR = deaths/pop)

INSEE_data =  All_data %>% 
  ungroup() %>% 
  mutate(age_grp = age_grp(age),
         MR = ifelse(MR > 1 | pop == 0, 1, MR)) %>% # Some MR are >1, not exactly sure why...
 filter(age<101) %>% # for simplicity, delete all ages >100
  group_by(sexe, year) %>%
  mutate(p_tot =sum(pop),
        p_prop = pop/p_tot) %>% 
  filter(sexe == "Both") %>% 
  ungroup()

INSEE_data %>% 
  group_by(sexe, year) %>% 
  summarise(MR = sum(deaths)) %>% 
  ggplot() + 
  geom_line(aes(x = year,
                y = MR,
                group = sexe,
                color = sexe ))+
  theme(axis.text.x = element_text(angle = 90))
  

# calculate life expectancy
life_exp_tab = data.frame("year" = 2020:2050, "life_exp" = rep(NA, length(2020:2050)))

for (i in 1:nrow(life_exp_tab)){
  yy = life_exp_tab$year[i]
  
  tab = INSEE_data[INSEE_data$year == yy,]
  tab = tab[order(tab$age),]
  tab$MR[tab$age == max(tab$age)] = 1
  
  prop_alive = c(1, cumprod((1 - tab$MR) ))
  deaths <- -diff(prop_alive)
  life_exp = sum(deaths * 0:(max(tab$age)) ) 
  life_exp_tab$life_exp[i] = life_exp
}
plot(life_exp_tab$year, life_exp_tab$life_exp)

#ajouter experence de vie esperance de vie - age = yll




INSEE_data = INSEE_data %>% mutate()



rm(Pop.proj, Mortality.rate, All_data)

