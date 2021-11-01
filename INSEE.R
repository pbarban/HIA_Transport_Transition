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
  mutate(Mortality.rate = deaths/pop)

df = All_data %>% 
  ungroup() %>% 
  mutate(age_grp = age_grp(age),
         Mortality.rate = ifelse(Mortality.rate > 1, 1, Mortality.rate)) # Some MR are >1, not excatly sure why...

life_exp = function(df, year,sexe, MR){
test = df %>% 
  group_by(year,sexe) %>% 
  arrange(year,sexe) %>% 
  mutate(prop_alive = cumprod(1 - MR),
         deaths =  -(prop_alive - lag(prop_alive)),
         life_exp = age *deaths) %>% 
  group_by(sexe, year) %>% 
  summarise(life_exp = sum(life_exp, na.rm = T)) 
return(test)
}


life_exp(df, MR = Mortality.rate)





#Life expectancy table 
library(ggplot2)
life_exp(df, year,sexe,  Mortality.rate) 

test %>% 
  ggplot() + geom_line(aes(x = year, y = life_exp, group = sexe, color = sexe))



life_exp_tab = data.frame("year" = 2020:2050, "life_exp" = rep(NA, length(2020:2050)))

for (i in nrow(life_exp_tab)){
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

prop_alive
deaths
