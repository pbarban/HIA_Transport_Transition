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

df = All_data %>% 
  ungroup() %>% 
  mutate(age_grp = age_grp(age),
         MR = ifelse(MR > 1 | pop == 0, 1, MR)) # Some MR are >1, not exactly sure why...

#recalculate for both sexs
df.both = df %>% filter(sexe == "Male") ; dim(df.both)
df.both$sexe = "Both"
head(df.both)
df.both$pop = df.both$deaths = df.both$MR = 0
df.both$pop = df$pop[df$sexe=="Male"] + df$pop[df$sexe=="Female"]
df.both$deaths = df$deaths[df$sexe=="Male"] + df$deaths[df$sexe=="Female"]
df.both$MR = df.both$deaths / df.both$pop
df.both$MR[df.both$MR>1] = 1

#concatene to df
df = rbind(df, df.both)
table(df$sexe)

#Life expectancy table 
library(ggplot2)
life_exp(df, year,sexe,  MR)  %>% 
  ggplot() + 
  geom_line(aes(x = year, y = life_exp, group = sexe, color = sexe)) +
  theme(axis.text.x = element_text(angle = 90))
