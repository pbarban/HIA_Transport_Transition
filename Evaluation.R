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

source("Denmark_data.R")
source("INSEE.R")
source("negaWatt_data.R")



#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
###################
### create yearly life expectancy table
###################
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

#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################