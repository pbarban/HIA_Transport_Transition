# Check if the packages that we need are installed
pacman::p_load(ggplot2) 


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



