################
#### code cleané à partir de Codes.R
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
negaWatt.data <-read_rds("nw_data.rds")
str(negaWatt.data)

negaWatt.data$type <- factor(negaWatt.data$type, levels=c("walk","cycle", "e_cycle"), labels=c("Walk", "Bike", "e-Bike"))
table(negaWatt.data$type)

p1 = negaWatt.data %>% 
  ggplot() +  
  geom_line(aes(x = year, y = value/52.1, group = type, color = type), size = 1)+
  ylab("") +
  xlab("") +
  labs(title = "Trends in active transportation mileage, negaWatt scenario", subtitle = "In km/inhab/week") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(colour = "#595a5c", size = 12),
        legend.position="top",
        axis.text=element_text(size=10),
        axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))+
  ylim(0, 13)

plot(p1)
