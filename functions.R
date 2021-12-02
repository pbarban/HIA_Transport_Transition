# Test function
test_fun = function(x) {
  if(is.numeric(x)){
    x = x +7
  }
  else{
    warning(paste(x, "is not a numeric my friend"))
  }
  print(x)
}


# Download INSEE scenarios
# Also cleaned the dataframe and give a long output instead of wide
# Option both always set as TRUE, add together women and men 

scenario = "SP01"

download_INSEE = function(scenario = "SP01", both = T){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(readxl,
                 tidyr)
  
  temp <-  tempfile()
  
  if(substr(scenario,1,2) == "SP"){
    id = 2530035 #ID for pop proj
    type = "pop"
  }
  
  else if(substr(scenario,1,2) == "SD") {
    id = 2530048 #ID for mortality
    type = "deaths"
  }
  
  dataURL <- paste0("https://www.insee.fr/fr/statistiques/fichier/",id,"/projpop0550_",scenario,".xls")
  download.file(dataURL, destfile=temp, mode='wb')
  data <- readxl::read_excel(temp, skip = 4, col_names = TRUE)
  
  if(both == T){
  data_cleaned = data %>% 
    na.omit() %>% 
    rename( "age" = starts_with("AGE")) %>% 
    mutate(age = ifelse(substr(age, nchar(age), nchar(age)) == "+",
                        gsub('.{1}$', '', age),
                        age),
           sexe = ifelse(SEXE == "1", "Male", "Female")) %>% 
    mutate(across(.cols = c(everything(), - sexe),  as.numeric)) %>% 
    select(-SEXE) %>% 
    pivot_longer(!c(age, sexe),
                 names_to = "year",
                 values_to = all_of(type) ) %>% 
    pivot_wider(names_from = sexe,
                values_from = all_of(type)) %>% 
    mutate(Both = Female + Male) %>% 
    pivot_longer(!c(age, year),
                 names_to = "sexe",
                 values_to = all_of(type))
  }
  
  else{
    data_cleaned = data %>% 
      na.omit() %>% 
      rename( "age" = starts_with("AGE")) %>% 
      mutate(age = ifelse(substr(age, nchar(age), nchar(age)) == "+",
                          gsub('.{1}$', '', age),
                          age),
             sexe = ifelse(SEXE == "1", "Male", "Female")) %>% 
      mutate(across(.cols = c(everything(), - sexe),  as.numeric)) %>% 
      select(-SEXE) %>% 
      pivot_longer(!c(age, sexe),
                   names_to = "year",
                   values_to = all_of(type) )
    
  }
  return(data_cleaned)
}

# function that create categories based on an age column

age_grp = function(age){
  age_grp = gsub(",", "-",
                 gsub("\\[|\\]|\\(|\\)", "",
                      cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F)))
  post = sub(".*-","",age_grp)
  age_grp = paste0(sub("-.*", "", age_grp),
                   "-", as.numeric(post)-1)
  return(age_grp)
}

# Get life expectancy by year and sex
life_exp = function(df, MR ,age){
  test = df %>% 
    mutate(prop_alive = cumprod(1 - MR),
           deaths =  -(prop_alive - lag(prop_alive)),
           life_exp = sum(age *deaths, na.rm = T)) 
  return(test)
}

# Get population demographic pyramid 
pop_pyramid = function(country,year){
   temp <-  tempfile()
  
  if(country == "Denmark"){
    id = 208
  }
  else if(country == "Netherlands"){
    id = 528
  }
  
  dataURL = paste("https://www.populationpyramid.net/api/pp",id,year,"?csv=true", sep = "/") 
  download.file(dataURL, destfile=temp, mode='wb')
  data <- read.csv(temp)
  
  file.remove(temp)
  return(data)
}

# Disaggregate discrete age categories in dataframe into continuous age

disaggregate_age = function(df, age_grp){
  
  age = df %>% 
    group_by(age_grp) %>% 
    select(age_grp) %>%
    distinct() %>% 
    mutate(pre = as.numeric(sub("\\-.*", "", age_grp)),
           post = as.numeric(sub(".*\\-", "", age_grp)),
           diff = post - pre) %>% 
    slice(rep(row_number(), diff +1))  %>% 
    mutate(n = row_number() - 1,
           age = pre + n) %>% 
    select(age_grp, age)
  
  df = df %>% 
    merge(age, by = "age_grp")
  
  return(df)
}

### Give the rho needed to get a ratio between groups 
# Could be improved 
## Dont forget to groub_by before 

rho <- function(df,distance){
  df <- df %>% 
    mutate(rho = distance / first(distance))
  return(df)
}













############################################################################################
##############################################
#### functions of the health impact assessement itself
##############################################
############################################################################################


n_prev = function(data, RR, Ref_volume){
  # calculate the number of death prevented based on a demo dataset, a RR and a Ref_volume
  res = (1-RR)*(data$minute_pp_w/Ref_volume)*data$MR*data$pop
  return(res)
}


impact_per_type = function(df_demo, # demographic data frame
                           df_acti, # data frame of physical activity
                           target_distri, # data frame with the target age-distribution of physical activity
                           type_eval = "cycle", # has to be "walk", "cycle" or "e_cycle"
                           RR = cycle_RR, 
                           Ref_volume = cycle_Ref_volume,
                           speed = cycle_speed){
  # output a list with 2 demographic tables which calculate for each year and age the number of deaths prevented + yll
  # for a specified transport type
  # requirs a demographic dataset, a dataset of physical activity volumes and a target distribution of volumes
  
  acti =  df_acti %>% filter(type == type_eval)
  
  type_target = ifelse(type_eval == "e_cycle" , "cycle", type_eval)# if e_bike, use the target distrib of classic bike
  target = target_distri %>% filter(type == type_target)
  
  ####### 
  ##in S1, the scenario assessed, calculate the km_w per person
  S1tab = df_demo
  S1tab = S1tab %>% arrange(year)
  S1tab$rho = as.numeric(target$rho[match(S1tab$age, target$age)])
  S1tab$total_km_y = acti$total_km_y[match(S1tab$year, acti$year)]
  
  # creat sum(rho*pop) for each year
  tmp = S1tab %>% group_by(year) %>% 
    mutate(rho_pop = rho*pop,
           sum_rho_pop = sum(rho_pop))
  S1tab$sum_rho_pop = tmp$sum_rho_pop[match(S1tab$year, tmp$year)] ; rm(tmp)       
  S1tab$km_pp_y = S1tab$total_km_y*S1tab$rho/S1tab$sum_rho_pop
  
  S1tab$minute_pp_w =  (60*S1tab$km_pp_y /speed) / (365.25/7)
  
  ####### 
  # create the reference scenario = 2020 volumes all along
  S0tab = S1tab
  n_rep = nrow(S0tab) / nrow(S1tab[S1tab$year==2021,])
  S0tab$minute_pp_w = rep(S1tab$minute_pp_w[S1tab$year==2021], n_rep) 
  
  ### calculated number prevented
  S1tab$n_prev = n_prev(S1tab, RR=RR, Ref_volume=Ref_volume)
  S1tab$yll_prev = S1tab$n_prev*S1tab$yll
  
  S0tab$n_prev = n_prev(S0tab, RR=RR, Ref_volume=Ref_volume)
  S0tab$yll_prev = S0tab$n_prev*S1tab$yll
  
  S1tab$n_prev_wo_S0 = S1tab$n_prev - S0tab$n_prev 
  S1tab$yll_prev_wo_S0 = S1tab$yll_prev - S0tab$yll_prev 
  
  li = list(S1 = S1tab, S0 = S0tab)
  return(li)
}



####### now wrap up a function calculating the impact of all types of transport
impact_all_types = function(df_demo, # demographic data frame
                            df_acti, # data frame of physical activity
                            target_distri, # data frame with the target age-distribution of physical activity
                            cycle_RR = 0.90, 
                            cycle_Ref_volume = 100,
                            cycle_speed = 14,
                            walk_RR = 0.89,
                            walk_Ref_volume= 168,
                            walk_speed=4.8,
                            eCycle_RR= 0.9224138,
                            eCycle_Ref_volume =100,
                            eCycle_speed = 18){
  
  res_walk = impact_per_type(df_demo = df_demo,
                             df_acti = df_acti,
                             target_distri = target_distri,
                             type_eval = "walk", 
                             RR = walk_RR, 
                             Ref_volume = walk_Ref_volume,
                             speed = walk_speed)
  
  res_cycle = impact_per_type(df_demo = df_demo,
                              df_acti = df_acti,
                              target_distri = target_distri,
                              type_eval = "cycle", 
                              RR = cycle_RR, 
                              Ref_volume = cycle_Ref_volume,
                              speed = cycle_speed)
  
  res_ecycle = impact_per_type(df_demo = df_demo,
                               df_acti = df_acti,
                               target_distri = target_distri,
                               type_eval = "e_cycle", 
                               RR = eCycle_RR, 
                               Ref_volume = eCycle_Ref_volume,
                               speed = eCycle_speed)
  
  tot_S1tab = df_demo
  tot_S1tab = tot_S1tab %>% arrange(year) 
  tot_S0tab = tot_S1tab
  
  tot_S0tab = tot_S0tab %>% 
    mutate(n_prev = res_walk$S0$n_prev + res_cycle$S0$n_prev + res_ecycle$S0$n_prev,
           yll_prev = res_walk$S0$yll_prev + res_cycle$S0$yll_prev + res_ecycle$S0$yll_prev)
  tot_S1tab = tot_S1tab %>% 
    mutate(n_prev = res_walk$S1$n_prev + res_cycle$S1$n_prev + res_ecycle$S1$n_prev,
           yll_prev = res_walk$S1$yll_prev + res_cycle$S1$yll_prev + res_ecycle$S1$yll_prev)
  tot_S1tab = tot_S1tab %>% 
    mutate(n_prev_wo_S0 = tot_S1tab$n_prev - tot_S0tab$n_prev,
           yll_prev_wo_S0 = tot_S1tab$yll_prev- tot_S0tab$yll_prev)
  
  li = list(tot_S1 = tot_S1tab, tot_S0 = tot_S0tab)
  return(li)
  
}
