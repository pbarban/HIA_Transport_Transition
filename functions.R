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

########################################################################################

download_INSEE <- function(FEC = "bas",
                           ESP = "cent",
                           MIG = "cent",
                           age_limit,
                           year_limit){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(readxl,
                 tidyr)
  
  id = 2517966
  
  scenario = paste0("FEC",FEC,"ESP",ESP,"MIG",MIG)
  
  temp <-  tempfile()
  
  dataURL <- paste0("https://www.insee.fr/fr/statistiques/fichier/",id,"/projpop0760_",scenario,".xls")
  download.file(dataURL, destfile=temp, mode='wb')
  
  #### #### #### #### #### #### #### #### #### #### #### #### #### 
  #### #### #### ####           pop            #### #### #### #### 
  #### #### #### #### #### #### #### #### #### #### #### #### #### 
  
  
  data_pop_tot <- readxl::read_excel(temp, sheet = "populationTot", skip = 4, col_names = TRUE) %>% 
    rename( "age" = ends_with("janvier")) %>% 
    mutate(age = as.numeric(gsub("([0-9]+).*$", "\\1", age))) %>% 
    na.omit() %>% 
    mutate(across(.cols = c(everything()),  as.numeric))%>% 
    pivot_longer(!c(age),
                 names_to = "year",
                 values_to = "pop") %>% 
    mutate(sexe = "Both")
  
  data_pop_F <- readxl::read_excel(temp, sheet = "populationF", skip = 4, col_names = TRUE)%>% 
    rename( "age" = ends_with("janvier")) %>% 
    mutate(age = as.numeric(gsub("([0-9]+).*$", "\\1", age))) %>% 
    na.omit() %>% 
    mutate(across(.cols = c(everything()),  as.numeric))%>% 
    pivot_longer(!c(age),
                 names_to = "year",
                 values_to = "pop") %>% 
    mutate(sexe = "Female")
  
  data_pop_H <- readxl::read_excel(temp, sheet = "populationH", skip = 4, col_names = TRUE) %>% 
    rename( "age" = ends_with("janvier")) %>% 
    mutate(age = as.numeric(gsub("([0-9]+).*$", "\\1", age))) %>% 
    na.omit() %>% 
    mutate(across(.cols = c(everything()),  as.numeric))%>% 
    pivot_longer(!c(age),
                 names_to = "year",
                 values_to = "pop") %>% 
    mutate(sexe = "Male")
  
  all_data_pop <- rbind(data_pop_tot, data_pop_F) %>% 
    rbind(data_pop_H)
  
  #### #### #### #### #### #### #### #### #### #### #### #### #### 
  #### #### #### ####           décès          #### #### #### #### 
  #### #### #### #### #### #### #### #### #### #### #### #### #### 
  
  data_deaths_tot <- readxl::read_excel(temp, sheet = "nbre_deces", skip = 4, col_names = TRUE)%>% 
    rename( "age" = ends_with("e")) %>% 
    mutate(age = as.numeric(gsub("([0-9]+).*$", "\\1", age))) %>% 
    na.omit() %>% 
    mutate(across(.cols = c(everything()),  as.numeric))%>% 
    pivot_longer(!c(age),
                 names_to = "year",
                 values_to = "deaths") %>% 
    mutate(sexe = "Both")
  
  data_deaths_F <- readxl::read_excel(temp, sheet = "nbre_decesF", skip = 4, col_names = TRUE)%>% 
    rename( "age" = ends_with("e")) %>% 
    mutate(age = as.numeric(gsub("([0-9]+).*$", "\\1", age))) %>% 
    na.omit() %>% 
    mutate(across(.cols = c(everything()),  as.numeric))%>% 
    pivot_longer(!c(age),
                 names_to = "year",
                 values_to = "deaths") %>% 
    mutate(sexe = "Female")
  
  data_deaths_H <- readxl::read_excel(temp, sheet = "nbre_decesH", skip = 4, col_names = TRUE)%>% 
    rename( "age" = ends_with("e")) %>% 
    mutate(age = as.numeric(gsub("([0-9]+).*$", "\\1", age))) %>% 
    na.omit() %>% 
    mutate(across(.cols = c(everything()),  as.numeric))%>% 
    pivot_longer(!c(age),
                 names_to = "year",
                 values_to = "deaths") %>% 
    mutate(sexe = "Male")
  
  all_data_deaths <- rbind(data_deaths_tot, data_deaths_F) %>% 
    rbind(data_deaths_H)
  
  all_data <- merge(all_data_pop, all_data_deaths, by = c("age", "year","sexe"))
  
  #### #### #### #### #### #### #### #### #### #### #### #### #### 
  #### #### #### ####          Mortalité       #### #### #### #### 
  #### #### #### #### #### #### #### #### #### #### #### #### #### 
  
  data_mortalite_F <- readxl::read_excel(temp, sheet = "hyp_mortaliteF", skip = 4, col_names = TRUE)%>% 
    rename( "age" = ends_with("e")) %>% 
    mutate(age = as.numeric(gsub("([0-9]+).*$", "\\1", age))) %>% 
    na.omit() %>% 
    mutate(across(.cols = c(everything()),  as.numeric))%>% 
    pivot_longer(!c(age),
                 names_to = "year",
                 values_to = "MR") %>% 
    mutate(sexe = "Female")
  
  data_mortalite_H <- readxl::read_excel(temp, sheet = "hyp_mortaliteH", skip = 4, col_names = TRUE) %>% 
    rename( "age" = ends_with("e")) %>% 
    mutate(age = as.numeric(gsub("([0-9]+).*$", "\\1", age))) %>% 
    na.omit() %>% 
    mutate(across(.cols = c(everything()),  as.numeric))%>% 
    pivot_longer(!c(age),
                 names_to = "year",
                 values_to = "MR") %>% 
    mutate(sexe = "Male")
  
  all_data_MR <- rbind(data_mortalite_H, data_mortalite_F)
  
  data_mortalite_Both <- all_data_MR %>% 
    group_by(age, year) %>% 
    summarise(MR = mean(MR)) %>% 
    ungroup()%>% 
    mutate(sexe = "Both")
  
  all_data_MR <- all_data_MR %>% 
    rbind(data_mortalite_Both)
  
  
  all_data <- merge(all_data, all_data_MR, by = c("age", "year","sexe")) %>% 
    mutate(MR_manual = deaths/pop) 
  
  if(missing(year_limit) == F){
    all_data <- all_data %>% 
      filter(year <= year_limit)
  }
  
  #if(missing(age_limit) == F){
  #  all_data <- all_data %>% 
  #    filter(age <= age_limit)
  #}
  
  return(all_data)
}


########################################################################################

download_INSEE2 = function(scenario = "SP01", both = T){
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
n_prev = function(data){
  # calculate the number of death prevented based on a dataset formated as S1tab, with colums as RR and a Ref_volume
  res = (1-data$RR)*(data$minute_pp_w/data$Ref_volume)*data$MR*data$pop
  return(res)
}


optimize_rho_a = function(par = c(a, b), tab,
                          obj_delta = 6.7,
                          obj_rho = obj_rho,
                          coef_delta = 1, #coef to give the relative importance of criteria delta
                          coef_rho=5 #coef to give the relative importance of criteria rho
){
  tab$rho_a = par[1]*tab$age+par[2]
  tab$rho_a[tab$rho_a<=0] = 0.001
  tab$rho_a[tab$rho_a>=1] = 0.90 #rho_a is the vector of proportion of KM cycled with eBike per age
  
  tab$km_y_a = tab$km_pp_y_bike*tab$pop
  
  # ebike
  a_rho_a_km_a_ebike = tab$age*tab$km_y_a*tab$rho_a #sum of km weighted by age
  rho_a_km_a_ebike = tab$km_y_a*tab$rho_a # sum of km ebike
  
  #classical bike
  a_rho_a_km_a_classical = tab$age*tab$km_y_a*(1-tab$rho_a) #sum of km weighted by age
  rho_a_km_a_classical = tab$km_y_a*(1-tab$rho_a) # sum of km classical bike
  
  # mean ages
  age_mean_ebike = sum(a_rho_a_km_a_ebike)/sum(rho_a_km_a_ebike) 
  age_mean_classical = sum(a_rho_a_km_a_classical) / sum(rho_a_km_a_classical)
  #age_mean_classical[age_mean_classical==0] = 14 # to avoid the algo set it to zero
  
  delta = age_mean_ebike - age_mean_classical # this is my first optim criteria
  
  # now recalculate the global rho obtaines (ie. prop eBik)
  rho = sum( tab$km_y_a *tab$rho_a) / sum(tab$km_y_a)
  
  # calculate the value to obtimize = distance
  distance = coef_rho*( (rho -obj_rho)^2  /obj_rho) +
    coef_delta *( (delta-obj_delta)^2 / obj_delta )
  
  return(distance)
}







allocate_km_by_age =function(df_demo= INSEE_data, # demographic data frame
                             df_acti= nw_data, # data frame of aggregated active transport volume
                             target_distri=den, # data frame with the target age-distribution of physical activity
                             walk_speed=4.8,
                             cycle_speed = 14,
                             eCycle_speed = 18,
                             obj_delta = 6.7, #targeted age diff btw classical and eBike users
                             coef_delta = 1, #coef to give the relative importance of criteria delta
                             coef_rho=5
){
  
  #####
  # 1) expand df_acti data
  exp_acti = select(df_demo, year, p_tot) %>% 
    right_join(df_acti, by = "year") %>% 
    rename(km_pp_y = value) %>% 
    mutate(total_km_y = p_tot*km_pp_y,
           speed = case_when(type == "walk" ~ walk_speed,
                             type == "cycle" ~ cycle_speed,
                             type == "e_cycle" ~ eCycle_speed),
           minute_pp_w = (60*km_pp_y /speed) / (365.25/7))  # weekly minutes
  
  # 1.1) calculate prop.eBike_year
  prop.eBike_year = data.frame(year = unique(df_acti$year), prop = NA)
  for (i in 1: length(unique(df_acti$year))){
    yy = unique(df_acti$year)[i]
    prop.eBike_year$prop[i]= df_acti[df_acti$type=="e_cycle" & df_acti$year == yy, "value"] / 
      (df_acti[df_acti$type=="tot_cycle" & df_acti$year == yy, "value"])
  }
  
  #####
  #  2) allocates the aggregated volume of walking across ages
  acti =  exp_acti %>% filter(type == "walk")
  target = target_distri %>% filter(type == "walk")
  
  # 2.1) in S1, the scenario assessed, calculate the km_w per person
  S1tab = df_demo %>% filter(sexe == "Both")
  S1tab = S1tab %>% arrange(year)
  S1tab$rho_w = as.numeric(target$rho[match(S1tab$age, target$age)])
  S1tab$total_km_y_w = acti$total_km_y[match(S1tab$year, acti$year)]
  
  # 2.2) creat sum(rho*pop) for each year
  tmp = S1tab %>% group_by(year) %>% 
    mutate(rho_pop = rho_w*pop,
           sum_rho_pop = sum(rho_pop))
  sum_rho_pop = tmp$sum_rho_pop[match(S1tab$year, tmp$year)] ; rm(tmp)       
  S1tab$km_pp_y_walk = S1tab$total_km_y_w*S1tab$rho_w/sum_rho_pop
  
  
  #####
  # 3) allocates the global (ie cycling + eBike) volume of cycling across ages
  acti =  exp_acti %>% filter(type == "tot_cycle")
  target = target_distri %>% filter(type == "cycle")
  
  # 3.1) in S1, the scenario assessed, calculate the km_w per person
  S1tab$rho_bike = as.numeric(target$rho[match(S1tab$age, target$age)])
  S1tab$total_km_y_bike = acti$total_km_y[match(S1tab$year, acti$year)]
  
  # 3.2) creat sum(rho*pop) for each year
  tmp = S1tab %>% group_by(year) %>% 
    mutate(rho_pop = rho_bike*pop,
           sum_rho_pop = sum(rho_pop))
  sum_rho_pop = tmp$sum_rho_pop[match(S1tab$year, tmp$year)] ; rm(tmp)       
  S1tab$km_pp_y_bike = S1tab$total_km_y_bike*S1tab$rho_bike/sum_rho_pop
  
  
  #####
  # 4) allocate km_cycle btw cycle and eBike to allow delta_age
  S1tab$rho_a = NA
  for (i in 1: length(unique(S1tab$year))){
    yy = unique(S1tab$year)[i]
    
    dt = S1tab %>% 
      filter(year == yy) 
    
    obj_rho = as.numeric(prop.eBike_year[prop.eBike_year$year==yy, "prop"])
    
    if(obj_delta == 0){ # if we assume no age difference, then the yearly proportion applies to all ages
      S1tab$rho_a[S1tab$year==yy] = rep(obj_rho, length( S1tab$rho_a[S1tab$year==yy]) )
    } else {
      opt = optim(par = c(0.009, -0.03), fn = optimize_rho_a,
                  tab = dt,
                  obj_delta,
                  obj_rho = obj_rho,
                  coef_delta= coef_delta,
                  coef_rho= coef_rho )
      rho_a = opt$par[1]*dt$age+opt$par[2]
      S1tab$rho_a[S1tab$year==yy] =rho_a
      
    }
  }
  S1tab$km_pp_y_classical = S1tab$km_pp_y_bike*(1-S1tab$rho_a)
  S1tab$km_pp_y_eCycle = S1tab$km_pp_y_bike*S1tab$rho_a
  
  S1tab = S1tab %>% 
    select(age, year, pop, deaths, MR, age_grp, yll, 
           km_pp_y_walk, km_pp_y_classical, km_pp_y_eCycle) %>% 
    pivot_longer(c(km_pp_y_walk, km_pp_y_classical, km_pp_y_eCycle), 
                 names_to = "type",
                 values_to = "km_pp_y") 
  S1tab$type <- factor(S1tab$type, levels=c("km_pp_y_walk","km_pp_y_classical","km_pp_y_eCycle"), 
                       labels=c("Walk","Bike", "E-bike" ))
  
  return(S1tab)
}



impact_all_types = function(df_demo= INSEE_data, # demographic data frame
                            df_acti= nw_data, # data frame of aggregated active transport volume
                            target_distri=den, # data frame with the target age-distribution of physical activity
                            walk_speed=4.8,
                            cycle_speed = 14,
                            eCycle_speed = 18,
                            obj_delta = 6.7, #targeted age diff btw classical and eBike users
                            coef_delta = 1, #coef to give the relative importance of criteria delta
                            coef_rho=5,
                            walk_RR = 0.89,
                            walk_Ref_volume= 168,
                            cycle_RR = 0.90, 
                            cycle_Ref_volume = 100,
                            eCycle_RR= 0.9224138,
                            eCycle_Ref_volume =100,
                            age_min = 20, # minimal age to consider health benefits
                            age_max = 84){
  
  S1tab = allocate_km_by_age  (df_demo= df_demo, # demographic data frame
                               df_acti= df_acti, # data frame of aggregated active transport volume
                               target_distri=target_distri, # data frame with the target age-distribution of physical activity
                               walk_speed=walk_speed,
                               cycle_speed = cycle_speed,
                               eCycle_speed = eCycle_speed,
                               obj_delta = obj_delta, #targeted age diff btw classical and eBike users
                               coef_delta = coef_delta, #coef to give the relative importance of criteria delta
                               coef_rho=coef_rho)
  S1tab = S1tab %>% 
    mutate(speed = case_when(type == "Walk" ~ walk_speed,
                             type == "Bike" ~ cycle_speed,
                             type == "E-bike" ~ eCycle_speed),
           RR = case_when(type == "Walk" ~ walk_RR,
                          type == "Bike" ~ cycle_RR,
                          type == "E-bike" ~ eCycle_RR),
           Ref_volume = case_when(type == "Walk" ~ walk_Ref_volume,
                                  type == "Bike" ~ cycle_Ref_volume,
                                  type == "E-bike" ~ eCycle_Ref_volume)) %>% 
    mutate(minute_pp_w = (60*km_pp_y /speed) / (365.25/7))
  
  ####### 
  # create the reference scenario = 2020 volumes all along
  S0tab = S1tab
  n_rep = nrow(S0tab) / nrow(S1tab[S1tab$year==2021,])
  S0tab$minute_pp_w = rep(S1tab$minute_pp_w[S1tab$year==2021], n_rep) 
  
  ### calculated number prevented
  S1tab$n_prev = n_prev(S1tab)
  S1tab$yll_prev = S1tab$n_prev*S1tab$yll
  
  S0tab$n_prev = n_prev(S0tab)
  S0tab$yll_prev = S0tab$n_prev*S1tab$yll
  
  S1tab$n_prev_wo_S0 = S1tab$n_prev - S0tab$n_prev 
  S1tab$yll_prev_wo_S0 = S1tab$yll_prev - S0tab$yll_prev 
  
  ### apply minimal/maximal age
  S1tab$n_prev[S1tab$age<age_min] = S1tab$n_prev[S1tab$age>=age_max] = 0
  S1tab$yll_prev[S1tab$age<age_min] =  S1tab$yll_prev[S1tab$age>=age_max] =0
  S1tab$n_prev_wo_S0[S1tab$age<age_min] = S1tab$n_prev_wo_S0[S1tab$age>=age_max] =0
  S1tab$yll_prev_wo_S0[S1tab$age<age_min] = S1tab$yll_prev_wo_S0[S1tab$age>=age_max] =0
  S0tab$n_prev[S0tab$age<age_min] = S0tab$n_prev[S0tab$age>=age_max] =0
  S0tab$yll_prev[S0tab$age<age_min] = S0tab$yll_prev[S0tab$age>=age_max] =0
  
  
  # create tables with grouped impact by year
  impact_tot_S1 = S1tab %>% 
    group_by(year, age, MR, pop) %>%  # need to keep MR and pop for life exp calculation
    summarise(n_prev_tot = sum(n_prev),
              yll_prev_tot = sum(yll_prev),
              n_prev_wo_S0_tot = sum(n_prev_wo_S0),
              yll_prev_wo_S0_tot = sum(yll_prev_wo_S0),)
  impact_tot_S0 = S0tab %>% 
    group_by(year, age, MR, pop) %>% 
    summarise(n_prev_tot = sum(n_prev),
              yll_prev_tot = sum(yll_prev))
  
  #calculate life expectancy for each year
  life_exp_gain = NULL
  for (yy in unique(impact_tot_S1$year)){
    tmp = impact_tot_S1%>%  filter(year == yy)
    tmp = tmp[order(tmp$age),]
    tmp$MR[tmp$MR>1] = 1
    tmp$MR[tmp$age==max(tmp$age)] = 1
    
    tmp$MR.S1 =(tmp$MR*tmp$pop - tmp$n_prev_wo_S0_tot)/tmp$pop  # recalculate mortality rates in scenario S1 by discounting deaths prevented
    tmp$MR.S1[tmp$age==max(tmp$age)] = 1
    
    prop_alive_S0 = c(1, cumprod((1 - tmp$MR) ))
    deaths_S0 <- -diff(prop_alive_S0)
    life_exp_S0 = sum(deaths_S0 * 0:(max(tmp$age)) ) 
    
    prop_alive_S1 = c(1, cumprod((1 - tmp$MR.S1) ))
    deaths_S1 <- -diff(prop_alive_S1)
    life_exp_S1 = sum(deaths_S1 * 0:(max(tmp$age)) ) 
    
    diff_exp = life_exp_S1 - life_exp_S0
    
    life_exp_gain = c(life_exp_gain, diff_exp)
    
  }
  life_exp = data.frame(year = unique(impact_tot_S1$year), gain = life_exp_gain)
  
  
  #### return
  li = list(S1 = S1tab, S0 = S0tab, 
            impact_tot_S1=impact_tot_S1, impact_tot_S0=impact_tot_S0,
            life_exp = life_exp)
  
  return(li)
  
}



#########################################
####### functions for plotting the evolution of mileage
#########################################
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
    group_by(age_grp, order, year, type) %>% 
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


######
# for sensitivity analysis

agg_impact_IC = function(df_demo= INSEE_data, # demographic data frame
                         df_acti= nw_data, # data frame of aggregated active transport volume
                         target_distri=den, # data frame with the target age-distribution of physical activity
                         walk_speed=4.8,
                         cycle_speed = 14.9,
                         eCycle_speed = 18.1,
                         obj_delta = 6.7, #targeted age diff btw classical and eBike users
                         coef_delta = 1, #coef to give the relative importance of criteria delta
                         coef_rho=5,
                         walk_RR = 0.89,
                         walk_RR_low = 0.96,
                         walk_RR_sup = 0.83,
                         walk_Ref_volume= 168,
                         cycle_RR = 0.90, 
                         cycle_RR_low = 0.94, 
                         cycle_RR_sup = 0.87, 
                         cycle_Ref_volume = 100,
                         eCycle_RR= 0.91,
                         eCycle_RR_low= 0.946,
                         eCycle_RR_sup= 0.883,
                         eCycle_Ref_volume =100,
                         age_min = 20, # minimal age to consider health benefits
                         age_max = 84,
                         year_output = 2045){
  # function returning aggregated impact estimates + IC
  imp_central = impact_all_types(df_demo, df_acti, target_distri, 
                                 walk_speed, cycle_speed, eCycle_speed, 
                                 obj_delta, coef_delta, coef_rho,
                                 walk_RR, walk_Ref_volume,
                                 cycle_RR, cycle_Ref_volume,
                                 eCycle_RR, eCycle_Ref_volume,
                                 age_min,age_max)
  imp_low = impact_all_types(df_demo, df_acti, target_distri, 
                             walk_speed, cycle_speed, eCycle_speed, 
                             obj_delta, coef_delta, coef_rho,
                             walk_RR = walk_RR_low , walk_Ref_volume,
                             cycle_RR = cycle_RR_low, cycle_Ref_volume,
                             eCycle_RR = eCycle_RR_low, eCycle_Ref_volume,
                             age_min,age_max)
  
  imp_sup = impact_all_types(df_demo, df_acti, target_distri, 
                             walk_speed, cycle_speed, eCycle_speed, 
                             obj_delta, coef_delta, coef_rho,
                             walk_RR = walk_RR_sup , walk_Ref_volume,
                             cycle_RR = cycle_RR_sup, cycle_Ref_volume,
                             eCycle_RR = eCycle_RR_sup, eCycle_Ref_volume,
                             age_min,age_max)
  
  res_per_year =cbind(age = imp_central$impact_tot_S1$age,
                      year = imp_central$impact_tot_S1$year,
                      death_prev = imp_central$impact_tot_S1$n_prev_wo_S0_tot,
                      death_prev_low = imp_low$impact_tot_S1$n_prev_wo_S0_tot,
                      death_prev_sup = imp_sup$impact_tot_S1$n_prev_wo_S0_tot,
                      yll = imp_central$impact_tot_S1$yll_prev_wo_S0_tot,
                      yll_low = imp_low$impact_tot_S1$yll_prev_wo_S0_tot,
                      yll_sup = imp_sup$impact_tot_S1$yll_prev_wo_S0_tot)
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
  
  # deaths prevented
  print(paste0("Annual # death prevented, year=", year_output, ": ",
               round(res_per_year_group$death_prev[res_per_year_group$year==year_output]),
               " [",
               round(res_per_year_group$death_prev_low[res_per_year_group$year==year_output]),
               "-",
               round(res_per_year_group$death_prev_sup[res_per_year_group$year==year_output]),
               "]"
  ))
  print(paste0("Cumulative # death prevented (thousand), 2021-2050: ",
               round(sum(res_per_year_group$death_prev)/1000),
               " [",
               round(sum(res_per_year_group$death_prev_low)/1000),
               "-",
               round(sum(res_per_year_group$death_prev_sup)/1000),
               "]"
  ))
  
  # YLL prevented
  print(paste0("Annual # YLL prevented (thousand), year=", year_output, ": ",
               round(res_per_year_group$yll[res_per_year_group$year==year_output]/1000),
               " [",
               round(res_per_year_group$yll_low[res_per_year_group$year==year_output]/1000),
               "-",
               round(res_per_year_group$yll_sup[res_per_year_group$year==year_output]/1000),
               "]"
  ))
  print(paste0("Cumulative # YLL prevented (million), 2021-2050: ",
               round(sum(res_per_year_group$yll)/1e6, digits = 2),
               " [",
               round(sum(res_per_year_group$yll_low)/1e6, digits = 2),
               "-",
               round(sum(res_per_year_group$yll_sup)/1e6, digits = 2),
               "]"
  ))
  
  # benefits
  print(paste0("Annual health benefits in billion euros, year=", year_output, ": ",
               round(res_per_year_group$euro[res_per_year_group$year==year_output] / 1e9, digits = 1),
               " [",
               round(res_per_year_group$euro_low[res_per_year_group$year==year_output]/ 1e9, digits = 1),
               "-",
               round(res_per_year_group$euro_sup[res_per_year_group$year==year_output]/ 1e9, digits = 1),
               "]"
  ))
  print(paste0("Cumulative  health benefits in billion euros, 2021-2050: ",
               round(sum(res_per_year_group$euro)/1e9),
               " [",
               round(sum(res_per_year_group$euro_low)/1e9),
               "-",
               round(sum(res_per_year_group$euro_sup)/1e9),
               "]"
  ))
  # gain in life exp
  print(paste0("Gain in life expectancy, year=", year_output, ": ",
               round(12*imp_central$life_exp$gain[imp_central$life_exp$year ==year_output ],2),
               " [",
               round(12*imp_low$life_exp$gain[imp_low$life_exp$year ==year_output ],2),
               "-",
               round(12*imp_sup$life_exp$gain[imp_sup$life_exp$year ==year_output ],2),
               "]"
  ))
}