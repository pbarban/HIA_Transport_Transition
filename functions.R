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

n_prev_v2 = function(data){
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





allocate_km_by_age =function(df_demo, # demographic data frame
                             df_acti, # data frame of aggregated active transport volume
                             target_distri, # data frame with the target age-distribution of physical activity
                             type_eval = "cycle" # has to be "walk", "cycle", "e_cycle" or "tot_cycle)
                            ){
  # for a specified transport type, given an aggregated transport volume and a target distribution, 
  # allocates the aggregated volume across ages
  acti =  df_acti %>% filter(type == type_eval)
  
  type_target = ifelse(type_eval %in% c("e_cycle","tot_cycle")  , "cycle", type_eval)# if e_bike or tot_cycle, use the target distrib of classic bike
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
  
 return(S1tab)
  
}



allocate_km_by_age_v2 =function(df_demo= INSEE_data, # demographic data frame
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
  

  
impact_all_types_v2 = function(df_demo= INSEE_data, # demographic data frame
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
  
  S1tab = allocate_km_by_age_v2  (df_demo= df_demo, # demographic data frame
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
  S1tab$n_prev = n_prev_v2(S1tab)
  S1tab$yll_prev = S1tab$n_prev*S1tab$yll
  
  S0tab$n_prev = n_prev_v2(S0tab)
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
                               




impact_per_type = function(df_demo, # demographic data frame
                           df_acti, # data frame of aggregated active transport volume
                           target_distri, # data frame with the target age-distribution of physical activity
                           type_eval = "cycle", # has to be "walk", "cycle", "e_cycle" or "tot_cycle
                           RR = cycle_RR, 
                           Ref_volume = cycle_Ref_volume,
                           speed = cycle_speed,
                           age_min = 20, # minimal age to consider health benefits
                           age_max = 84 # maximal age to consider health benefits
                           ){
  # output a list with 2 demographic tables which calculate for each year and age the number of deaths prevented + yll
  # for a specified transport type
  # requires a demographic dataset, a dataset of physical activity volumes and a target distribution of volumes
  
  
  
  S1tab = allocate_km_by_age(df_demo, # demographic data frame
                             df_acti, # data frame of aggregated active transport volume
                             target_distri, # data frame with the target age-distribution of physical activity
                             type_eval)
  
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
  
  ### apply minimal/maximal age
  S1tab$n_prev[S1tab$age<age_min] = S1tab$n_prev[S1tab$age>=age_max] = 0
  S1tab$yll_prev[S1tab$age<age_min] =  S1tab$yll_prev[S1tab$age>=age_max] =0
  S1tab$n_prev_wo_S0[S1tab$age<age_min] = S1tab$n_prev_wo_S0[S1tab$age>=age_max] =0
  S1tab$yll_prev_wo_S0[S1tab$age<age_min] = S1tab$yll_prev_wo_S0[S1tab$age>=age_max] =0
  S0tab$n_prev[S0tab$age<age_min] = S0tab$n_prev[S0tab$age>=age_max] =0
  S0tab$yll_prev[S0tab$age<age_min] = S0tab$yll_prev[S0tab$age>=age_max] =0
  
  
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
                            eCycle_speed = 18,
                            age_min = 20, # minimal age to consider health benefits
                            age_max = 84){
  
  res_walk = impact_per_type(df_demo = df_demo,
                             df_acti = df_acti,
                             target_distri = target_distri,
                             type_eval = "walk", 
                             RR = walk_RR, 
                             Ref_volume = walk_Ref_volume,
                             speed = walk_speed,
                             age_min, # minimal age to consider health benefits
                             age_max)
  
  res_cycle = impact_per_type(df_demo = df_demo,
                              df_acti = df_acti,
                              target_distri = target_distri,
                              type_eval = "cycle", 
                              RR = cycle_RR, 
                              Ref_volume = cycle_Ref_volume,
                              speed = cycle_speed,
                              age_min, # minimal age to consider health benefits
                              age_max)
  
  res_ecycle = impact_per_type(df_demo = df_demo,
                               df_acti = df_acti,
                               target_distri = target_distri,
                               type_eval = "e_cycle", 
                               RR = eCycle_RR, 
                               Ref_volume = eCycle_Ref_volume,
                               speed = eCycle_speed,
                               age_min, # minimal age to consider health benefits
                               age_max)
  
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



###############
### calculate gain in life expectancy

life.expectancy = function(tot_impact, # an object created by the function >impact_all_types()
                           yy) # the year at which we want to calculate life expectancy
{
  # calculate life expectancy based on an output from >impact_all_types()
  
  dta = tot_impact$tot_S1 
  dta$MR[dta$MR>1] = 1
  dta$MR[dta$age==max(dta$age)] = 1
  
  # keep year yy only
  tmp = dta%>%  filter(year == yy)
  tmp = tmp[order(tmp$age),]
  
  tmp$MR.S1 =(tmp$MR*tmp$pop - tmp$n_prev_wo_S0)/tmp$pop  # recalculate mortality rates in scenario S1 by discounting deaths prevented
  tmp$MR.S1[tmp$age==max(tmp$age)] = 1
  
  prop_alive_S0 = c(1, cumprod((1 - tmp$MR) ))
  deaths_S0 <- -diff(prop_alive_S0)
  life_exp_S0 = sum(deaths_S0 * 0:(max(tmp$age)) ) 
  
  prop_alive_S1 = c(1, cumprod((1 - tmp$MR.S1) ))
  deaths_S1 <- -diff(prop_alive_S1)
  life_exp_S1 = sum(deaths_S1 * 0:(max(tmp$age)) ) 
  
  diff_exp = life_exp_S1 - life_exp_S0
  
  return(data.frame("life_exp_S0" = life_exp_S0, "life_exp_S1"=life_exp_S1, "difference"=diff_exp))
}

