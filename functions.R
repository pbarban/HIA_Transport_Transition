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
life_exp = function(df, year,sexe, MR ){
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

