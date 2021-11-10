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

download_INSEE = function(scenario = "SP01"){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(readxl,
                 tidyr)
  
  temp <-  tempfile()
  
  if(substr(scenario,1,2) == "SP"){
    id = 2530035 #ID for pop proj
    value = "pop"
  }
  
  else if(substr(scenario,1,2) == "SD") {
    id = 2530048 #ID for mortality
    value = "deaths"
  }
  
  dataURL <- paste0("https://www.insee.fr/fr/statistiques/fichier/",id,"/projpop0550_",scenario,".xls")
  download.file(dataURL, destfile=temp, mode='wb')
  data <- readxl::read_excel(temp, skip = 4, col_names = TRUE)
  
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
                 values_to = value ) 
  
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



