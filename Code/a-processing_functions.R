
import_respiration= function(FILEPATH){
  # import data file
  filePaths_respiration <- list.files(FILEPATH, pattern = "csv", full.names = TRUE, recursive = TRUE)
  respiration_data <- read.csv(FILEPATH, header = TRUE) %>% mutate(Date = lubridate::mdy(Date), pre.inc = as.factor(pre.inc) ,
    Inc.temp = as.factor(Inc.temp)) %>% janitor::clean_names()
  respiration_data = respiration_data %>% mutate(source = basename(FILEPATH))
  respiration_data
  list(respiration_data = respiration_data)
}

process_respiration = function(respiration_data){
  respiration_processed = respiration_data %>%
    mutate(Datemdy = lubridate::mdy(Date),
           JD = strftime(Datemdy, format = "%j"),
           JD2 = as.numeric(JD)-152) %>%
    filter(Temp !="4")%>%
    group_by(ID,Temp,Add) %>%
    dplyr::summarise(val=cumtrapz(JD2,Res),Res, JD2)

}

import_nutrients= function(FILEPATH){
  # import data file
  filePaths_nutrients <- list.files(FILEPATH, pattern = "csv", full.names = TRUE, recursive = TRUE)
  nutrients_data <- read.csv(FILEPATH, header = TRUE) %>% mutate(Date = lubridate::mdy(Date)) %>% janitor::clean_names()
  nutrients_data = nutrients_data %>% mutate(source = basename(FILEPATH))
  nutrients_data
  list(nutrient_data = nutrient_data)
}

process_enzyme = function(enzyme_data){
  enzyme_processed = enzyme_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10","Pre")),
           EndoC=EndoC*50,
           EndoX=EndoX*50,
           alpha= (EndoC+EndoX)/(EndoC+EndoX+BG+BX),
           alpha1= EndoC/(EndoC+BG),
           alpha2= EndoX/(EndoX+BX),
           CN=BG/(LAP+NAG)
           )
  
}
