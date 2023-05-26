
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
    filter(Temp !="4", ID!='C10-2',ID!='D10-3',ID!='E10-4',ID!='AN2-4',ID!='EN2-4')%>%   #Removing pre-incubation measurements, and extreme outliers. 
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
           EndoX=ifelse(EndoX < 0, yes = 0, no = EndoX),
           alpha= (EndoC+EndoX)/(EndoC+EndoX+BG+BX),
           alphaEC= (EndoC/(EndoC+BG)),
            alphaEX= (EndoX/(EndoX+BX)),
           CN=BG/(LAP+NAG)
           )
}

process_KotzTemp = function(KotzTemp_data){
  KotzTemp_data2 = KotzTemp_data %>%
    mutate(DATE=as.Date(DATE),
           YEAR=year(DATE),
           MONTH=month(DATE)
    )
  Kotz_proccessed=summaryBy(data=KotzTemp_data2, TAVG~YEAR+MONTH, FUN=c(length2, mean), na.rm=TRUE)
}

process_SoilTemp = function(SoilTempHydric_data,SoilTempMesic_data,SoilTempXeric_data,Kotz_proccessed){
  
  hydric.met.station.daily <- summary_by(data=SoilTempHydric_data, Control_10~Date, FUN=c(length2, mean))
  SoilTempHydric_data2 = hydric.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date)
    )
  
  hydric.met.station.monthly <- summary_by(data=SoilTempHydric_data2, Control_10.mean~YEAR+MONTH, FUN=c(length2, mean))
  HydricSoilMonthly_Processed <- subset(hydric.met.station.monthly, Control_10.mean.length2>27)
  hydric.soil.kotz.air <- merge(HydricSoilMonthly_Processed, Kotz_proccessed, by=c("YEAR", "MONTH"))
  hydric.summer.model <- lm(data=subset(hydric.soil.kotz.air, TAVG.mean>=5), Control_10.mean.mean~TAVG.mean)
  hydric.winter.model <- lm(data=subset(hydric.soil.kotz.air, TAVG.mean<5), Control_10.mean.mean~TAVG.mean)
  Kotz_proccessed_H=Kotz_proccessed
  
  Kotz_proccessed_H$Hydric.Soil <- ifelse(Kotz_proccessed$TAVG.mean>=5, predict(hydric.summer.model, Kotz_proccessed), 
                                        predict(hydric.winter.model, Kotz_proccessed))

  
  
  Mesic.met.station.daily <- summary_by(data=SoilTempMesic_data, Control_10~Date, FUN=c(length2, mean))
  SoilTempMesic_data2 = Mesic.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date)
    )
  
  Mesic.met.station.monthly <- summary_by(data=SoilTempMesic_data2, Control_10.mean~YEAR+MONTH, FUN=c(length2, mean))
  MesicSoilMonthly_Processed <- subset(Mesic.met.station.monthly, Control_10.mean.length2>27)
  Mesic.soil.kotz.air <- merge(MesicSoilMonthly_Processed, Kotz_proccessed_H, by=c("YEAR", "MONTH"))
  Mesic.summer.model <- lm(data=subset(Mesic.soil.kotz.air, TAVG.mean>=5), Control_10.mean.mean~TAVG.mean)
  Mesic.winter.model <- lm(data=subset(Mesic.soil.kotz.air, TAVG.mean<5), Control_10.mean.mean~TAVG.mean)
  Kotz_proccessed_HM<-Kotz_proccessed_H
  Kotz_proccessed_HM$Mesic.Soil <- ifelse(Kotz_proccessed_H$TAVG.mean>=5, predict(Mesic.summer.model, Kotz_proccessed_H), 
                                          predict(Mesic.winter.model, Kotz_proccessed_H))
  
  
  Xeric.met.station.daily <- summary_by(data=SoilTempXeric_data, Control_10~Date, FUN=c(length2, mean))
  SoilTempXeric_data2 = Xeric.met.station.daily %>%
    mutate(DATE=as.Date(Date),
           YEAR=year(Date),
           MONTH=month(Date)
    )
  
  Xeric.met.station.monthly <- summary_by(data=SoilTempXeric_data2, Control_10.mean~YEAR+MONTH, FUN=c(length2, mean))
  XericSoilMonthly_Processed <- subset(Xeric.met.station.monthly, Control_10.mean.length2>27)
  Xeric.soil.kotz.air <- merge(XericSoilMonthly_Processed, Kotz_proccessed, by=c("YEAR", "MONTH"))
  Xeric.summer.model <- lm(data=subset(Xeric.soil.kotz.air, TAVG.mean>=5), Control_10.mean.mean~TAVG.mean)
  Xeric.winter.model <- lm(data=subset(Xeric.soil.kotz.air, TAVG.mean<5), Control_10.mean.mean~TAVG.mean)
  Kotz_proccessed_HMX<-Kotz_proccessed_HM
  Kotz_proccessed_HMX$Xeric.Soil <- ifelse(Kotz_proccessed_HM$TAVG.mean>=5, predict(Xeric.summer.model, Kotz_proccessed_HM), 
                                           predict(Xeric.winter.model, Kotz_proccessed_HM))
 
  return(Kotz_proccessed_HMX)
  
   }

