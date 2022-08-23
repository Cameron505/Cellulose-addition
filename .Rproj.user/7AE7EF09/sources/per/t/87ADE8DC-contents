library(doBy)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(reshape2)


cbPalette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
theme_CKM <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.text = element_text(size = 40),
          legend.title = element_text(size = 30),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=1.5, fill = NA),
          
          plot.title = element_text(hjust = 0, size = 50),
          axis.text = element_text(size = 30, color = "black"),
          axis.title = element_text(size = 40, face = "bold", color = "black"),
          
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=40, face="bold"), #facet labels
          strip.text.y = element_text(size=40, face="bold", angle = 270) #facet labels
    )
}

theme_set(theme_pubr())

###KOTZ AIR TEMP DATA (from NOAA National Centers for Environmental Information)
kotz.daily.air.temp <- read.csv("data/Temp/kotz.daily.air.temp.1900.2022.csv") 

#TAVG estimated as mean of TMIN and TMAX for consistency throughout record

kotz.daily.air.temp$DATE <- as.Date(kotz.daily.air.temp$DATE)

kotz.daily.air.temp$YEAR <- year(kotz.daily.air.temp$DATE)

kotz.daily.air.temp$MONTH <- month(kotz.daily.air.temp$DATE)

#calculate sample size
length2= function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}

#calculate monthly mean Kotz air temp
kotz.monthly.air.temp <- summaryBy(data=kotz.daily.air.temp, TAVG~YEAR+MONTH, FUN=c(length2, mean), na.rm=TRUE)

##HYDRIC MET STATION DATA##
hydric.met.station <- read.csv("data/Temp/Hydric Met Station.csv")

#read date as such
hydric.met.station$Date <- as.Date(hydric.met.station$Date)

#calculate daily means
hydric.met.station.daily <- summary_by(data=hydric.met.station, Control_10~Date, FUN=c(length2, mean))

#quick look at the hydric 10 cm soil temp data
ggplot(data=hydric.met.station.daily)+
  geom_line(aes(x=Date, y=Control_10.mean))+
  ylab("Soil Temperature at 10 cm Depth (deg C)")+
  ggtitle("Hydric")

#add columns for year and month
hydric.met.station.daily$YEAR <- year(hydric.met.station.daily$Date)
hydric.met.station.daily$MONTH <- month(hydric.met.station.daily$Date)

#calculate monthly means
hydric.met.station.monthly <- summary_by(data=hydric.met.station.daily, Control_10.mean~YEAR+MONTH, FUN=c(length2, mean))

#grab only complete months
hydric.met.station.monthly <- subset(hydric.met.station.monthly, Control_10.mean.length2>27)

#merge monthly hydric 10 cm soil temps with Kotz long-term air temp record
hydric.soil.kotz.air <- merge(hydric.met.station.monthly, kotz.monthly.air.temp, by=c("YEAR", "MONTH"))

#plots of hydric 10 cm soil versus Kotz air temp
ggplot(data=hydric.soil.kotz.air, aes(x=TAVG.mean, y=Control_10.mean.mean, color=MONTH))+
  geom_point()+
  ylab("Soil Temperature at 10 cm Depth (deg C)")+
  xlab("Kotzebue Air Temperature (deg C)")+
  ggtitle("Hydric")

#looks like there is a clear breakpoint around a Kotz air temp of 5 deg C; double-check when NCEI data are updated
hydric.summer.model <- lm(data=subset(hydric.soil.kotz.air, TAVG.mean>=5), Control_10.mean.mean~TAVG.mean)
summary(hydric.summer.model)

hydric.winter.model <- lm(data=subset(hydric.soil.kotz.air, TAVG.mean<5), Control_10.mean.mean~TAVG.mean)
summary(hydric.winter.model)


##MESIC MET STATION DATA##
mesic.met.station <- read.csv("data/Temp/Mesic Met Station.csv")

#read date as such
mesic.met.station$Date <- as.Date(mesic.met.station$Date)

#calculate daily means
mesic.met.station.daily <- summary_by(data=mesic.met.station, Control_10~Date, FUN=c(length2, mean), na.rm=TRUE)

#quick look at the mesic 10 cm soil temp data
ggplot(data=mesic.met.station.daily)+
  geom_line(aes(x=Date, y=Control_10.mean))+
  ylab("Soil Temperature at 10 cm Depth (deg C)")+
  ggtitle("Mesic")

#add columns for year and month
mesic.met.station.daily$YEAR <- year(mesic.met.station.daily$Date)
mesic.met.station.daily$MONTH <- month(mesic.met.station.daily$Date)

#calculate monthly means
mesic.met.station.monthly <- summary_by(data=mesic.met.station.daily, Control_10.mean~YEAR+MONTH, FUN=c(length2, mean))

#grab only complete months
mesic.met.station.monthly <- subset(mesic.met.station.monthly, Control_10.mean.length2>27)

#merge monthly mesic 10 cm soil temps with Kotz long-term air temp record
mesic.soil.kotz.air <- merge(mesic.met.station.monthly, kotz.monthly.air.temp, by=c("YEAR", "MONTH"))

#plots of mesic 10 cm soil versus Kotz air temp
ggplot(data=mesic.soil.kotz.air, aes(x=TAVG.mean, y=Control_10.mean.mean, color=MONTH))+
  geom_point()+
  ylab("Soil Temperature at 10 cm Depth (deg C)")+
  xlab("Kotzebue Air Temperature (deg C)")+
  ggtitle("Mesic")

#mesic models
mesic.summer.model <- lm(data=subset(mesic.soil.kotz.air, TAVG.mean>=5), Control_10.mean.mean~TAVG.mean)
summary(mesic.summer.model)

mesic.winter.model <- lm(data=subset(mesic.soil.kotz.air, TAVG.mean<5), Control_10.mean.mean~TAVG.mean)
summary(mesic.winter.model)


##XERIC MET STATION DATA##
xeric.met.station <- read.csv("data/Temp/Xeric Met Station.csv")

#read date as such
xeric.met.station$Date <- as.Date(xeric.met.station$Date)

#calculate daily means
xeric.met.station.daily <- summary_by(data=xeric.met.station, Control_10~Date, FUN=c(length2, mean), na.rm=TRUE)

#quick look at the xeric 10 cm soil temp data
ggplot(data=xeric.met.station.daily)+
  geom_line(aes(x=Date, y=Control_10.mean))+
  ylab("Soil Temperature at 10 cm Depth (deg C)")+
  ggtitle("Xeric")

#add columns for year and month
xeric.met.station.daily$YEAR <- year(xeric.met.station.daily$Date)
xeric.met.station.daily$MONTH <- month(xeric.met.station.daily$Date)

#calculate monthly means
xeric.met.station.monthly <- summary_by(data=xeric.met.station.daily, Control_10.mean~YEAR+MONTH, FUN=c(length2, mean))

#grab only complete months
xeric.met.station.monthly <- subset(xeric.met.station.monthly, Control_10.mean.length2>27)

#merge monthly xeric 10 cm soil temps with Kotz long-term air temp record
xeric.soil.kotz.air <- merge(xeric.met.station.monthly, kotz.monthly.air.temp, by=c("YEAR", "MONTH"))

#plots of xeric 10 cm soil versus Kotz air temp
ggplot(data=xeric.soil.kotz.air, aes(x=TAVG.mean, y=Control_10.mean.mean, color=MONTH))+
  geom_point()+
  ylab("Soil Temperature at 10 cm Depth (deg C)")+
  xlab("Kotzebue Air Temperature (deg C)")+
  ggtitle("Xeric")

#xeric models
xeric.summer.model <- lm(data=subset(xeric.soil.kotz.air, TAVG.mean>=5), Control_10.mean.mean~TAVG.mean)
summary(xeric.summer.model)

xeric.winter.model <- lm(data=subset(xeric.soil.kotz.air, TAVG.mean<5), Control_10.mean.mean~TAVG.mean)
summary(xeric.winter.model)

#add soil temperature predictions to Kotz air temperature dataframe
kotz.monthly.air.temp$Xeric.Soil <- ifelse(kotz.monthly.air.temp$TAVG.mean>=5, predict(xeric.summer.model, kotz.monthly.air.temp), 
                                                                                       predict(xeric.winter.model, kotz.monthly.air.temp))

kotz.monthly.air.temp$Mesic.Soil <- ifelse(kotz.monthly.air.temp$TAVG.mean>=5, predict(mesic.summer.model, kotz.monthly.air.temp), 
                                           predict(mesic.winter.model, kotz.monthly.air.temp))

kotz.monthly.air.temp$Hydric.Soil <- ifelse(kotz.monthly.air.temp$TAVG.mean>=5, predict(hydric.summer.model, kotz.monthly.air.temp), 
                                           predict(hydric.winter.model, kotz.monthly.air.temp))
  
  
#plots of three sites temperatures

EZ<- melt(kotz.monthly.air.temp, id.vars=c("YEAR","MONTH","TAVG.length2"))

EZ2<- EZ%>%
  mutate(decade = floor(YEAR/10)*10) %>%
  group_by(MONTH, decade, variable)
EZ2$decade<- as.character(EZ2$decade)

EZ3 = EZ2 %>%
  filter(variable== "Xeric.Soil" | variable== "Mesic.Soil" |variable== "Hydric.Soil", decade >= 1950) %>%
  mutate(MONTH2 = month.abb[MONTH])
EZ3$MONTH2<-factor(EZ3$MONTH2, levels = month.abb)

ET<-ggplot(EZ3, aes(x=MONTH2, y=value, fill=decade))+
  geom_bar(stat='identity',position='dodge')+
  ylab(expression(paste("10 cm estimated Soil Temp.(",degree,C,')')))+
  xlab("Month")+
  ggtitle("")+
  scale_fill_manual(values=cbPalette)+
  ggtitle("Historic soil temperature estimates")+
  guides(fill=guide_legend(title="Decade"))+
  geom_hline(yintercept=10, linetype="dashed", color = "red", size=1)+
  geom_hline(yintercept=6, linetype="dashed", color = "red", size=1)+
  geom_hline(yintercept=2, linetype="dashed", color = "red", size=1)+
  geom_hline(yintercept=-2, linetype="dashed", color = "red", size=1)+
  geom_hline(yintercept=-6, linetype="dashed", color = "red", size=1)+
  geom_hline(yintercept=-10, linetype="dashed", color = "red", size=1)+
  theme_CKM()

ggsave("Graphs/Estimated soil temps.png", plot=ET, width= 20, height= 12)

DF<- read.csv("DF.csv")
DF$time<- as.character(DF$time)
DF$abs<- as.character(DF$abs)
DF$conc<- as.character(DF$conc)
ggplot(DF, aes(x=time, y=abs, color= conc))+
  geom_line()
