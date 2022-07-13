
source("code/0-packages.R")
A = read.csv("Data/RES2.csv")
A$Date<- as.Date(A$Date, format="%m/%d/%Y")
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
A<-A[A$ID!='C10-2',]
A<-A[A$ID!='D10-3',]
A<-A[A$ID!='E10-4',]
A<-A[A$ID!='AN2-4',]
A<-A[A$ID!='EN2-4',]
A$csum <- ave(A$Res.2, A$ID, FUN=cumsum)
NGA<-A[A$Temp<=0,]
AA<-A[A$Add!="",]
AA<-AA[AA$Temp>=0,]

PRes<-ggplot(AA, aes(x=Date, y=Res.2, color=Add))+
  stat_summary(fun.y = mean,geom = "point",lwd=4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", lwd=1, width = 0.5)+
  facet_wrap(~Temp)+
  theme_light()+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  ylab(expression(paste( " ",mu,"g-C",g^-1," Dry Soil ",Hr^-1)))+
  xlab("Date")+
  theme_CKM()+
  ggtitle("Soil Respiration (above freezing)")

PRes1<-ggplot(NGA, aes(x=Date, y=Res.2, color=Add))+
  stat_summary(fun.y = mean,geom = "point",lwd=4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", lwd=1, width = 0.5)+
  facet_wrap(~Temp)+
  theme_light()+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  ylab(expression(paste( " ",mu,"g-C",g^-1," Dry Soil ",Hr^-1)))+
  xlab("Date")+
  theme_CKM()+
  ggtitle("Soil Respiration (Below freezing)")

PResC<-ggplot(AA, aes(x=Date, y=csum, color=Add))+
  stat_summary(fun.y = mean,geom = "point",lwd=4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", lwd=1, width = 0.5)+
  facet_wrap(~Temp)+
  theme_light()+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  ylab(expression(paste( " ",mu,"g-C",g^-1," Dry Soil ")))+
  xlab("Date")+
  theme_CKM()+
  ggtitle("Soil Respiration (above freezing)")

PRes1C<-ggplot(NGA, aes(x=Date, y=csum, color=Add))+
  stat_summary(fun.y = mean,geom = "point",lwd=4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", lwd=1, width = 0.5)+
  facet_wrap(~Temp)+
  theme_light()+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  ylab(expression(paste( " ",mu,"g-C",g^-1," Dry Soil ")))+
  xlab("Date")+
  theme_CKM()+
  ggtitle("Soil Respiration (Below freezing)")


A<-A[A$Temp!="4",]
A$JD<-format(A$Date, "%j")
A$JD<-as.numeric(A$JD)
A$JD<-A$JD-152
PP<-ggplot(A, aes(x=JD, y=csum, color=Add))+
  stat_summary(fun.y = mean,geom = "point",lwd=4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", lwd=1, width = 0.5)+
  facet_wrap(~Temp, scales= "free")+
  theme_light()+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  ylab(expression(paste( " ",mu,"g-C ",g^-1," Dry Soil ")))+
  xlab("incubation day")+
  theme_CKM()+
  ggtitle("Cumulative Soil Respiration")







ggsave('cellulose addition above freezing.png', plot=PRes, width= 20, height= 12)
ggsave('cellulose addition below freezing.png', plot=PRes1, width= 20, height= 12)
ggsave('Cumulative cellulose addition above freezing.png', plot=PResC, width= 20, height= 12)
ggsave('Cumulative cellulose addition below freezing.png', plot=PRes1C, width= 20, height= 12)
ggsave('Cumulative cellulose addition.png', plot=PP, width= 20, height= 12)
