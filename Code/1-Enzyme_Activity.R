## Enzyme analysis

source("code/0-packages.R")
source("code/fticrrr/a-functions_processing.R")


EZ = read.csv("Data/Enzyme.csv")


EZ2<- melt(EZ, id.vars=c("ID","Date","Add","Temp"))


ggplot(EZ2,aes(x = Temp, y = value, fill = Add))+
  geom_bar(stat = "identity")+
  facet_grid(~variable)+
  theme_kp()