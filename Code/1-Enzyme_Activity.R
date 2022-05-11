## Enzyme analysis

source("code/0-packages.R")
#source("code/fticrrr/a-functions_processing.R")


EZ = read.csv("Data/Enzyme.csv")


EZ2<- melt(EZ, id.vars=c("ID","Date","Add","Temp"))
EZ2$Temp<- factor(EZ2$Temp, levels= c("-6","-2","2","6","10","4"))

EZERROR = EZ2 %>%
  group_by(Temp, variable) %>%
  summarize(mean_value = mean(value, na.rm = TRUE),
            se = sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
  ungroup()

ggplot(EZERROR,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 1, width = 1, color="black")+
  facet_wrap(~ variable, ncol = 3,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  theme_kp()
  



ggsave("Graphs/Cellulose_Enzymes.png", device= "png")
