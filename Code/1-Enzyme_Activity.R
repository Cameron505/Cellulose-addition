## Enzyme analysis

source("code/0-packages.R")
#source("code/fticrrr/a-functions_processing.R")


EZ = read.csv("Data/Enzyme.csv")
EZ$EndoC <- EZ$EndoC * 50  #current units are in per mL. making more comparable to other enzymes.
EZ$EndoX <- EZ$EndoX * 50  #current units are in per mL. making more comparable to other enzymes.
EZ$CN<-EZ$BG/(EZ$LAP+EZ$NAG)
EZ2<- melt(EZ, id.vars=c("ID","Date","Add","Temp"))
EZ2$Temp[EZ2$Temp=="4"]<-"Pre"
EZ2$Temp<- factor(EZ2$Temp, levels= c("-6","-2","2","6","10","Pre"))
EZ$alpha <- (EZ$EndoC+EZ$EndoX)/(EZ$EndoC+EZ$EndoX+EZ$BG+EZ$BX)
EZ$alpha1 <- (EZ$EndoC)/(EZ$EndoC+EZ$BG)
EZ$alpha2 <- (EZ$EndoX)/(EZ$EndoX+EZ$BX)
EZ$Temp[EZ$Temp=="4"]<-"Pre"
EZ$Temp<- factor(EZ$Temp, levels= c("-6","-2","2","6","10","Pre"))
EZalpha<- melt(EZ, id.vars=c("ID","Date","Add","Temp"))


##### Without additions
EZERROR2 = EZ2 %>%
  group_by(Temp, variable) %>%
  summarize(mean_value = mean(value, na.rm = TRUE),
            se = sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
  ungroup()

ggplot(EZERROR2,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 1, width = 1, color="black")+
  facet_wrap(~ variable, ncol = 3,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  theme_CKM()
  
ggsave("Graphs/Cellulose_Enzymes.png", device= "png")


EZCARBON = EZERROR2 %>%
  filter(variable== "BG" | variable== "BX" |variable== "EndoC" |variable== "EndoX")

ggplot(EZCARBON,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 0.8, width = 0.7, color="black")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("Enzyme activity per g dry soil")+
  theme_CKM()
ggsave("Graphs/Carbon_Enzymes.png")

EZNUT = EZERROR2 %>%
  filter(variable== "LAP" | variable== "NAG" |variable== "PHOS" |variable== "AFS")

ggplot(EZNUT,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 1, width = 1, color="black")+
  facet_wrap(~ variable, ncol = 3,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  theme_CKM()

ggsave("Graphs/NUT_MISC_Enzymes.png")

EZCBH = EZERROR2 %>%
  filter(variable== "CBH")

ggplot(EZCBH,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 0.8, width = 0.7, color="black")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("Enzyme activity per g dry soil")+
  theme_CKM()
ggsave("Graphs/CBH_Enzymes.png")
 
EZ[EZ < 0] <- 0.1
## Stats
ANOVBG = 
  adonis(EZ %>% dplyr::select(BG)~ Add * Temp , data = EZ) 
ANOVBX = 
  adonis(EZ %>% dplyr::select(BX)~ Add * Temp , data = EZ) 
ANOVENDC = 
  adonis(EZ %>% dplyr::select(EndoC)~ Add * Temp , data = EZ) 
ANOVENDX = 
  adonis(EZ %>% dplyr::select(EndoX)~ Add * Temp , data = EZ) 
ANOVLAP = 
  adonis(EZ %>% dplyr::select(LAP)~ Add * Temp , data = EZ) 
ANOVCBH = 
  adonis(EZ %>% dplyr::select(CBH)~ Add * Temp , data = EZ) 
ANOVNAG = 
  adonis(EZ %>% dplyr::select(NAG)~ Add * Temp , data = EZ) 
ANOVPHOS = 
  adonis(EZ %>% dplyr::select(PHOS)~ Add * Temp , data = EZ) 
ANOVAFS = 
  adonis(EZ %>% dplyr::select(AFS)~ Add * Temp , data = EZ) 

ANOVBG
ANOVBX
ANOVENDC
ANOVENDX
ANOVCBH
ANOVAFS
ANOVLAP
ANOVNAG
ANOVPHOS


##Attempting to add stats to graphs in ggplot without calculating outside. 
EZ3 <- EZ2 %>%
  filter(variable== "LAP" | variable== "NAG" |variable== "PHOS" |variable== "AFS")

ggplot(EZ3,aes(x = Temp, y = value, fill=Temp))+
  stat_summary(geom = "bar")+
  stat_summary(geom="errorbar")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  #stat_compare_means(method = "anova",label = "p.signif",  label.y = c(140,1000,200,1200))+
  theme_CKM()




EZCN = EZERROR2 %>%
  filter(variable== "CN")

ggplot(EZCN,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 0.8, width = 0.7, color="black")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("BG/(LAP+NAG)")+
  theme_CKM()
ggsave("Graphs/Carbon_Enzymes.png")




##### With additions
EZERROR2 = EZ2 %>%
  group_by(Temp, variable, Add) %>%
  summarize(mean_value = mean(value, na.rm = TRUE),
            se = sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
  ungroup()

ggplot(EZERROR2,aes(x = Temp, y = mean_value, fill=Add))+
  geom_bar(stat = "identity", position="dodge")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 1, width = 1, color="black", position="dodge")+
  facet_wrap(~ variable, ncol = 3,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  theme_CKM()

ggsave("Graphs/Cellulose_Enzymes_ADD.png", device= "png")


EZCARBON = EZERROR2 %>%
  filter(variable== "BG" | variable== "BX" |variable== "EndoC" |variable== "EndoX")

ggplot(EZCARBON,aes(x = Temp, y = mean_value, fill=Add))+
  geom_bar(stat = "identity", position="dodge")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 0.8, width = 0.7, color="black", position="dodge")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("Enzyme activity per g dry soil")+
  theme_CKM()
ggsave("Graphs/Carbon_Enzymes_ADD.png")

EZNUT = EZERROR2 %>%
  filter(variable== "LAP" | variable== "NAG" |variable== "PHOS" |variable== "AFS")

ggplot(EZNUT,aes(x = Temp, y = value, fill=Add))+
  geom_bar(stat = "identity", position="dodge")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 1, width = 1, color="black", position="dodge")+
  facet_wrap(~ variable, ncol = 3,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  theme_CKM()

ggsave("Graphs/NUT_MISC_Enzymes_ADD.png")

EZCBH = EZERROR2 %>%
  filter(variable== "CBH")

ggplot(EZCBH,aes(x = Temp, y = mean_value, fill=Add))+
  geom_bar(stat = "identity", position="dodge")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 0.8, width = 0.7, color="black", position="dodge")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("Enzyme activity per g dry soil")+
  theme_CKM()
ggsave("Graphs/CBH_Enzymes_ADD.png")

EZ[EZ < 0] <- 0.1
## Stats
ANOVBG = 
  adonis(EZ %>% dplyr::select(BG)~ Add * Temp , data = EZ) 
ANOVBX = 
  adonis(EZ %>% dplyr::select(BX)~ Add * Temp , data = EZ) 
ANOVENDC = 
  adonis(EZ %>% dplyr::select(EndoC)~ Add * Temp , data = EZ) 
ANOVENDX = 
  adonis(EZ %>% dplyr::select(EndoX)~ Add * Temp , data = EZ) 
ANOVLAP = 
  adonis(EZ %>% dplyr::select(LAP)~ Add * Temp , data = EZ) 
ANOVCBH = 
  adonis(EZ %>% dplyr::select(CBH)~ Add * Temp , data = EZ) 
ANOVNAG = 
  adonis(EZ %>% dplyr::select(NAG)~ Add * Temp , data = EZ) 
ANOVPHOS = 
  adonis(EZ %>% dplyr::select(PHOS)~ Add * Temp , data = EZ) 
ANOVAFS = 
  adonis(EZ %>% dplyr::select(AFS)~ Add * Temp , data = EZ) 

ANOVBG
ANOVBX
ANOVENDC
ANOVENDX
ANOVCBH
ANOVAFS
ANOVLAP
ANOVNAG
ANOVPHOS


##Attempting to add stats to graphs in ggplot without calculating outside. 
EZ3 <- EZ2 %>%
  filter(variable== "LAP" | variable== "NAG" |variable== "PHOS" |variable== "AFS")

ggplot(EZ3,aes(x = Temp, y = value, fill=Add))+
  stat_summary(geom = "bar")+
  stat_summary(geom="errorbar")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  #stat_compare_means(method = "anova",label = "p.signif",  label.y = c(140,1000,200,1200))+
  theme_CKM()




EZCN = EZERROR2 %>%
  filter(variable== "CN")

ggplot(EZCN,aes(x = Temp, y = mean_value, fill=Add))+
  geom_bar(stat = "identity", position="dodge")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 0.8, width = 0.7, color="black", position="dodge")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("BG/(LAP+NAG)")+
  theme_CKM()
ggsave("Graphs/CNratio_Enzymes.png")



## Temperature sensativity
EZA = EZalpha %>%
  filter(variable== "alpha1")
AlphaERROR = EZA %>%
  group_by(Temp,Add) %>%
  summarize(mean_value = mean(value, na.rm = TRUE),
            se = sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
  ungroup()

ggplot(EZ,aes(x = Temp, y = alpha, fill=Temp))+
  stat_summary(geom="bar")+
  stat_summary(geom='errorbar')+
  #facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("alpha")+
  theme_CKM()

ggplot(EZ,aes(x = Temp, y = alpha1, fill=Temp))+
  stat_summary(geom="bar")+
  stat_summary(geom='errorbar')+
  #facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("alpha1")+
  theme_CKM()

ggplot(EZ,aes(x = Temp, y = alpha2, fill=Temp))+
  stat_summary(geom="bar")+
  stat_summary(geom='errorbar')+
  #facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("alpha2")+
  theme_CKM()











EZCARBON5 = EZERROR2 %>%
  filter(variable== "BG" | variable== "BX")

ggplot(EZCARBON5,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 0.8, width = 0.7, color="black")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("Enzyme activity per g dry soil")+
  theme_CKM()
ggsave("Graphs/ExoCarbon_Enzymes.png")



EZCARBON6 = EZERROR2 %>%
  filter(variable== "EndoC" |variable== "EndoX")

ggplot(EZCARBON6,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 0.8, width = 0.7, color="black")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("Enzyme activity per g dry soil")+
  theme_CKM()
ggsave("Graphs/Carbon_Enzymes.png")
