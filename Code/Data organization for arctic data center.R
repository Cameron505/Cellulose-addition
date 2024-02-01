#Organizing data fro the arctic data center.

respiration_processed$Temp<-as.character(respiration_processed$Temp)
  

Data<- enzyme_data %>%
  full_join(MicrobialBiomass_data)%>%
  full_join(respiration_processed) %>%
  select(-T2)

write.csv(Data, "Data/Cellulose_incubation.csv")
