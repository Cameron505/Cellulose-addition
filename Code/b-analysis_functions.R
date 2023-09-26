plot_respiration = function(respiration_processed){
  
  gg_res =
    respiration_processed %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10"))) %>%
    ggplot(aes(x = JD2, y = Res, color = Add))+
    geom_point(position = position_dodge(width = 0.4),
               size = 2)+
    ylab(expression(paste( "Respiration (",mu,"g-C",day^-1, ")")))+
    facet_wrap(~Temp, scale="free")+
    #theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    ylab(expression(paste( "Respiration (",mu,"g-C",day^-1, ")")))+
    xlab("incubation day")+
    labs(color='Addition') +
    ggtitle("Soil Respiration")+
    theme_CKM2()
  
  gg_cumres =
    respiration_processed %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10"))) %>%
    ggplot(aes(x = JD2, y = val))+
    geom_point(aes(color =  Add, shape=Add),position = position_dodge(width = 0.4),
               size = 2)+
    ylab(expression(paste( "Respiration (",mu,"g-C)")))+
    facet_wrap(~Temp, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    ylab(expression(paste( "Respiration (",mu,"g-C)")))+
    xlab("incubation day")+
    labs(shape = 'Addition', color = 'Addition')+
    ggtitle("Cumulative Soil Respiration")+
    theme_CKM2()
  
  gg_Avgres =
    respiration_processed %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10"))) %>%
    ggplot(aes(x=JD2, y=Res, color=Add))+
    stat_summary(fun = mean,geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~Temp, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    ylab(expression(paste( "Respiration (",mu,"g-C",day^-1, ")")))+
    xlab("incubation day")+
    labs(color='Addition') +
    ggtitle("Average Soil Respiration")+
    theme_CKM2()
  
  
  R.label<- c("-6 °C","-2 °C","2 °C","6 °C","10 °C")
  names(R.label)<- c("-6","-2","2","6","10")
  
  
  gg_Avgcumres =
    respiration_processed %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10"))) %>%
    ggplot(aes(x=JD2, y=val,color=Add, shape=Add))+
    stat_summary(aes(color=Add, shape=Add),fun = mean,geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~Temp, scale="free", labeller = labeller(Temp=R.label))+
    labs(shape = 'Addition', color = 'Addition')+
    theme_light()+
    scale_color_manual(values=cbPalette)+
    ylab(expression(paste( "Respiration (",mu,"g-C)")))+
    xlab("Incubation day")+
    ggtitle("Cumulative Soil Respiration")+
    theme_CKM3()
  
  
  A<- respiration_processed%>%
    group_by(Temp,Add)%>%
    filter(JD2==max(JD2))%>%
    summarise(mean(val),sd(val))
    
  a = nlme::lme(Res ~ Temp + Add + JD2,
                random = ~1|ID,
                data = respiration_processed)
  
  aanova<-anova(a) %>%
    knitr::kable("simple")
  
  
  respiration_processed[c('ID2','rep')]<-str_split_fixed(respiration_processed$ID,'-',2)
  
  PD<- respiration_processed%>%
    group_by(Temp,Add,rep)%>%
    filter(JD2==max(JD2))%>%
    select(-c(ID2,Res,ID))%>%
    pivot_wider(names_from=Add, values_from = val)
  PD2<- respiration_processed%>%
    group_by(Temp,Add)%>%
    filter(JD2==max(JD2),Add==c("Cellobiose"))%>%
    pivot_wider(names_from=Add, values_from = val)
  
  PDT<-PD%>%
    mutate(PC= (None/(Cellobiose))*100,PT= (None/(Cellotetraose))*100,P90= (None/(`Cellulose- 90`))*100,P20= (None/(`Cellulose- 20`))*100,PCC= (None/(`Coloidal cellulose`))*100)%>%
    pivot_longer(cols= PC:PCC,
                 names_to= "compare",
                 values_to= "Percent")
  
  
  
 
  
  
  
  
  
  list("Respiration" = gg_res,
       "Average Respiration" = gg_Avgres,
       "Cumulative Respiration" = gg_cumres,
       "Average Cumulative Respiration" = gg_Avgcumres,
       aanova=aanova)
  
}

plot_nutrients = function(nutrients_data){
  
  nutrients_data_long = nutrients_data %>%
    pivot_longer(cols= NH4:TRS,
                 names_to= "analyte",
                 values_to= "conc")
  
  hsd_label = 
    nutrients_data_long %>% 
    group_by(analyte) %>% 
    do(fit_hsd(.))
  
  hsd_label2 = 
    nutrients_data_long %>% 
    group_by(analyte,Add) %>% 
    do(fit_hsd(.))
  
  
  gg_NH4 =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=NH4, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = hsd_label2 %>% filter(analyte == "NH4"), aes(y = 14, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    labs(color='Add temp') +
    ggtitle("Ammonium")
  
  gg_NO3 =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=NO3, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = hsd_label2 %>% filter(analyte == "NO3"), aes(y = 45, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    labs(color='Add temp') +
    ggtitle("Nitrate")
  
  gg_TFPA =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=TFPA, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = hsd_label2 %>% filter(analyte == "TFPA"), aes(y = 145, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    labs(color='Add temp') +
    ggtitle("TFPA")
  
  gg_TRS =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=TRS, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = hsd_label2 %>% filter(analyte == "TRS"), aes(y = 2.1, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'Mol' ~g^-1 ~ dry ~ soil*')'))+
    labs(color='Add temp') +
    ggtitle("TRS")
  
  gg_PO4 =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=PO4, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = hsd_label2 %>% filter(analyte == "PO4"), aes(y = 0.4, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    labs(color='Add temp') +
    ggtitle("Phosphate")
  
  gg_NH4_2 =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=NH4))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = hsd_label %>% filter(analyte == "NH4"), aes(y = 12.5, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Ammonium ('*mu*'g '*NH[4]^"+"~-N~g^-1 ~ dry ~ soil*')'))+
    labs(color='Add temp') +
    ggtitle("Ammonium")
  
  gg_NO3_2 =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=NO3))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = hsd_label %>% filter(analyte == "NO3"), aes(y = 31, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Nitrate ('*mu*'g '*NO[3]^"-"~-N~g^-1 ~ dry ~ soil*')'))+
    labs(color='Add temp') +
    ggtitle("Nitrate")
  
  gg_TFPA_2 =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=TFPA))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = hsd_label %>% filter(analyte == "TFPA"), aes(y = 120, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Total free primary amines-Leucine equiv. (nMol' ~g^-1 ~ dry ~ soil*')'))+
    labs(color='Add temp') +
    ggtitle("TFPA")
  
  gg_TRS_2 =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=TRS))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = hsd_label %>% filter(analyte == "TRS"), aes(y = 0.76, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Total reducing sugars-glucose equiv. ('*mu*'Mol' ~g^-1 ~ dry ~ soil*')'))+
    labs(color='Add temp') +
    ggtitle("TRS")
  
  gg_PO4_2 =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=PO4))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = hsd_label %>% filter(analyte == "PO4"), aes(y = 0.4, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Phosphate ('*mu*'g '*PO[4]^"3-"~-P~g^-1 ~ dry ~ soil*')'))+
    labs(color='Add temp') +
    ggtitle("Phosphate")
  
  
  list("Ammonium" = gg_NH4_2,
       "Ammonium with addition" = gg_NH4,
       "Nitrate" = gg_NO3_2,
       "Nitrate with addition" = gg_NO3,
       "Total free primary amines" = gg_TFPA_2,
       "Total free primary amines with addition" = gg_TFPA,
       "Phosphate" = gg_PO4_2,
       "Phosphate with addition" = gg_PO4,
       "Total reducing sugars" = gg_TRS_2,
       "Total reducing sugars with addition" = gg_TRS
       
  )
  
}

plot_MicrobialBiomass = function(MicrobialBiomass_data){
  
  
  
  microbialbiomass_data_long = MicrobialBiomass_data %>%
    pivot_longer(cols= TOC:MBN,
                 names_to= "analyte",
                 values_to= "conc")
  
  MB_hsd_label = 
    microbialbiomass_data_long %>% 
    group_by(analyte) %>% 
    do(fit_hsd(.))
  
  MB_hsd_label2 = 
    microbialbiomass_data_long %>% 
    group_by(analyte,Add) %>% 
    do(fit_hsd(.))
  
  gg_MBC =
    MicrobialBiomass_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=MBC, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = MB_hsd_label2 %>% filter(analyte == "MBC"), aes(y = 4500, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    labs(color='Addition') +
    ggtitle("Microbial biomass carbon")
  
  gg_MBN =
    MicrobialBiomass_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=MBN, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = MB_hsd_label2 %>% filter(analyte == "MBN"), aes(y = 400, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
    labs(color='Addition') +
    ggtitle("Microbial biomass Nitrogen")
  
  gg_MBC_2 =
    MicrobialBiomass_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=MBC))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = MB_hsd_label %>% filter(analyte == "MBC"), aes(y = 3300, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Microbial biomass ('*mu*'g C'~g^-1 ~ dry ~ soil*')'))+
    labs(color='Addition') +
    ggtitle("Microbial biomass carbon")
  
  gg_MBN_2 =
    MicrobialBiomass_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=MBN))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = MB_hsd_label %>% filter(analyte == "MBN"), aes(y = 350, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('Microbial biomass ('*mu*'g N'~g^-1 ~ dry ~ soil*')'))+
    labs(color='Addition') +
    ggtitle("Microbial biomass Nitrogen")
  
  list("Microbial biomass carbon" = gg_MBC_2,
       "Microbial biomass carbon by addotion" = gg_MBC,
       "Microbial biomass nitrogen" = gg_MBN_2,
       "Microbial biomass nitrogen by addition" = gg_MBN
       
  )
  
}

plot_enzyme1 = function(enzyme_processed){
  
enzyme_data_long = enzyme_processed %>%
    pivot_longer(cols= BG:CN,
                 names_to= "analyte",
                 values_to= "conc")
  
ENZM_hsd_label = 
    enzyme_data_long %>% 
    group_by(analyte) %>% 
    do(fit_hsd(.))
  
ENZM_hsd_label2 = 
    enzyme_data_long %>% 
    group_by(analyte,Add) %>% 
    do(fit_hsd(.))
  
  
  
  
  gg_BG =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=BG, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "BG"), aes(y = 490, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("β-1,4-glucosidase (BG)")
  
  gg_BX =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=BX, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "BX"), aes(y = 250, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("β-1,4-xylosidase (BX) ")
  
  gg_EndoC =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=EndoC, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "EndoC"), aes(y = 310, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("endo-β-D-1,4-glucanase (EC)")
  
  
  gg_EndoX =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=EndoX, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "EndoX"), aes(y = 200, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("endo-β-1,4xylanase (EX)")
  
 
  gg_CBH =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=CBH, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "CBH"), aes(y = 70, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("cellobiohydrolase (CBH)")
  
  gg_BG_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=BG))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "BG"), aes(y = 490, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("β-1,4-glucosidase (BG)")
  
  gg_BX_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=BX))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "BX"), aes(y = 250, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("β-1,4-xylosidase (BX) ")
  
  gg_EndoC_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=EndoC))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "EndoC"), aes(y = 310, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("endo-β-D-1,4-glucanase (EC)")
  
  gg_EndoX_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=EndoX))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "EndoX"), aes(y = 200, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("endo-β-1,4xylanase (EX)")
  
  gg_CBH_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=CBH))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "CBH"), aes(y = 70, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("cellobiohydrolase (CBH)")
  
  gg_AFS =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=AFS, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "AFS"), aes(y = 180, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("α-L-Arabinofuranosidase (AFS)")
  
  gg_AFS_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=AFS))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "AFS"), aes(y = 180, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("α-L-Arabinofuranosidase (AFS)")
  
  

  gg_BG_3 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=BG))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 position = position_dodge(width = 0.6), 
                 alpha = 0.2,
                 aes(group = interaction(Temp)))+
    geom_point(position = position_dodge(width = 0.6), size = 4)+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "BG"), aes(y = 610, label = label),size = 6, color = "black")+
    #scale_y_continuous(expand=c(0,0),limits=c(0,440))+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("β-1,4-glucosidase (BG)")+
    theme_CKM2()
  
  gg_BX_3 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=BX))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 position = position_dodge(width = 0.6), 
                 alpha = 0.2,
                 aes(group = interaction(Temp)))+
    geom_point(position = position_dodge(width = 0.6), size = 4)+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "BX"), aes(y = 275, label = label),size = 6, color = "black")+
    #scale_y_continuous(expand=c(0,0),limits=c(0,220))+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("β-1,4-xylosidase (BX) ")+
    theme_CKM2()
  
  
  gg_EndoC_3 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=EndoC))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 position = position_dodge(width = 0.6), 
                 alpha = 0.2,
                 aes(group = interaction(Temp)))+
    geom_point(position = position_dodge(width = 0.6), size = 4)+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "EndoC"), aes(y = 365, label = label),size = 6, color = "black")+
    #scale_y_continuous(expand=c(0,0),limits=c(0,440))+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("endo-β-D-1,4-glucanase (EC)")+
    theme_CKM2()
  
  gg_EndoX_3 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=EndoX))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 position = position_dodge(width = 0.6), 
                 alpha = 0.2,
                 aes(group = interaction(Temp)))+
    geom_point(position = position_dodge(width = 0.6), size = 4)+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "EndoX"), aes(y = 235, label = label),size = 6, color = "black")+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("endo-β-1,4xylanase (EX)")+
    theme_CKM2()
  
  
  
  
  
  
  Enzyme_Pre_4<- enzyme_processed%>%
    mutate(Temp=as.character(Temp),
           Temp = replace(Temp, Temp == "Pre", "4"),
           Temp = as.numeric(as.character(Temp)))
  
  
  
  gg_BG5 =
    Enzyme_Pre_4 %>%
    ggplot(aes(x=Temp, y=BG))+
    geom_point(size = 2) +
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    stat_smooth(method= "lm")+
    stat_cor(label.y=c(400), size=10)+
    stat_regline_equation(label.y=c(450), size=10)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("β-1,4-glucosidase (BG)")
  
  gg_BX5 =
    Enzyme_Pre_4 %>%
    ggplot(aes(x=Temp, y=BX))+
    geom_point(size = 2) +
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    stat_smooth(method= "lm")+
    stat_cor(label.y=c(400), size=10)+
    stat_regline_equation(label.y=c(450), size=10)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("β-1,4-xylosidase (BX) ")
  
  gg_EndoC5 =
    Enzyme_Pre_4 %>%
    ggplot(aes(x=Temp, y=EndoC))+
    geom_point(size = 2) +
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    stat_smooth(method= "lm")+
    stat_cor(label.y=c(400), size=10)+
    stat_regline_equation(label.y=c(450), size=10)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("endo-β-D-1,4-glucanase (EC)")
  
  
  gg_EndoX5 =
    Enzyme_Pre_4 %>%
    ggplot(aes(x=Temp, y=EndoX))+
    geom_point(size = 2) +
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    stat_smooth(method= "lm")+
    stat_cor(label.y=c(400), size=10)+
    stat_regline_equation(label.y=c(450), size=10)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("endo-β-1,4xylanase (EX)")
  
  
  
  
  
  
  
  
  
  lm1 = lm(EndoX ~ Temp,data=Enzyme_Pre_4)
  summary(lm1)
  lm2 = lm(EndoC ~ Temp,data=Enzyme_Pre_4)
  summary(lm2)
  lm3 = lm(BG ~ Temp,data=Enzyme_Pre_4)
  summary(lm3)
  lm4 = lm(BX ~ Temp,data=Enzyme_Pre_4)
  summary(lm4)
  
  
  
  
  
  
  
  
 
  
  
  
  gg_Ecombine= plot_grid(
    gg_EndoC_3,
    gg_EndoX_3,
    gg_BG_3,
    gg_BX_3,
    align = 'vh',
    labels = c("A", "B", "C", "D"),
    label_size = 16,
    label_x= 0,
    label_y= 1.03,
    hjust = -1,
    nrow = 2
  )
  title3 <- ggdraw() + draw_label("Enzyme Activity", size=30,fontface='bold')
  gg_EcombineA=plot_grid(title3,gg_Ecombine, ncol = 1, rel_heights = c(0.19,2))
  
  
  gg_Ecombine2= plot_grid(
    gg_EndoC_3,
    gg_EndoX_3,
    align = 'vh',
    labels = c("A", "B"),
    label_size = 14,
    label_x= 0,
    label_y= 1.03,
    hjust = -1,
    nrow = 1
  )
  
  
  
  gg_Ecombine3=plot_grid(title3,gg_Ecombine2, ncol = 1, rel_heights = c(0.1,1))
  title4 <- ggdraw() + draw_label("", size=18,fontface='bold')
  
  gg_Ecombine4= plot_grid(
    gg_BG_3,
    gg_BX_3,
    align = 'vh',
    labels = c("C", "D"),
    label_size = 14,
    label_x= 0,
    label_y= 1.03,
    hjust = -1,
    nrow = 1
  )
  gg_Ecombine5=plot_grid(title4,gg_Ecombine4, ncol = 1, rel_heights = c(0.1,1))
  
  list("BG" = gg_BG_2,
       "BG with addition" = gg_BG,
       "Endo cellulase" = gg_EndoC_2,
       "Endo cellulase with addition" = gg_EndoC,
       "CBH" = gg_CBH_2,
       "CBH with addition" = gg_CBH,
       "BX" = gg_BX_2,
       "BX with addition" = gg_BX,
       "Endo Xylanase" = gg_EndoX_2,
       "Endo xylanase with addition" = gg_EndoX,
       "AFS" = gg_AFS_2,
       "AFS" = gg_AFS,
       gg_Ecombine=gg_Ecombine,
       gg_Ecombine3=gg_Ecombine3,
       gg_Ecombine5=gg_Ecombine5,
       gg_EcombineA=gg_EcombineA
       )
  
}

plot_enzyme2 = function(enzyme_processed){
  
  enzyme_data_long = enzyme_processed %>%
    pivot_longer(cols= BG:CN,
                 names_to= "analyte",
                 values_to= "conc")
  
  ENZM_hsd_label = 
    enzyme_data_long %>% 
    group_by(analyte) %>% 
    do(fit_hsd(.))
  
  ENZM_hsd_label2 = 
    enzyme_data_long %>% 
    group_by(analyte,Add) %>% 
    do(fit_hsd(.))  
  
  
  
  
  
  gg_LAP =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=LAP, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "LAP"), aes(y = 1600, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("leucine aminopeptidase (LAP)")
  
  gg_LAP_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=LAP))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "LAP"), aes(y = 1600, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("leucine aminopeptidase (LAP)")
  
  gg_NAG =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=NAG, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "NAG"), aes(y = 325, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("N-acetyl-β-d-glucosaminidase (NAG)")
  
  gg_NAG_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=NAG))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "NAG"), aes(y = 325, label = label))+
    
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("N-acetyl-β-d-glucosaminidase (NAG)")
  
  gg_PHOS =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=PHOS, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "PHOS"), aes(y = 3000, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("phosphatase (PHOS)")
  
  gg_PHOS_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=PHOS))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "PHOS"), aes(y = 3000, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("phosphatase (PHOS)")
  
  gg_per =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=per, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "per"), aes(y = 35, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature")+
    labs(color='Addition') +
    ggtitle("peroxidase (per)")
  
  gg_per_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=per))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "per"), aes(y = 35, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature")+
    labs(color='Addition') +
    ggtitle("peroxidase (per)")
  
  gg_pox =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=pox, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "pox"), aes(y = 22, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempoxature")+
    labs(color='Addition') +
    ggtitle("phenoloxidase (pox)")
  
  gg_pox_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=pox))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "pox"), aes(y = 22, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempoxature")+
    labs(color='Addition') +
    ggtitle("phenoloxidase (pox)")
  
  gg_netox =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=NETPEROX, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "NETPEROX"), aes(y = 42, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempoxature")+
    labs(color='Addition') +
    ggtitle("Net oxidative")
  
  gg_netox_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=NETPEROX))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "NETPEROX"), aes(y = 42, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempoxature")+
    labs(color='Addition') +
    ggtitle("Net oxidative")
  
 
  
  
  list("LAP" = gg_LAP_2,
       "LAP" = gg_LAP,
       "NAG" = gg_NAG_2,
       "NAG" = gg_NAG,
       "PHOS" = gg_PHOS_2,
       "PHOS" = gg_PHOS,
       "peroxidase" = gg_per_2,
       "peroxidase" = gg_per,
       "phenol oxidase" = gg_pox_2,
       "phenol oxidase" = gg_pox,
       "Net oxidative" = gg_netox_2,
       "Net oxidative" = gg_netox
       
  )
  
}

plot_enzyme3 = function(enzyme_processed){

  
  enzyme_data_long = enzyme_processed %>%
    pivot_longer(cols= BG:CN,
                 names_to= "analyte",
                 values_to= "conc")
  
  ENZM_hsd_label = 
    enzyme_data_long %>% 
    group_by(analyte) %>% 
    do(fit_hsd(.))
  
  ENZM_hsd_label2 = 
    enzyme_data_long %>% 
    group_by(analyte,Add) %>% 
    do(fit_hsd(.))  
  
  gg_CN =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=CN, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "CN"), aes(y = 0.75, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature (°C)")+
    labs(color='Addition') +
    ggtitle("BG/(LAP+NAG)")
  
  gg_CN_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=CN))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "CN"), aes(y = 0.75, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature (°C)")+
    labs(color='Addition') +
    ggtitle("BG/(LAP+NAG)")
  
  gg_alpha =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=alpha, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "alpha"), aes(y = 0.68, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("alpha [(EC+EX)/(EC+EX+BG+BX)]")
  
  gg_alpha_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=alpha))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "alpha"), aes(y = 0.68, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("alpha [(EC+EX)/(EC+EX+BG+BX)]")
  
  
  
  
  gg_alpha1 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=alphaEC, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "alphaEC"), aes(y = 0.68, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("alpha [EndoC/(EndoC+BG)]")
  
  gg_alpha_12 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=alphaEC))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "alphaEC"), aes(y = 0.68, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("alpha [EndoC/(EndoC+BG)]")
  
  gg_alpha2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=alphaEX, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label2 %>% filter(analyte == "alphaEX"), aes(y = 0.82, label = label),position= position_dodge(width = 1))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("alpha [EndoX/(EndoX+BX)]")
  
  gg_alpha_22 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=alphaEX))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "alphaEX"), aes(y = 0.82, label = label))+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("alpha [EndoX/(EndoX+BX)]")
  
  
  #EA<- enzyme_processed%>%
   # pivot_longer(alpha:alphaEX)%>%
    #group_by(Temp,name)%>%
    #summarise(mean(value),sd(value))%>%
    #filter(name %in% c("alpha","alphaEC","alphaEX"))
  
  
  plot_grid(gg_alpha_12,gg_alpha_22)
  
  list("BG/(LAP+NAG)" = gg_CN_2,
       "BG/(LAP+NAG)" = gg_CN,
       "Alpha" = gg_alpha_2,
       "Alpha" = gg_alpha,
       gg_alpha_12=gg_alpha_12,
       gg_alpha1=gg_alpha1,
       gg_alpha_22=gg_alpha_22,
       gg_alpha2=gg_alpha2
       
       
  )
  
}

plot_PredictedSoilTemp = function(Kotz_proccessed_HMX){
  
  EZ2<- Kotz_proccessed_HMX%>%
    pivot_longer(cols=TAVG.mean:Xeric.Soil)%>%
    mutate(decade = floor(YEAR/10)*10) %>%
    group_by(MONTH, decade, name)
  EZ2$decade<- as.character(EZ2$decade)
  
  EZ3 = EZ2 %>%
    filter(name== "Xeric.Soil" | name== "Mesic.Soil" |name== "Hydric.Soil", decade >= 1950) %>%
    mutate(MONTH2 = month.abb[MONTH])
  EZ3$MONTH2<-factor(EZ3$MONTH2, levels = month.abb)
  
  gg_SoilTempPredict=
    EZ3%>%
    ggplot(aes(x=MONTH2, y=value, fill=decade))+
    geom_bar(stat='identity',position='dodge')+
    ylab(expression(paste("10 cm estimated Soil Temp. (°C)")))+
    xlab("Month")+
    scale_fill_manual(values=cbPalette)+
    ggtitle("Historic soil temp. estimates")+
    guides(fill=guide_legend(title="Decade"))+
    geom_hline(yintercept=10, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=6, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=2, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-2, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-6, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-10, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=0, color = "black", size=1)+
    theme(          legend.text = element_text(size = 12),
                    legend.key.size = unit(1.5, 'lines'),
                    legend.background = element_rect(colour = NA),
                    panel.border = element_rect(color="black",size=2, fill = NA),
                    plot.title = element_text(size = 20, face = "bold"),
                    axis.text = element_text(size = 12, color = "black"),
                    axis.title.x = element_text(size = 15, face = "bold", color = "black"),
                    axis.title.y = element_text(size = 15, face = "bold", color = "black"),
                    axis.text.x = element_text(angle = 45,vjust=1.1,hjust= 0.9),
                    
                    # formatting for facets
                    panel.background = element_blank(),
                    strip.background = element_rect(colour="white", fill="white"), #facet formatting
                    panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
                    panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
                    strip.text.x = element_text(size=25, face="bold"), #facet labels
                    strip.text.y = element_text(size=25, face="bold", angle = 270) #facet labels
    )

  
  
  
  
  list("Estimated Historic Soil Temperature" = gg_SoilTempPredict
  )
  
}

plot_enzyme_respiration = function(enzyme_processed,respiration_processed){
  
  
  enzyme_data_long = enzyme_processed %>%
    pivot_longer(cols= BG:CN,
                 names_to= "analyte",
                 values_to= "conc")
  
  ENZM_hsd_label = 
    enzyme_data_long %>% 
    group_by(analyte) %>% 
    do(fit_hsd(.))
  
  ENZM_hsd_label2 = 
    enzyme_data_long %>% 
    group_by(analyte,Add) %>% 
    do(fit_hsd(.))  
    
  enzyme_processed[enzyme_processed$EndoX<0, "EndoX"] <- 0
  
  gg_alpha_12 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=alphaEC))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 position = position_dodge(width = 0.6), 
                 alpha = 0.2,
                 aes(group = interaction(Temp)))+
    geom_point(position = position_dodge(width = 0.6), size = 3)+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "alphaEC"), aes(y = 1.15, label = label))+
    theme_light()+
    ylim(NA, 1.3)+
    #scale_y_continuous(expand = c(0,0), limits=c(0,0.62),oob=rescale_none)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("EC-alpha [EC/(EC+BG)]")+
    theme_CKM3()
  
  gg_alpha_22 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=alphaEX))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 position = position_dodge(width = 0.6), 
                 alpha = 0.2,
                 aes(group = interaction(Temp)))+
    geom_point(position = position_dodge(width = 0.6), size = 3)+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "alphaEX"), aes(y = 1.15, label = label))+
    theme_light()+
    ylim(NA, 1.3)+
    #scale_y_continuous(expand = c(0,0), limits=c(0,0.9),oob=rescale_none)+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("EX-alpha [EX/(EX+BX)]")+
    theme_CKM3()
  
  
  
  
  title1 <- ggdraw() + draw_label("EC and EX α values", size=20,fontface='bold')+theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  EndoAlpha <- plot_grid(gg_alpha_12,gg_alpha_22,gg_alpha_12,gg_alpha_22, nrow=2, labels = c("A", "B","C","D"),label_size = 12)
  
  
  
  
  
  enzyme_data = enzyme_processed %>%
    select(ID,Temp,Add,BG,BX,EndoC,EndoX,alpha)%>%
    mutate(alphaEC= (EndoC/(EndoC+BG)),alphaEX= (EndoX/(EndoX+BX)))%>%
    filter(Add=="None", Temp != "Pre")
  
  respiration_processed = respiration_processed %>%
    select(ID,Temp,Add,val,JD2)%>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10")))%>%
    filter(Add=="None")%>%
    group_by(Temp)%>%
    filter(JD2==max(JD2, na.rm=TRUE))
  
  MERG<-c("ID","Temp","Add")
RESENZYME = merge(enzyme_data,respiration_processed, by= MERG) %>%
  pivot_longer(cols=BG:alphaEX,
               names_to= "enzyme",
               values_to= "activ")
  
Graphs = RESENZYME %>%
  ggplot(aes(x=activ, y=val))+
  geom_point(size = 2)+
  geom_smooth(method=lm, color="black")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=3000)+
  theme_light()+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  labs(x = "enzyme activity")+
  labs(y = "Total C respired")+
  facet_wrap(~enzyme,  scales = "free")+
  ggtitle("Enzyme activity vs total C respired")+
  theme_CKM3()


RESENZYME2 = merge(enzyme_data,respiration_processed, by= MERG) %>%
  pivot_longer(cols=BG:EndoX,
               names_to= "enzyme",
               values_to= "activ")

e.label<- c("β-1,4-glucosidase (BG)","β-1,4-xylosidase (BX)","endo-β-D-1,4-glucanase(EC)","endo-β-1,4xylanase (EX)")
names(e.label)<- c("BG","BX","EndoC","EndoX")

Graphs2 = RESENZYME2 %>%
  ggplot(aes(x=activ, y=val))+
  geom_point(size = 3)+
  geom_smooth(method=lm, color="black")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
  theme_light()+
  ylim(NA, 5500)+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  labs(x = "Enzyme activity")+
  labs(y = "Total C respired")+
  facet_wrap(~enzyme,  scales = "free", labeller= labeller(enzyme=e.label))+
  ggtitle("Enzyme activity vs total C respired")+
  theme_CKM3()

Graphs3 = RESENZYME2 %>%
  filter(enzyme=="EndoC")%>%
  ggplot(aes(x=activ, y=val))+
  geom_point(size = 3)+
  geom_smooth(method=lm, color="black")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
  theme_light()+
  ylim(NA, 5500)+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  labs(y = "Total C respired")+
  ggtitle("endo-β-D-1,4-glucanase (EC)")+
  theme_CKM3()
Graphs4 = RESENZYME2 %>%
  filter(enzyme=="EndoX")%>%
  ggplot(aes(x=activ, y=val))+
  geom_point(size = 3)+
  geom_smooth(method=lm, color="black")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
  theme_light()+
  ylim(NA, 5500)+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  ggtitle("endo-β-1,4xylanase (EX)")+
  theme_CKM3()
Graphs5 = RESENZYME2 %>%
  filter(enzyme=="BG")%>%
  ggplot(aes(x=activ, y=val))+
  geom_point(size = 3)+
  geom_smooth(method=lm, color="black")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
  theme_light()+
  ylim(NA, 5500)+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  labs(x = "Enzyme activity")+
  labs(y = "Total C respired")+
  ggtitle("β-1,4-glucosidase (BG)")+
  theme_CKM3()
Graphs6 = RESENZYME2 %>%
  filter(enzyme=="BX")%>%
  ggplot(aes(x=activ, y=val))+
  geom_point(size = 3)+
  geom_smooth(method=lm, color="black")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
  ylim(NA, 5500)+
  theme_light()+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  labs(x = "Enzyme activity")+
  ggtitle("β-1,4-xylosidase (BX)")+
  theme_CKM3()


title <- ggdraw() + draw_label("Enzyme activity vs total C respired", size=20,fontface='bold')




res_EA<- plot_grid(Graphs3,Graphs4,Graphs5,Graphs6,ncol=2,labels = c("A", "B","C","D"),label_size = 12)

res_EA2<-plot_grid(title,res_EA, ncol=1, rel_heights = c(0.2,2))

EndoAlpha2<-plot_grid(title1,EndoAlpha, ncol=1, rel_heights = c(0.2,2))


alpha_Res_EA<-plot_grid(title,res_EA,title1,EndoAlpha, ncol=1, rel_heights = c(0.2,2,0.2,1))
  
  list(Graphs=Graphs,
       alpha_Res_EA=alpha_Res_EA,
       res_EA2=res_EA2,
       EndoAlpha2=EndoAlpha2
  )
  
}