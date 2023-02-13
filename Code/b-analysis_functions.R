plot_respiration = function(respiration_processed){
  
  gg_res =
    respiration_processed %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10"))) %>%
    ggplot(aes(x = JD2, y = Res, color = Add))+
    geom_point(position = position_dodge(width = 0.4),
               size = 2)+
    ylab(expression(paste( "Respiration (",mu,"g-C",day^-1, ")")))+
    facet_wrap(~Temp, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    ylab(expression(paste( "Respiration (",mu,"g-C",day^-1, ")")))+
    xlab("incubation day")+
    labs(color='Addition') +
    ggtitle("Soil Respiration")
  
  gg_cumres =
    respiration_processed %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10"))) %>%
    ggplot(aes(x = JD2, y = val, color =  Add))+
    geom_point(position = position_dodge(width = 0.4),
               size = 2)+
    ylab(expression(paste( "Respiration (",mu,"g-C)")))+
    facet_wrap(~Temp, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    ylab(expression(paste( "Respiration (",mu,"g-C)")))+
    xlab("incubation day")+
    labs(color='Addition') +
    ggtitle("Cumulative Soil Respiration")
  
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
    ggtitle("Average Soil Respiration")
  
  gg_Avgcumres =
    respiration_processed %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10"))) %>%
    ggplot(aes(x=JD2, y=val, color=Add))+
    stat_summary(fun = mean,geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~Temp, scale="free")+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    ylab(expression(paste( "Respiration (",mu,"g-C)")))+
    xlab("incubation day")+
    labs(color='Addition') +
    ggtitle("Average Cumulative Soil Respiration")
  list("Respiration" = gg_res,
       "Average Respiration" = gg_Avgres,
       "Cumulative Respiration" = gg_cumres,
       "Average Cumulative Respiration" = gg_Avgcumres)
  
}

plot_nutrients = function(nutrients_data){
  
  fit_aov = function(nutrients_data){
    
    a = aov(conc ~ Temp, data = nutrients_data)
    broom::tidy(a) %>% 
      filter(term == "Temp") %>% 
      dplyr::select(`p.value`) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }
  
  nutrients_data_long = nutrients_data %>%
    pivot_longer(cols= NH4:TRS,
                 names_to= "analyte",
                 values_to= "conc")
  
  all_aov = 
    nutrients_data_long %>% 
    group_by(analyte,Add) %>% 
    do(fit_aov(.)) %>% 
    mutate(Temp = "-2")
    # factor the Inc_temp so they can line up in the graph
    #mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre")))
  
  HSD_all = agricolae::HSD.test(all_aov, "Add") #attempting to get abc comparisons. 
  
  
  gg_NH4 =
    nutrients_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=NH4, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
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
    geom_text(data = all_aov %>% filter(analyte == "NH4"), aes(y = 5, label = asterisk))+
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
  
  gg_MBC =
    MicrobialBiomass_data %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10", "Pre"))) %>%
    ggplot(aes(x=Temp, y=MBC, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
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
  
  gg_BG =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=BG, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
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
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("α-L-Arabinofuranosidase (AFS)")
  
  

  
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
       "AFS" = gg_AFS
       )
  
}

plot_enzyme2 = function(enzyme_processed){
  
  gg_LAP =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=LAP, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
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
  
  
  gg_CN =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=CN, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature")+
    labs(color='Addition') +
    ggtitle("BG/(LAP+NAG)")
  
  gg_CN_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=CN))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature")+
    labs(color='Addition') +
    ggtitle("BG/(LAP+NAG)")
  
  gg_alpha =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=alpha, fill=Add))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("alpha [(EC+EX)/(EC+EX+BG+BX)]")
  
  gg_alpha_2 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=alpha))+
    stat_summary(fun = mean,geom = "bar",size = 2, position= "dodge") +
    stat_summary(fun.data = mean_se, geom = "errorbar", position= "dodge")+
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("alpha [(EC+EX)/(EC+EX+BG+BX)]")

  
  
  list("BG/(LAP+NAG)" = gg_CN_2,
       "BG/(LAP+NAG)" = gg_CN,
       "Alpha" = gg_alpha_2,
       "Alpha" = gg_alpha
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
    ylab(expression(paste("10 cm estimated Soil Temp. °C)")))+
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
    theme(          legend.text = element_text(size = 12),
                    legend.key.size = unit(1.5, 'lines'),
                    legend.background = element_rect(colour = NA),
                    panel.border = element_rect(color="black",size=1.5, fill = NA),
                    
                    plot.title = element_text(hjust = 0, size = 25),
                    axis.text = element_text(size = 20, color = "black"),
                    axis.title = element_text(size = 21, face = "bold", color = "black"),
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