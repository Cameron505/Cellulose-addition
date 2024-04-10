plot_respiration_MS = function(respiration_processed){
 
 
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
    theme_CKM2()+
    theme(plot.title = element_blank())
  
  C<-text_grob("Cumulative Soil Respiration", size=tit)
  A<-text_grob("Incubation day", size=Axis.x)
  B<-text_grob(bquote('Cumulative C respired (μg)'), size=Axis.y, rot = 90)
  EA22<-arrangeGrob(gg_Avgcumres, left = B, bottom = A, top=C)
  EA222<-as_ggplot(EA22)
  
  title3 <- ggdraw() + draw_label("Theoretical hault of cellulose decomposition", size=tit)
  
  
  
  list(EA222=EA222,
       title3=title3)
  
}

plot_enzyme1_MS = function(enzyme_processed){
  
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
    ggtitle("endo-β-1,4-xylanase (EX)")+
    theme_CKM2()
  
  gg_Ecombine= plot_grid(
    gg_EndoC_3,
    gg_EndoX_3,
    gg_BG_3,
    gg_BX_3,
    align = 'vh',
    labels = c("A", "B", "C", "D"),
    label_size = lab,
    hjust = -1,
    nrow = 2
  )
  
 
  C<-text_grob("Enzyme Activities", size=tit)
  A<-text_grob("Incubation tempature (°C)", size=Axis.x)
  B<-text_grob(bquote('nmol'~g^-1 ~ dry ~ soil~hr^-1*''), size=Axis.y, rot = 90)
  EA22<-arrangeGrob(gg_Ecombine, left = B, bottom = A, top=C)
  EA222<-as_ggplot(EA22)
  
  
  list(EA222=EA222
  )
  
}

plot_alpha_MS = function(enzyme_processed,respiration_processed){
  
  
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
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "alphaEC"), aes(y = 1.1, label = label))+
    theme_light()+
    ylim(0, 1.1)+
    #scale_y_continuous(expand = c(0,0), limits=c(0,0.62),oob=rescale_none)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("EC-alpha [EC/(EC+BG)]")+
    theme_CKM2()
  
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
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "alphaEX"), aes(y = 1.1, label = label))+
    theme_light()+
    ylim(0, 1.1)+
    #scale_y_continuous(expand = c(0,0), limits=c(0,0.9),oob=rescale_none)+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("EX-alpha [EX/(EX+BX)]")+
    theme_CKM2()
  
  
  
  
  
  
  EndoAlpha <- plot_grid(gg_alpha_12,gg_alpha_22, nrow=1, labels = c("A", "B"),label_size = lab)
  C<-text_grob("EC and EX α values", size=tit)
  A<-text_grob("Incubation tempature (°C)", size= Axis.x)
  B<-text_grob("α", size=Axis.y, rot = 90)
  EA22<-arrangeGrob(EndoAlpha, left = B, bottom = A, top=C)
  EA222<-as_ggplot(EA22)
  
  
  
  
  list(EA222=EA222
  )
  
}

plot_enzyme_respiration_MS = function(enzyme_processed,respiration_processed){
  
  
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
  
  
  RESENZYME2 = merge(enzyme_data,respiration_processed, by= MERG) %>%
    pivot_longer(cols=BG:EndoX,
                 names_to= "enzyme",
                 values_to= "activ")
  
  e.label<- c("β-1,4-glucosidase (BG)","β-1,4-xylosidase (BX)","endo-β-D-1,4-glucanase(EC)","endo-β-1,4-xylanase (EX)")
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
    theme_CKM2()
  
  Graphs3 = RESENZYME2 %>%
    filter(enzyme=="EndoC")%>%
    ggplot(aes(x=activ, y=val))+
    geom_point(size = 3)+
    geom_smooth(method=lm, color="black")+
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
    theme_light()+
    ylim(NA, 5500)+
    scale_x_continuous(breaks = seq(0, 300, by=50), labels = function(x) ifelse(x == 300, "", x)) +
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(y = "Total C respired")+
    ggtitle("endo-β-D-1,4-glucanase (EC)")+
    theme_CKM2()
  Graphs4 = RESENZYME2 %>%
    filter(enzyme=="EndoX")%>%
    ggplot(aes(x=activ, y=val))+
    geom_point(size = 3)+
    geom_smooth(method=lm, color="black")+
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
    theme_light()+
    ylim(NA, 5500)+
    scale_x_continuous(breaks = seq(0, 150, by=50), labels = function(x) ifelse(x == 150, "", x)) +
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    ggtitle("endo-β-1,4-xylanase (EX)")+
    theme_CKM2()
  Graphs5 = RESENZYME2 %>%
    filter(enzyme=="BG")%>%
    ggplot(aes(x=activ, y=val))+
    geom_point(size = 3)+
    geom_smooth(method=lm, color="black")+
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
    theme_light()+
    ylim(NA, 5500)+
    scale_x_continuous(breaks = seq(0, 400, by=50), labels = function(x) ifelse(x == 400, "", x)) +
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Enzyme activity")+
    labs(y = "Total C respired")+
    ggtitle("β-1,4-glucosidase (BG)")+
    theme_CKM2()
  Graphs6 = RESENZYME2 %>%
    filter(enzyme=="BX")%>%
    ggplot(aes(x=activ, y=val))+
    geom_point(size = 3)+
    geom_smooth(method=lm, color="black")+
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
    ylim(NA, 5500)+
    scale_x_continuous(breaks = seq(0, 350, by=50), labels = function(x) ifelse(x == 350, "", x)) +
    theme_light()+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Enzyme activity")+
    ggtitle("β-1,4-xylosidase (BX)")+
    theme_CKM2()
  
  
  title <- ggdraw() + draw_label("Enzyme activity vs total C respired", size=20,fontface='bold')
  
  
  
  
  res_EA<- plot_grid(Graphs3,Graphs4,Graphs5,Graphs6,ncol=2,labels = c("A", "B","C","D"),label_size = lab)
  C<-text_grob(expression(paste("Enzyme activity vs total C respired" )), size=tit)
  A<-text_grob(expression(paste('nmol g'^-1, "dry soil hr "^-1*'potential enzyme activities' )), size=Axis.x)
  B<-text_grob(expression(paste("Total C respired (μg)" )), size=Axis.y, rot = 90)
  res_EA22<-arrangeGrob(res_EA, left = B, bottom = A,top=C)
  AA<-grid.draw(res_EA22)
  res_EA222<-as_ggplot(res_EA22)
  
  
  
  
  list(res_EA222=res_EA222
  )
  
}

plot_PredictedSoilTemp_MS = function(Kotz_proccessed_HMX){
  
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
    ggtitle("Historic soil temperature estimates")+
    guides(fill=guide_legend(title="Decade"))+
    geom_hline(yintercept=10, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=6, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=2, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-2, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-6, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-10, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=0, color = "black", size=1)+
    theme_CKM2()+
    theme(plot.title = element_blank())
    
  C<-text_grob("Historic soil temperature estimates", size=tit)
  A<-text_grob("Month", size=Axis.x)
  B<-text_grob(bquote('Predicted soil temperature ( °C)'), size=Axis.y, rot = 90)
  EA22<-arrangeGrob(gg_SoilTempPredict, left = B, bottom = A, top=C)
  EA222<-as_ggplot(EA22)
  
  
  
  list(EA222=EA222
  )
  
}