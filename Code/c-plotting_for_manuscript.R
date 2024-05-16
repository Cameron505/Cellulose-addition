plot_respiration_MS = function(respiration_processed){
 
 
  R.label<- c("-6 °C","-2 °C","2 °C","6 °C","10 °C")
  names(R.label)<- c("-6","-2","2","6","10")
  
  
  gg_Avgcumres =
    respiration_processed %>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10"))) %>%
    ggplot(aes(x=JD2, y=val,color=Add))+
    stat_summary(aes(color=Add),fun = mean,geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    facet_wrap(~Temp, scale="free", labeller = labeller(Temp=R.label))+
    labs(shape = 'Addition', color = 'Addition')+
    theme_light()+
    scale_color_manual(values=cbPalette)+
    theme_CKM2()+
    theme(plot.title = element_blank())
  
  C<-text_grob("Cumulative soil respiration", size=tit)
  A<-text_grob("Incubation day", size=Axis.x)
  B<-text_grob(bquote('Cumulative carbon respired (μg)'), size=Axis.y, rot = 90)
  EA22<-arrangeGrob(gg_Avgcumres, left = B, bottom = A, top=C)
  EA222<-as_ggplot(EA22)
  
  title3 <- ggdraw() + draw_label("Theoretical barrier for cellulose decomposition", size=tit)
  
  
  
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
  gg_BG_L =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=BG))+
    geom_boxplot(show.legend = T, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 position = position_dodge(width = 0.6), 
                 alpha = 0.2,
                 aes(group = interaction(Temp)))+
    geom_point(aes(color=Add),position = position_dodge(width = 0.6), size = 4)+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "BG"), aes(y = 610, label = label),size = 6, color = "black")+
    #scale_y_continuous(expand=c(0,0),limits=c(0,440))+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(nmol'~g^-1 ~ dry ~ soil~hr^-1*')'))+
    labs(color='Addition') +
    ggtitle("β-1,4-glucosidase (BG)")
  
  
  gg_BG_3 =
    enzyme_processed %>%
    ggplot(aes(x=Temp, y=BG))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 position = position_dodge(width = 0.6), 
                 alpha = 0.2,
                 aes(group = interaction(Temp)))+
    geom_point(aes(color=Add),position = position_dodge(width = 0.6), size = 4)+
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
    geom_point(aes(color=Add),position = position_dodge(width = 0.6), size = 4)+
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
    geom_point(aes(color=Add),position = position_dodge(width = 0.6), size = 4)+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "EndoC"), aes(y = 365, label = label),size = 6, color = "black")+
    #scale_y_continuous(expand=c(0,0),limits=c(0,440))+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(miliCellG5 Units'~g^-1 ~ dry ~ soil~hr^-1*')'))+
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
    geom_point(aes(color=Add),position = position_dodge(width = 0.6), size = 4)+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "EndoX"), aes(y = 235, label = label),size = 6, color = "black")+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Incubation temperature", 
         y = bquote('(miliCellG5 Units'~g^-1 ~ dry ~ soil~hr^-1*')'))+
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
  
 
  gg_EcombineA= plot_grid(
    gg_EndoC_3,
    gg_EndoX_3,
    ncol=2,
    nrow=1,
    labels = c("A", "B"),
    label_size = lab,
    hjust = -1
  )
  
  gg_EcombineB= plot_grid(
    gg_BG_3,
    gg_BX_3,
    ncol=2,
    labels = c( "C", "D"),
    label_size = lab,
    hjust = -1
  )
  
  
  
  
  
  
  C<-text_grob("Enzyme activities", size=tit)
  A<-text_grob("Incubation tempature (°C)", size=Axis.x)
  B<-text_grob(bquote('nmol'~g^-1 ~ dry ~ soil~hr^-1*''), size=Axis.y2, rot = 90)
  B2<-text_grob(bquote('miliCellG5 units '~g^-1 ~ dry ~ soil*''), size=Axis.y2, rot = 90)
  Q<-arrangeGrob(gg_EcombineA, left = B2)
  Q2<-arrangeGrob(gg_EcombineB, left = B)
  
  gg_Q<-plot_grid(Q,Q2,ncol=1)
  
  gg_QQ<-arrangeGrob(gg_Q, bottom = A, top=C)
  gg_fix<-as_ggplot(gg_QQ)
  
  EA22<-arrangeGrob(gg_Ecombine, left = B, bottom = A, top=C)
  Legend<- get_legend(gg_BG_L)
  EA222<-as_ggplot(EA22)
  L<-as_ggplot(Legend)
  
  
  list(EA222=EA222,
       L=L,
       gg_fix=gg_fix
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
    geom_point(aes(color=Add),position = position_dodge(width = 0.6), size = 3)+
    geom_text(data = ENZM_hsd_label %>% filter(analyte == "alphaEC"), aes(y = 1.1, label = label))+
    theme_light()+
    ylim(0, 1.1)+
    #scale_y_continuous(expand = c(0,0), limits=c(0,0.62),oob=rescale_none)+
    labs(x = "Incubation tempature (°C)", 
         y = bquote(alpha))+
    labs(color='Addition') +
    ggtitle("EC-alpha [EC/(EC+BG)]")+
    scale_colour_manual(values=cbPalette)+
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
    geom_point(aes(color=Add),position = position_dodge(width = 0.6), size = 3)+
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
    geom_point(aes(shape=Temp),size = 3)+
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
    geom_point(aes(color=Add, shape=Temp),size = 3)+
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
    geom_point(aes(color=Add, shape=Temp),size = 3)+
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
    geom_point(aes(color=Add, shape=Temp),size = 3)+
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
    geom_point(aes(color=Add, shape=Temp),size = 3)+
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

plot_enzyme_respiration_MS2 = function(enzyme_processed,respiration_processed){
  
  
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
    filter(Temp != "Pre")
  
  respiration_processed2 = respiration_processed %>%
    select(ID,Temp,Add,val,JD2)%>%
    mutate(Temp = factor(Temp, levels=c("-6","-2","2","6","10")))%>%
    group_by(Temp)%>%
    filter(JD2==max(JD2, na.rm=TRUE))
  
  MERG<-c("ID","Temp","Add")
  RESENZYME = merge(enzyme_data,respiration_processed2, by= MERG) %>%
    pivot_longer(cols=BG:alphaEX,
                 names_to= "enzyme",
                 values_to= "activ")
  
  
  RESENZYME2 = merge(enzyme_data,respiration_processed2, by= MERG) %>%
    pivot_longer(cols=BG:EndoX,
                 names_to= "enzyme",
                 values_to= "activ")
  
  e.label<- c("β-1,4-glucosidase (BG)","β-1,4-xylosidase (BX)","endo-β-D-1,4-glucanase(EC)","endo-β-1,4-xylanase (EX)")
  names(e.label)<- c("BG","BX","EndoC","EndoX")
  
  GraphsL = RESENZYME2 %>%
    ggplot(aes(x=activ, y=val))+
    geom_point(aes(color=Add, shape=Temp),size = 3)+
    geom_smooth(method=lm, color="black")+
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
    theme_light()+
    ylim(NA, 5500)+
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Enzyme activity")+
    labs(y = "Total C respired")+
    facet_wrap(~enzyme,  scales = "free", labeller= labeller(enzyme=e.label))+
    ggtitle("Enzyme activity vs total C respired")
  
  
  
  
  
  
  Graphs2 = RESENZYME2 %>%
    ggplot(aes(x=activ, y=val))+
    geom_point(aes(color=Add, shape=Temp),size = 3)+
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
    geom_point(aes(color=Add, shape=Temp),size = 3)+
    geom_smooth(method=lm, color="black")+
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
    theme_light()+
    ylim(NA, 5500)+
    scale_x_continuous(breaks = seq(0, 350, by=50), labels = function(x) ifelse(x == 300, "", x)) +
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(y = "Total C respired")+
    ggtitle("endo-β-D-1,4-glucanase (EC)")+
    theme_CKM2()
  Graphs4 = RESENZYME2 %>%
    filter(enzyme=="EndoX")%>%
    ggplot(aes(x=activ, y=val))+
    geom_point(aes(color=Add, shape=Temp),size = 3)+
    geom_smooth(method=lm, color="black")+
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
    theme_light()+
    ylim(NA, 5500)+
    scale_x_continuous(breaks = seq(0, 250, by=50), labels = function(x) ifelse(x == 150, "", x)) +
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    ggtitle("endo-β-1,4-xylanase (EX)")+
    theme_CKM2()
  Graphs5 = RESENZYME2 %>%
    filter(enzyme=="BG")%>%
    ggplot(aes(x=activ, y=val))+
    geom_point(aes(color=Add, shape=Temp),size = 3)+
    geom_smooth(method=lm, color="black")+
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.y=5000)+
    theme_light()+
    ylim(NA, 5500)+
    scale_x_continuous(breaks = seq(0, 700, by=50), labels = function(x) ifelse(x == 400, "", x)) +
    scale_colour_manual(values=cbPalette)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Enzyme activity")+
    labs(y = "Total C respired")+
    ggtitle("β-1,4-glucosidase (BG)")+
    theme_CKM2()
  Graphs6 = RESENZYME2 %>%
    filter(enzyme=="BX")%>%
    ggplot(aes(x=activ, y=val))+
    geom_point(aes(color=Add, shape=Temp),size = 3)+
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
  Legend=get_legend(GraphsL)
  res_EA2222<-arrangeGrob(res_EA222,right=Legend)
  res_EA2222<-as_ggplot(res_EA2222)
  
  
  
  CTS<-c("BG", "BX", "EndoC","EndoX","val")
  
  
  
  RelativeChange <- RESENZYME2 %>%
    pivot_wider(names_from = enzyme, values_from = activ) %>%
    mutate_at(vars(Temp), as.character) %>%
    mutate_at(vars(Temp, BG, BX, EndoC, EndoX, val), as.numeric) %>%
    group_by(Temp,Add) %>%
    summarise_at(
      vars(all_of(CTS)),
      .funs = list(average = mean),
      .names = "average_{.col}"
    ) %>%
    mutate_at(vars(Temp, BG_average, BX_average, EndoC_average, EndoX_average,val_average), as.numeric) %>%
    ungroup()%>%
    group_by(Add)%>%
    arrange(Add,Temp)%>%
    mutate(
      relative_change_BG = (BG_average - lag(BG_average)) / lag(BG_average),
      relative_change_BX = (BX_average - lag(BX_average)) / lag(BX_average),
      relative_change_EndoC = (EndoC_average - lag(EndoC_average)) / lag(EndoC_average),
      relative_change_EndoX = (EndoX_average - lag(EndoX_average)) / lag(EndoX_average),
      relative_change_res = (val_average - lag(val_average)) / lag(val_average)
    )
  
  
  custom_levels <- c("-6 to -2", "-2 to 2", "2 to 6", "6 to 10")
  
  A<-RelativeChange%>%
    pivot_longer(relative_change_BG:relative_change_res)%>%
    mutate(Temp = case_when(
      is.na(Temp) ~ NA_character_,
      Temp <= -2 ~ "-6 to -2",
      Temp > -2 & Temp <= 2 ~ "-2 to 2",
      Temp > 2 & Temp <= 6 ~ "2 to 6",
      Temp > 6 & Temp <= 10 ~ "6 to 10"
    )) %>%
    filter(!is.na(Temp))%>%
    mutate(Temp = factor(Temp, levels = custom_levels))%>%
  ggplot(aes(x=Temp, y=value, color=name))+
    geom_point(size = 3, position=position_dodge(width=1))+
    scale_colour_manual(values=cbPalette6)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Proportional activity")+
    ggtitle("")+
    theme_CKML()
  
  
  
  
  
  AA<-RelativeChange%>%
    pivot_longer(relative_change_BG:relative_change_res)%>%
    filter(name!="relative_change_res")%>%
    mutate(Temp = case_when(
      is.na(Temp) ~ NA_character_,
      Temp <= -2 ~ "-6 to -2",
      Temp > -2 & Temp <= 2 ~ "-2 to 2",
      Temp > 2 & Temp <= 6 ~ "2 to 6",
      Temp > 6 & Temp <= 10 ~ "6 to 10"
    )) %>%
    filter(!is.na(Temp))%>%
    mutate(Temp = factor(Temp, levels = custom_levels))%>%
    ggplot(aes(x=Temp, y=value))+
    geom_point(size = 3)+
    scale_colour_manual(values=cbPalette6)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Proportional activity")+
    ggtitle("")+
    facet_wrap(~name)+
    scale_colour_manual(values=cbPalette)+
    theme_CKM2()
  
  
  
  AR<-RelativeChange%>%
    pivot_longer(relative_change_BG:relative_change_res)%>%
    filter(name=="relative_change_res")%>%
    mutate(Temp = case_when(
      is.na(Temp) ~ NA_character_,
      Temp <= -2 ~ "-6 to -2",
      Temp > -2 & Temp <= 2 ~ "-2 to 2",
      Temp > 2 & Temp <= 6 ~ "2 to 6",
      Temp > 6 & Temp <= 10 ~ "6 to 10"
    )) %>%
    filter(!is.na(Temp))%>%
    mutate(Temp = factor(Temp, levels = custom_levels))%>%
    ggplot(aes(x=Temp, y=value))+
    geom_point(size = 3)+
    scale_colour_manual(values=cbPalette6)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Proportional activity")+
    ggtitle("")+
    facet_wrap(~name)+
    scale_colour_manual(values=cbPalette)+
    theme_CKM2()
  
  RelativeChange<-na.omit(RelativeChange)
  
  RER1<-RelativeChange%>%
    pivot_longer(relative_change_BG:relative_change_EndoX)%>%
    mutate(Temp = case_when(
      is.na(Temp) ~ NA_character_,
      Temp <= -2 ~ "-6 to -2",
      Temp > -2 & Temp <= 2 ~ "-2 to 2",
      Temp > 2 & Temp <= 6 ~ "2 to 6",
      Temp > 6 & Temp <= 10 ~ "6 to 10"
    )) %>%
    filter(!is.na(Temp))%>%
    mutate(Temp = factor(Temp, levels = custom_levels))%>%
    mutate_at(vars(value, relative_change_res), as.numeric) %>%
    ggplot(aes(x=relative_change_res, y=value))+
    stat_summary(aes(color=Temp), fun="mean")+
    scale_colour_manual(values=cbPalette6)+
    scale_fill_manual(values=cbPalette)+
    labs(y = "Proportional activity")+
    ggtitle("")+
    facet_wrap(~name)+
    xlab("Relative change in respiration")+
    geom_smooth() +  
    theme_CKML()
  
  
  
  
  
  RER<-RelativeChange%>%
    pivot_longer(relative_change_BG:relative_change_EndoX)%>%
    mutate(Temp = case_when(
      is.na(Temp) ~ NA_character_,
      Temp <= -2 ~ "-6 to -2",
      Temp > -2 & Temp <= 2 ~ "-2 to 2",
      Temp > 2 & Temp <= 6 ~ "2 to 6",
      Temp > 6 & Temp <= 10 ~ "6 to 10"
    )) %>%
    filter(!is.na(Temp))%>%
    mutate(Temp = factor(Temp, levels = custom_levels))%>%
    mutate_at(vars(value, relative_change_res), as.numeric) %>%
    ggplot(aes(x=relative_change_res, y=value))+
    geom_point(aes(color=Temp),size = 3)+
    scale_colour_manual(values=cbPalette6)+
    scale_fill_manual(values=cbPalette)+
    labs(y = "Proportional activity")+
    ggtitle("")+
    facet_wrap(~name)+
    xlab("Relative change in respiration")+
    #geom_smooth() +  
    theme_CKML()
  
  
  RelativeChange2 <- RESENZYME2 %>%
    pivot_wider(names_from = enzyme, values_from = activ) %>%
    mutate_at(vars(Temp), as.character) %>%
    mutate_at(vars(Temp, BG, BX, EndoC, EndoX, val), as.numeric) %>%
    group_by(Temp) %>%
    summarise_at(
      vars(all_of(CTS)),
      .funs = list(average = mean),
      .names = "average_{.col}"
    ) %>%
    mutate_at(vars(Temp, BG_average, BX_average, EndoC_average, EndoX_average,val_average), as.numeric) %>%
    mutate(
      relative_change_BG = (BG_average - lag(BG_average)) / lag(BG_average),
      relative_change_BX = (BX_average - lag(BX_average)) / lag(BX_average),
      relative_change_EndoC = (EndoC_average - lag(EndoC_average)) / lag(EndoC_average),
      relative_change_EndoX = (EndoX_average - lag(EndoX_average)) / lag(EndoX_average),
      relative_change_res = (val_average - lag(val_average)) / lag(val_average)
    )
  
  RERR<-RelativeChange2%>%
    pivot_longer(relative_change_BG:relative_change_EndoX)%>%
    mutate(Temp = case_when(
      is.na(Temp) ~ NA_character_,
      Temp <= -2 ~ "-6 to -2",
      Temp > -2 & Temp <= 2 ~ "-2 to 2",
      Temp > 2 & Temp <= 6 ~ "2 to 6",
      Temp > 6 & Temp <= 10 ~ "6 to 10"
    )) %>%
    filter(!is.na(Temp))%>%
    mutate(Temp = factor(Temp, levels = custom_levels))%>%
    mutate_at(vars(value, relative_change_res), as.numeric) %>%
    ggplot(aes(x=relative_change_res, y=value))+
    geom_point(aes(color=Temp),size = 3)+
    scale_colour_manual(values=cbPalette6)+
    scale_fill_manual(values=cbPalette)+
    labs(y = "Relative change in enzyme potential")+
    ggtitle("")+
    facet_wrap(~name)+
    xlab("Relative change in respiration")+
    #geom_smooth(method = "loess") +
    theme_CKML()
  
  AAR<-RelativeChange2%>%
    pivot_longer(relative_change_BG:relative_change_res)%>%
    filter(name!="relative_change_res")%>%
    mutate(Temp = case_when(
      is.na(Temp) ~ NA_character_,
      Temp <= -2 ~ "-6 to -2",
      Temp > -2 & Temp <= 2 ~ "-2 to 2",
      Temp > 2 & Temp <= 6 ~ "2 to 6",
      Temp > 6 & Temp <= 10 ~ "6 to 10"
    )) %>%
    filter(!is.na(Temp))%>%
    mutate(Temp = factor(Temp, levels = custom_levels))%>%
    ggplot(aes(x=Temp, y=value))+
    geom_point(size = 3)+
    scale_colour_manual(values=cbPalette6)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Proportional activity")+
    ggtitle("")+
    facet_wrap(~name)+
    scale_colour_manual(values=cbPalette)+
    theme_CKM2()
  
  
  
  ARR<-RelativeChange2%>%
    pivot_longer(relative_change_BG:relative_change_res)%>%
    filter(name=="relative_change_res")%>%
    mutate(Temp = case_when(
      is.na(Temp) ~ NA_character_,
      Temp <= -2 ~ "-6 to -2",
      Temp > -2 & Temp <= 2 ~ "-2 to 2",
      Temp > 2 & Temp <= 6 ~ "2 to 6",
      Temp > 6 & Temp <= 10 ~ "6 to 10"
    )) %>%
    filter(!is.na(Temp))%>%
    mutate(Temp = factor(Temp, levels = custom_levels))%>%
    ggplot(aes(x=Temp, y=value))+
    geom_point(size = 3)+
    scale_colour_manual(values=cbPalette6)+
    scale_fill_manual(values=cbPalette)+
    labs(x = "Proportional activity")+
    ggtitle("")+
    facet_wrap(~name)+
    scale_colour_manual(values=cbPalette)+
    theme_CKM2()

  
  
  
  
  
  list(res_EA2222=res_EA2222,
       Legend=Legend,
       AA=AA,
       AR=AR,
       RER=RER,
       RERR=RERR,
       AAR=AAR,
       ARR=ARR
  )
  
}

plot_PredictedSoilTemp_MS = function(Kotz_proccessed_HMX,SoilTempClass_data){
  
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
    guides(fill=guide_legend(title="Decade ending"))+
    geom_hline(yintercept=10, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=6, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=2, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-2, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-6, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-10, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=0, color = "black", size=1)+
    theme_CKM2()+
    theme(plot.title = element_blank())
    
  
 
  gg_SoilTempPredict_Site=
    EZ3%>%
    mutate(name = recode(name, "Hydric.Soil" = "Wet", "Mesic.Soil" = "Moist", "Xeric.Soil" = "Dry"))%>%
    ggplot(aes(x=MONTH2, y=value, fill=decade))+
    geom_bar(stat='identity',position='dodge')+
    ylab(expression(paste("10 cm estimated Soil Temp. (°C)")))+
    xlab("Month")+
    scale_fill_manual(values=cbPalette5)+
    ggtitle("Historic soil temperature estimates")+
    guides(fill=guide_legend(title="Decade ending"))+
    geom_hline(yintercept=10, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=6, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=2, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-2, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-6, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=-10, linetype="dashed", color = "red", size=1)+
    geom_hline(yintercept=0, color = "black", size=1)+
    theme_CKMM()+
    facet_wrap(~name)+
    theme(plot.title = element_blank())
  
  
    gg_SoilTempPredict_SiteL=
      EZ3%>%
      mutate(name = recode(name, "Hydric.Soil" = "Wet", "Mesic.Soil" = "Moist", "Xeric.Soil" = "Dry"))%>%
    ggplot(aes(x=MONTH2, y=value, fill=decade))+
      geom_bar(stat='identity',position='dodge')+
      ylab(expression(paste("10 cm estimated Soil Temp. (°C)")))+
      xlab("Month")+
      scale_fill_manual(values=cbPalette5)+
      ggtitle("Historic soil temperature estimates")+
      guides(fill=guide_legend(title="Decade ending"))+
      geom_hline(yintercept=10, linetype="dashed", color = "red", size=1)+
      geom_hline(yintercept=6, linetype="dashed", color = "red", size=1)+
      geom_hline(yintercept=2, linetype="dashed", color = "red", size=1)+
      geom_hline(yintercept=-2, linetype="dashed", color = "red", size=1)+
      geom_hline(yintercept=-6, linetype="dashed", color = "red", size=1)+
      geom_hline(yintercept=-10, linetype="dashed", color = "red", size=1)+
      geom_hline(yintercept=0, color = "black", size=1)+
      facet_wrap(~name)+
      theme(plot.title = element_blank())
    FF<-get_legend(gg_SoilTempPredict_SiteL)
    Legend2<- as_ggplot(FF)
  
  C<-text_grob("Historic soil temperature estimates and day counts", size=tit)
  A<-text_grob("Month", size=Axis.x)
  B<-text_grob(bquote('Predicted soil temperature ( °C)'), size=Axis.y, rot = 90)
  
  
  
  
  EA22<-arrangeGrob(gg_SoilTempPredict, left = B, top=C)
  SET<-as_ggplot(EA22)
  
  #SET<- plot_grid(EA222, rel_widths = c(1,0.2))
  
  
  
  #### DAY COUNTS
  temp_cat_order <- c( "Near Freezing", "Weak Cold", "Very Cold","Extreme Cold")
  
  
  inc.lab<-c("2 to -2 °C", "-2 to -6 °C", "-6 to -10 °C", "<-10 °C")
  names(inc.lab) <- c("Near Freezing","Weak Cold",
                      "Very Cold",
                      "Extreme Cold")
  
  
  
  # Reorder Temp_Cat variable
  SoilTempClass_data1 <- SoilTempClass_data %>%
    mutate(Temp_Cat = factor(Temp_Cat, levels = temp_cat_order),
           Site = recode(Site, "hydric" = "Wet", "mesic" = "Moist", "xeric" = "Dry"))
  
 gg_daycount<- SoilTempClass_data1%>%
    ggplot(aes(x=as.factor(WaterYear),y=obs.day, fill= Temp_Cat))+
    geom_bar(stat="identity")+
    #facet_wrap(~Site)+
    theme_CKMM()+
    scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    scale_fill_manual(values=cbPalette4)+
   ylab("# of days")+
   xlab("Year")+
   scale_fill_discrete(labels= c("2 to -2 °C", "-2 to -6 °C", "-6 to -10 °C", "<-10 °C"))
  
 
 
 gg_daycountL<- SoilTempClass_data1%>%
   ggplot(aes(x=as.factor(WaterYear),y=obs.day, fill= Temp_Cat))+
   geom_bar(stat="identity")+
   #facet_wrap(~Site)+
   guides(fill=guide_legend(title="Temperature category"))+
   scale_y_continuous(expand = expansion(mult = c(0, 0)))+
   scale_fill_manual(values=cbPalette4)+
   ylab("# of days")+
   xlab("Year")+
   scale_fill_discrete(labels= c("2 to -2 °C", "-2 to -6 °C", "-6 to -10 °C", "<-10 °C"))
 
  AF<-get_legend(gg_daycountL)
  Legend1<- as_ggplot(AF)
  
  
  
  
  #CD<-text_grob("Days spent between ranges, 5 cm soil depth", size=tit)
  AD<-text_grob("Year", size=Axis.x)
  BD<-text_grob(bquote('Day counts'), size=Axis.y, rot = 90)
  CD<-text_grob(bquote('Days spent between temp. thresholds 2017-2023'))
  EAD<-arrangeGrob(gg_daycount, left = BD, bottom = AD, top= CD)
  EAD2<- plot_grid(EAD, Legend1, rel_widths = c(1,0.2))
  
  
 EAS<- plot_grid(SET,NULL,EAD, ncol=1, rel_heights = c(1,0.01,0.7), labels = c("A","", "B"),label_size = lab)
  
  
 
  
  
  list(SET=SET,
       gg_SoilTempPredict_Site=gg_SoilTempPredict_Site,
       gg_daycount=gg_daycount,
       Legend1=Legend1,
       Legend2=Legend2,
       EAS=EAS
  )
  
}