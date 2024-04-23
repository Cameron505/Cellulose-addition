library(tidyverse) #for tidy processing and plotting
library(vegan) # for PERMANOVA analysis
library(ggbiplot) #for PCA biplots
library(reshape2)
library(pracma)
library(ggpubr)
library(doBy)
library(lubridate)
library(agricolae)
library(cowplot)
library(gridExtra)
library(lsmeans)
# to install {ggbiplot}:
# library(devtools)
# install_github("vqv/ggbiplot")

#graphing parameters
Axis.x=20    #xaxis font size
Axis.y=20    #yaxis font size
Axis.y2=16
tit=22       #title font size
lab=12      #ABCD label sizes



length2= function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}


fit_hsd = function(dat){
  a = aov(conc ~ Temp, data = dat)
  h = HSD.test(a, "Temp")
  h$groups %>% mutate(Temp = row.names(.)) %>%
    dplyr::rename(label = groups) %>%  
    dplyr::select(Temp, label)
}


# custom ggplot theme
cbPalette <- c("#003152", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#888888")
cbPalette4 <-c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette5 <-c("#1F78B4", "#33A02C", "#6A3D9A", "#FF7F00", "#B15928", "#A6CEE3", "#B2DF8A", "#661100", "#6699CC")
cbPalette6 <- c("#F08080", "#6495ED", "#8FBC8F", "#FF8C00", "#9370DB", "#66CDAA", "#CD5C5C", "#7B68EE", "#6B8E23", "#708090")
theme_CKM <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "none",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=1, fill = NA),
          
          plot.title = element_text(hjust = 0, size = 25),
          axis.text = element_text(size = 30, color = "black"),
          axis.title = element_text(size = 30, face = "bold", color = "black"),
          
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=25, face="bold"), #facet labels
          strip.text.y = element_text(size=25, face="bold", angle = 270) #facet labels
    )
}

theme_CKM2 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(          legend.position = "none",
                    plot.title = element_text(vjust = 2, size = 12, face="bold"),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.ticks=element_line(size=1, color="black"),
                    axis.text=element_text(size=12, color="black"),
                    title=element_text(size=10, face ="bold"),
                    legend.title = element_text(size=14, face ="bold"),
                    panel.border = element_rect(color="black",size=2, fill = NA),
                    # formatting for facets
                    panel.background = element_rect(color="black",size=1, fill = NA),
                    strip.background = element_rect(colour= NA, fill= NA), #facet formatting
                    #panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
                    #panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
                    strip.text.x = element_text(size=12, face="bold"), #facet labels
                    strip.text.y = element_text(size=12, face="bold", angle = 270), #facet labels
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank()
    )
}
theme_CKML <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(          plot.title = element_text(vjust = 2, size = 12, face="bold"),
                    axis.ticks=element_line(size=1, color="black"),
                    axis.text=element_text(size=12, color="black"),
                    title=element_text(size=10, face ="bold"),
                    legend.title = element_text(size=14, face ="bold"),
                    panel.border = element_rect(color="black",size=2, fill = NA),
                    # formatting for facets
                    panel.background = element_rect(color="black",size=1, fill = NA),
                    strip.background = element_rect(colour= NA, fill= NA), #facet formatting
                    #panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
                    #panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
                    strip.text.x = element_text(size=12, face="bold"), #facet labels
                    strip.text.y = element_text(size=12, face="bold", angle = 270), #facet labels
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank()
    )
}
theme_CKMM <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(          legend.position = "none",
                    plot.title = element_text(vjust = 2, size = 12, face="bold"),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.ticks=element_line(size=1, color="black"),
                    axis.text=element_text(size=10, color="black"),
                    axis.text.x = element_text(size=10, angle = 90),
                    title=element_text(size=10, face ="bold"),
                    legend.title = element_text(size=14, face ="bold"),
                    panel.border = element_rect(color="black",size=2, fill = NA),
                    # formatting for facets
                    panel.background = element_rect(color="black",size=1, fill = NA),
                    strip.background = element_rect(colour= NA, fill= NA), #facet formatting
                    #panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
                    #panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
                    strip.text.x = element_text(size=12, face="bold"), #facet labels
                    strip.text.y = element_text(size=12, face="bold", angle = 270), #facet labels
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank()
    )
}
theme_CKM3 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(          legend.position = "none",
                    panel.border = element_rect(color="black",size=1, fill = NA),
                    plot.title = element_text(vjust = 2, size = 10, face="bold"),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.ticks=element_line(size=2, color="black"),
                    axis.text=element_text(size=12, color="black"),
                    title=element_text(size=10, face="bold"),
                    legend.title = element_text(size=10, face ="bold"),
                    # formatting for facets
                    panel.background = element_blank(),
                    strip.background = element_rect(colour="white", fill="white"), #facet formatting
                    panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
                    panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
                    strip.text.x = element_text(size=10, face="bold"), #facet labels
                    strip.text.y = element_text(size=10, face="bold", angle = 270) #facet labels
    )
}
