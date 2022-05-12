## FTICR-MS WORKFLOW
## USE THIS SCRIPT TO ANALYZE FTICR-MS DATA

##############################
##############################

## SOURCE THE FUNCTION FILES FIRST, AND THEN RUN THE SCRIPT
## IMPORT THE PROCESSED FILES AND THEN RUN THEM THROUGH THIS SCRIPT.
## -KFP/ 2022

##############################
##############################


#
# 0. setup packages and functions -----------------------------------------

source("code/0-packages.R")

# source the functions
source("code/fticrrr/b-functions_analysis.R")


#source("code/fticrrr/b-functions_relabund.R")
#source("code/fticrrr/c-functions_vankrevelen.R")
#source("code/fticrrr/d-functions_statistics.R")


#
# 1. load files -----------------------------------------------------------

fticr_meta  = read.csv("data/processed/fticr_polar_meta.csv")
fticr_data_longform = read.csv("data/processed/fticr_polar_data_longform.csv")
fticr_data_trt = read.csv("data/processed/fticr_polar_trt.csv")

## SET the treatment variables
## this will work with multiple variables too. just add all the variable names in the parentheses.
TREATMENTS = dplyr::quos(Site, Year, Season)


#
# 2. van krevelen plots ---------------------------------------------------
## 2a. domains ----

gg_vk_domains = 
  gg_vankrev(fticr_meta, aes(x = OC, y = HC, color = Class))+
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2"))+
  theme_kp()

gg_vk_domains_nosc = 
  gg_vankrev(fticr_meta, aes(x = OC, y = HC, color = as.numeric(NOSC)))+
  scale_color_gradientn(colors = PNWColors::pnw_palette("Bay"))+
  theme_kp()


#
## 2b. treatments ----

# first, create a dataframe with HC, OC columns

fticr_hcoc = 
  fticr_data_trt %>% 
  left_join(dplyr::select(fticr_meta, formula, HC, OC), by = "formula")

gg_vk_all = 
  gg_vankrev(fticr_hcoc, aes(x = OC, y = HC, color = Site))+
  stat_ellipse(level = 0.90, show.legend = FALSE)+
  facet_grid(Season~Year)+
  theme_kp()

gg_vk_all_site = 
  gg_vankrev(fticr_hcoc, aes(x = OC, y = HC, color = as.character(Year)))+
  stat_ellipse(level = 0.90, show.legend = FALSE)+
  facet_grid(~Site)+
  theme_kp()

#
# spring-late spring, hydric-mesic ----
fticr_spring2018 = 
  fticr_hcoc %>% 
  filter(Site %in% c("Hydric", "Mesic") & Season %in% c("Spring", "LateSpring") & Year == 2018)

gg_vk_spring2018 = 
  fticr_spring2018 %>% 
  gg_vankrev(aes(x = OC, y = HC, color = Season))+
  stat_ellipse(level = 0.90, show.legend = FALSE)+
  facet_wrap(~Site)+
  theme_kp()
# spring-late spring, hydric-mesic ---- Unique peaks
fticr_spring2018_Unique = 
  fticr_hcoc %>% 
  filter(Site %in% c("Hydric", "Mesic") & Season %in% c("Spring", "LateSpring") & Year == 2018) %>%
  distinct(formula, Site, HC, OC,Season) %>% 
  group_by(formula) %>% 
  dplyr::mutate(n = n())

gg_vk_spring2018_Unique = 
  fticr_spring2018_Unique %>% 
  gg_vankrev(aes(x = OC, y = HC, color = Season))+
  stat_ellipse(level = 0.90, show.legend = FALSE)+
  facet_wrap(~Site)+
  theme_kp()

#
# unique peaks, by site ----
fticr_unique_site = 
  fticr_hcoc %>% 
  distinct(formula, Site, HC, OC) %>% 
  group_by(formula) %>% 
  dplyr::mutate(n = n())


gg_site_unique =
  fticr_unique_site %>% filter(n == 1) %>% 
    gg_vankrev(aes(x = OC, y = HC, color = Site))+
    stat_ellipse(level = 0.90, show.legend = FALSE)+
    facet_wrap(~Site)+
    labs(title = "Unique peaks at each Site")+
    theme_kp()


gg_site_common = 
    fticr_unique_site %>% filter(n == 3) %>% 
    gg_vankrev(aes(x = OC, y = HC))+
    stat_ellipse(level = 0.90, show.legend = FALSE)+
    labs(title = "Peaks common to all sites")+
    theme_kp()


# overlay unique peaks onto common peaks
gg_site_common_unique = 
    fticr_unique_site %>% filter(n == 3) %>% 
    gg_vankrev(aes(x = OC, y = HC))+
    geom_point(data = fticr_unique_site %>% filter(n == 1),
               aes(color = Site), alpha = 0.7)+
    facet_wrap(~Site)+
    labs(title = "Unique peaks at each Site",
         subtitle = "black/grey = peaks common to all")+
    theme_kp()


## summarize unique peaks
fticr_unique_site_summary = 
  fticr_unique_site %>% 
  filter(n == 1) %>% 
  left_join(fticr_meta %>% dplyr::select(formula, Class)) %>% 
  group_by(Site, Class) %>% 
  dplyr::summarise(counts = n())

#
# unique peaks, spring 2018 ----
# comparing peaks lost/gained for spring vs. latespring

unique_peaks_2018 = 
  fticr_spring2018 %>% 
  group_by(formula, Site) %>% 
  dplyr::mutate(n = n())

unique_peaks_2018 %>% 
  filter(n == 1) %>% 
  gg_vankrev(aes(x = OC, y = HC, color = Season))+
  facet_wrap(~Site)+  
  stat_ellipse(level = 0.90, show.legend = FALSE)+
  theme_kp()


#
# 3. relative abundance ---------------------------------------------------
## 3a. calculations ----

# calculate relative abundance for each core/sample
# make sure totals add up to 100 % for each sample 
# use this for stats, including PERMANOVA, PCA
relabund_cores = 
  fticr_data_longform %>% 
  compute_relabund_cores(fticr_meta, TREATMENTS)

# compute mean values per treatment combination
# use this for summary tables and bar graphs
relabund_trt = 
  relabund_cores %>% 
  group_by(!!!TREATMENTS, Class) %>% 
  dplyr::summarize(rel_abund = round(mean(relabund),2),
                   se  = round((sd(relabund/sqrt(n()))),2),
                   relative_abundance = paste(rel_abund, "\u00b1",se)) %>% 
  ungroup()  %>% 
  mutate(Class = factor(Class, levels = c("aliphatic", "unsaturated/lignin", "aromatic", "condensed aromatic")))


## 3b. graphs ----

relabund_trt %>% 
  ggplot(aes(x = Site, y = rel_abund, fill = Class))+
  geom_bar(stat = "identity")+
  facet_grid(Season ~ Year)+
  theme_kp()


#

# 4. statistics -----------------------------------------------------------
## 4a. PCA ----

# all samples
pca_hydric = fit_pca_function(relabund_cores)

(gg_pca_by_site = 
  ggbiplot(pca_all$pca_int, obs.scale = 1, var.scale = 1,
           groups = as.character(pca_all$grp$Site), 
           ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
  geom_point(size=3,stroke=1, alpha = 0.5,
             aes(#shape = groups,
                 color = groups))+
  #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
  xlim(-4,4)+
  ylim(-3.5,3.5)+
  labs(shape="",
       title = "all samples",
       subtitle = "separation by Site")+
  theme_kp()+
  NULL
)


# Hydric only
pca_hydric = fit_pca_function(relabund_cores %>% filter(Site == "Hydric"))

(gg_pca_hydric = 
    ggbiplot(pca_hydric$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_hydric$grp$Season), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 0.5,
               aes(shape = as.character(pca_hydric$grp$Year),
                 color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "Hydric samples",
         subtitle = "separation by Season")+
    theme_kp()+
    NULL
)

# Mesic only
pca_Mesic = fit_pca_function(relabund_cores %>% filter(Site == "Mesic"))

(gg_pca_Mesic = 
    ggbiplot(pca_Mesic$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_Mesic$grp$Season), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 0.5,
               aes(shape = as.character(pca_Mesic$grp$Year),
                   color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "Mesic samples",
         subtitle = "separation by Season")+
    theme_kp()+
    NULL
)

# Xeric only
pca_Xeric = fit_pca_function(relabund_cores %>% filter(Site == "Xeric"))

(gg_pca_Xeric = 
    ggbiplot(pca_Xeric$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_Xeric$grp$Season), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 0.5,
               aes(shape = as.character(pca_Xeric$grp$Year),
                   color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "Xeric samples",
         subtitle = "separation by Season")+
    theme_kp()+
    NULL
)

# spring vs late spring mesic hydric
pca_Mesic_SpringLateSpring = fit_pca_function(relabund_cores %>% filter(Site %in% ("Mesic") & Season %in% c("Spring", "LateSpring") & Year == 2018))
gg_pca_Mesic_Spring_Latespring = 
    ggbiplot(pca_Mesic_SpringLateSpring$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(pca_Mesic_SpringLateSpring$grp$Season), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1, alpha = 0.5,
               aes(shape = as.character(pca_Mesic_SpringLateSpring$grp$Year),
                   color = groups))+
    #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(shape="",
         title = "Mesic samples",
         subtitle = "separation by Season")+
    theme_kp()+
    NULL

pca_Hydric_SpringLateSpring = fit_pca_function(relabund_cores %>% filter(Site %in% ("Hydric") & Season %in% c("Spring", "LateSpring") & Year == 2018))
gg_pca_Hydric_Spring_Latespring = 
  ggbiplot(pca_Hydric_SpringLateSpring$pca_int, obs.scale = 1, var.scale = 1,
           groups = as.character(pca_Hydric_SpringLateSpring$grp$Season), 
           ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
  geom_point(size=3,stroke=1, alpha = 0.5,
             aes(shape = as.character(pca_Hydric_SpringLateSpring$grp$Year),
                 color = groups))+
  #scale_shape_manual(values = c(21, 22, 19), name = "", guide = "none")+
  xlim(-4,4)+
  ylim(-3.5,3.5)+
  labs(shape="",
       title = "Hydric samples",
       subtitle = "separation by Season")+
  theme_kp()+
  NULL


#
## 4b. PERMANOVA ----

# you need to convert the rel-abundance file into wide form

relabund_wide = 
  relabund_cores %>% 
  ungroup() %>% 
  mutate(Class = factor(Class, 
                        levels = c("aliphatic", "unsaturated/lignin", 
                                   "aromatic", "condensed aromatic"))) %>% 
  dplyr::select(-c(abund, total)) %>% 
  spread(Class, relabund) %>% 
  replace(is.na(.), 0)

# adonis function for PERMANOVA
permanova_fticr_all = 
  adonis(relabund_wide %>% dplyr::select(aliphatic:`condensed aromatic`) ~ Site * Year * Season, 
         data = relabund_wide) 

# Site, Site:Year were significant (p < 0.05)
# Site accounted for 72 % of total variation among samples (R2= 0.72)