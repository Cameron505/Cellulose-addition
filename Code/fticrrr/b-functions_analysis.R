# FTICRRR: fticr results in R
# functions for data analysis

################################################## #
################################################## #

## This script contains functions for FTICR data analysis,
## including relative abundance, Van Krevelen Plots, and PCA.
## Source these functions in the workflow script
## -KFP/2022

################################################## #
################################################## #


#
# Relative Abundance ------------------------------------------------------

## this function will calculate relative abundance by compound class for each sample/core

compute_relabund_cores = function(fticr_data_longform, fticr_meta, TREATMENTS){
  fticr_data_longform %>% 
    # add the Class column to the data
    left_join(dplyr::select(fticr_meta, formula, Class), by = "formula") %>% 
    # calculate abundance of each Class as the sum of all counts
    group_by(CoreID, Class, !!!TREATMENTS) %>%
    dplyr::summarise(abund = sum(presence)) %>%
    ungroup %>% 
    # create a new column for total counts per core assignment
    # and then calculate relative abundance  
    group_by(CoreID, !!!TREATMENTS) %>% 
    dplyr::mutate(total = sum(abund),
                  relabund  = round((abund/total)*100,2))
}

#
# Van Krevelens -----------------------------------------------------------

## this function creates the base Van Krevelen plot. Add points to this using geom_point()

gg_vankrev <- function(data,mapping){
  ggplot(data,mapping) +
    # plot points
    geom_point(size=1, alpha = 0.2) + # set size and transparency
    # axis labels
    ylab("H/C") +
    xlab("O/C") +
    # axis limits
    xlim(0,1.25) +
    ylim(0,2.5) +
    # add boundary lines for Van Krevelen regions
    geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
    guides(colour = guide_legend(override.aes = list(alpha=1)))
}


#
# PCA ---------------------------------------------------------------------

## Use this function to compute the basic Principal Components Analysis

fit_pca_function = function(dat){
  # first, make wide-form
  relabund_pca=
    dat %>% 
    filter(!is.na(CoreID)) %>% 
    ungroup %>% 
    dplyr::select(-c(abund, total)) %>% 
    spread(Class, relabund) %>% 
    replace(.,is.na(.),0)  %>% 
    dplyr::select(-1)
  
  # then, separate the wide form into the numeric component (data) and the groups (treatments)
  num = 
    relabund_pca %>% 
    dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))
  
  grp = 
    relabund_pca %>% 
    dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
    dplyr::mutate(row = row_number())
  
  # finally, compute PCA using `prcomp()`
  pca_int = prcomp(num, scale. = T)
  
  list(num = num,
       grp = grp,
       pca_int = pca_int)
}


