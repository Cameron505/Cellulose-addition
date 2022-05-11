## FTICR-MS WORKFLOW
## USE THIS SCRIPT TO PROCESS FTICR-MS DATA

##############################
##############################

## SOURCE THE FUNCTION FILES FIRST, AND THEN RUN THE SCRIPT
## DON'T RUN THE PROCESSING SCRIPT MULTIPLE TIMES. ONCE IS ENOUGH.
## -KFP/ 2022

##############################
##############################


# 1. SET input file paths -------------------------------
#This code is set up to process two sets of 
#For non polar data (Must be COREKEY because that is called directly from the functions)
COREKEYNonpolar = "data/fticr/Field_NonPolar_Fdata.csv"
REPORT2 = "data/fticr/xtra_Report_15T_FieldSite_Lower_non-polar_Consolidated.csv"

#FOR POLAR DATA
COREKEYpolar = "data/fticr/Field_Polar_Fdata.csv"
REPORT = "data/fticr/xtra_Report_15T_FieldSite_Upper_Polar_Consolidated.csv"
## SET the treatment variables
## this will work with multiple variables too. just add all the variable names in the parentheses.
TREATMENTS = dplyr::quos(Site, Year, Season)

#
# 2. load packages and source the functions --------------------------------------------------------
source("code/0-packages.R")

source("code/fticrrr/a-functions_processing.R")


#
# 3. import files needed --------------------------------------------------
## import the FTICR report and the corekey/sample info

report_polar = read.csv(REPORT)
corekey_polar = read.csv(COREKEYpolar)

report_nonpolar = read.csv(REPORT2)
corekey_nonpolar = read.csv(COREKEYNonpolar)
#
# 4. process the data -----------------------------------------------------

## process and create eMeta file
## this file contains Metadata for each m/z peak

fticr_meta_polar = make_fticr_meta(report_polar)$meta2

fticr_meta_nonpolar = make_fticr_meta(report_nonpolar)$meta2
## process and create the longform Data file

# *_longform: molecules on a presence/absence bases, including all reps analyzed
# use *_longform file for statistical analyses, relative abundances

corekey_polar <- read.csv(COREKEYpolar)
fticr_data_longform_polar = make_fticr_data(report_polar, corekey_polar, TREATMENTS)$data_long_key_repfiltered

corekey_nonpolar <- read.csv(COREKEYNonpolar)
fticr_data_longform_nonpolar = make_fticr_data(report_nonpolar, corekey_nonpolar, TREATMENTS)$data_long_key_repfiltered

# *_trt: summary of the longform file, by treatment. replicates are removed
# use *_trt file for Van Krevelen graphs
fticr_data_trt_polar = make_fticr_data(report_polar, corekey_polar, TREATMENTS)$data_long_trt

fticr_data_trt_nonpolar = make_fticr_data(report_nonpolar, corekey_nonpolar, TREATMENTS)$data_long_trt
#


#
# 4b. combine polar and nonpolar ------------------------------------------




# 5. export processed data ------------------------------------------------
fticr_meta_polar %>% write.csv("data/processed/fticr_polar_meta.csv", row.names = FALSE)
fticr_data_longform_polar %>% write.csv("data/processed/fticr_polar_data_longform.csv", row.names = FALSE)
fticr_data_trt_polar %>% write.csv("data/processed/fticr_polar_trt.csv", row.names = FALSE)

fticr_meta_nonpolar %>% write.csv("data/processed/fticr_nonpolar_meta.csv", row.names = FALSE)
fticr_data_longform_nonpolar %>% write.csv("data/processed/fticr_nonpolar_data_longform.csv", row.names = FALSE)
fticr_data_trt_nonpolar %>% write.csv("data/processed/fticr_nonpolar_trt.csv", row.names = FALSE)


