# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "tidyverse"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
source("Code/0-packages.R")
source("Code/a-processing_functions.R")
source("Code/b-analysis_functions.R")
source("Code/c-plotting_for_manuscript.R")


# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  
  
  # data files
  #tar_target()
  tar_target(respiration_data_read,"Data/Res2.csv", format="file"),
  tar_target(respiration_data, read.csv(respiration_data_read)),
  tar_target(respiration_processed, process_respiration(respiration_data)),
  tar_target(nutrients_data_read,"Data/Nutrients.csv", format="file"),
  tar_target(nutrients_data, read.csv(nutrients_data_read)),
  tar_target(enzyme_data_read,"Data/Enzyme.csv", format="file"),
  tar_target(enzyme_data, read.csv(enzyme_data_read)),
  tar_target(enzyme_processed, process_enzyme(enzyme_data)),
  tar_target(MicrobialBiomass_data_read,"Data/Cell_Microbial biomass.csv", format="file"),
  tar_target(MicrobialBiomass_data, read.csv(MicrobialBiomass_data_read)),
  tar_target(KotzTemp_data_read,"Data/kotz.daily.air.temp.1900.2022.csv", format="file"),
  tar_target(KotzTemp_data, read.csv(KotzTemp_data_read)),
  tar_target(Kotz_proccessed, process_KotzTemp(KotzTemp_data)),
  tar_target(SoilTempHydric_data_read,"Data/Hydric Met Station.csv", format="file"),
  tar_target(SoilTempHydric_data, read.csv(SoilTempHydric_data_read)),
  tar_target(SoilTempMesic_data_read,"Data/Mesic Met Station.csv", format="file"),
  tar_target(SoilTempMesic_data, read.csv(SoilTempMesic_data_read)),
  tar_target(SoilTempXeric_data_read,"Data/Xeric Met Station.csv", format="file"),
  tar_target(SoilTempXeric_data, read.csv(SoilTempXeric_data_read)),
  
  tar_target(KotzTempPredict_data_read,"Data/Kotzebue_RCP6_SNAP_Predict.csv", format="file"),
  tar_target(KotzTempPredict_data, read.csv(KotzTempPredict_data_read)),
  tar_target(KotzPredict_proccessed, process_KotzTempPredict(KotzTempPredict_data)),
  
  
  #Merging and predicting soil temperatures based on air temps

  tar_target(Kotz_proccessed_HMX, process_SoilTemp(SoilTempHydric_data,SoilTempMesic_data,SoilTempXeric_data,Kotz_proccessed)),
  #Plotting for manuscript
  tar_target(gg_respiration2, plot_respiration_MS(respiration_processed)),
  tar_target(gg_enzyme12, plot_enzyme1_MS(enzyme_processed)),
  tar_target(gg_SoilTemp2, plot_PredictedSoilTemp_MS(Kotz_proccessed_HMX)),
  tar_target(gg_Res_Enzyme2, plot_enzyme_respiration_MS(enzyme_processed,respiration_processed)),
  tar_target(gg_alpha2, plot_alpha_MS(enzyme_processed,respiration_processed)),
  
  # analysis - graphs
  tar_target(gg_respiration, plot_respiration(respiration_processed)),
  tar_target(gg_nutrients, plot_nutrients(nutrients_data)),
  tar_target(gg_MicrobialBiomass, plot_MicrobialBiomass(MicrobialBiomass_data)),
  tar_target(gg_enzyme1, plot_enzyme1(enzyme_processed)),
  tar_target(gg_enzyme2, plot_enzyme2(enzyme_processed)),
  tar_target(gg_enzyme3, plot_enzyme3(enzyme_processed)),
  tar_target(gg_SoilTemp, plot_PredictedSoilTemp(Kotz_proccessed_HMX)),
  tar_target(gg_Res_Enzyme, plot_enzyme_respiration(enzyme_processed,respiration_processed)),
 
  
  # report  
  tar_render(report, path = "reports/CelluloseAddition_report.Rmd"),
  tar_render(report2, path = "reports/Temperature_threshold_figures.Rmd")
  
)
