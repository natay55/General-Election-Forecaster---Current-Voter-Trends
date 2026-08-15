#-------------------------------------------------------------------------------------------
# UK General Election Forecasting Model
# 
# Following:
#   Hanretty, Lauderdale and Vivyan (2015) — polling aggregator
#   Park, Gelman and Bafumi (2004)         — MRP framework  
#   Lauderdale (2018)                      — constituency level MRP
#
# Pipeline:
#   01 — libraries and configuration
#   02 — Bayesian polling aggregator (Stan)
#   03 — BES individual level survey data
#   04 — constituency level predictors and spatial lags
#   05 — GLM MRP model fitting
#   06 — poststratification
#   07 — asymmetric calibration and unwinding
#   08 — FPTP seat allocation and validation

#--------------------------------------------------------------------------------------------
#English model
source(here("main-pipeline","All","01_library_config.R"))
source(here("main-pipeline","England","02_polling_aggregator.R"))
source(here("main-pipeline","All","03_bes_data.R"))
source(here("main-pipeline","England","04_constituency_data.R"))
source(here("main-pipeline","England","05_mrp_model.R"))
source(here("main-pipeline","England","06_poststratification.R"))
source(here("main-pipeline","England","07_calibration_unwinding.R"))
source(here("main-pipeline","England","08_seat_predictions.R"))

#---------------------------------------------------------------------------------------------
#Welsh Model (note: constituency data adds both Welsh and English info so no need to run an 04)
source(here("main-pipeline","Wales","02_welsh_aggregator.R"))
source(here("main-pipeline","Wales","05_welsh_mrp_model.R"))
source(here("main-pipeline","Wales","06_welsh_poststratification.R"))
source(here("main-pipeline","Wales","07_welsh_calibration_unwinding.R"))
source(here("main-pipeline","Wales","08_welsh_seat_predictions.R"))

#---------------------------------------------------------------------------------------------
#Scottish Model 
source(here("main-pipeline","Scotland","02_scottish_aggregator.R"))
source(here("main-pipeline","Scotland","04_scottish_constituency_data.R"))
source(here("main-pipeline","Scotland","05_scottish_mrp_model.R"))
source(here("main-pipeline","Scotland","06_scottish_poststratification.R"))
source(here("main-pipeline","Scotland","07_scottish_calibration_unwinding.R"))
source(here("main-pipeline","Scotland","08_scot_seat_predictions.R"))

#---------------------------------------------------------------------------------------------
#Run total seat predictions
source(here("main-pipeline","All","09_add_predictions.R"))
source(here("main-pipeline","All","10_write_predictions.R"))

