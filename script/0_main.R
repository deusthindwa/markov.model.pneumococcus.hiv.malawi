#written by Deus
#01/08/2022
#pneumococcal carriage ad serotype dynamics in HIV-infected adults in the infant PCV era

#====================================================================

#load packages
pacman::p_load(char = c("lubridate", "tidyverse", "dplyr", "here", "rio", "scales", "boot", "magrittr",  "mvtnorm", "zoo", "patchwork", 
                        "PropCIs", "reshape2","purrr", "msm", "minqa", "ggridges", "timetk", "ggbreak", "ggpubr", "gridExtra"))

#====================================================================

#getting and putting datasets in right order for analysis
source(here("script", "1_data_wrangling.R"))

#characterising spn carriage at baseline
source(here("script", "2_carriage_char.R"))

#run SIS Markov model in a Markov modelling framework
source(here("script", "3_markov_modelfit.R"))

#rerun multiple chains SIS Markov model for convergence and model fit check
source(here("script", "4_model_convergence.R"))

#use SIS Markov model to estimate acquisition and clearance of carriage 
source(here("script", "5_carriage_acq_dur.R"))

#use SIS Markov model to estimate hazard ratios comparing covariate levels
source(here("script", "6_carriage_hazard_ratios.R"))

#use SIS Markov model to estimate the number of carriage episodes 
source(here("script", "6_carriage_episodes.R"))

