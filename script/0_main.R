#written by Deus
#01/08/2022
#pneumococcal carriage ad serotype dynamics in HIV-infected adults in the infant PCV era

#====================================================================

#load packages
pacman::p_load(char = c("lubridate","gtsummary", "tidyverse", "dplyr", "here", "rio", "scales", "boot", 
                        "magrittr",  "mvtnorm", "zoo", "patchwork", "mgcv", "PropCIs", "writexl", 
                        "reshape2", "growthcurver","purrr", "msm", "minqa", "ggridges", "msm", "lmtest", "timetk", "ggbreak",
                        "plotrix", "ggpubr"))

#====================================================================

#getting and putting datasets in right order for analysis
source(here("script", "1_data_wrangling.R"))

#characterising spn carriage at baseline
source(here("script", "2_carriage_char.R"))

#run SIS model structure in a Markov modelling framework
source(here("script", "3_markov_modelfit.R"))

#run multiple chains Markov model for convergence and check model fit 
source(here("script", "4_markov_convergence.R"))