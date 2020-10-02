# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### --------- Invertebrates ---------- ###
# -------------------------------------- #

# date written/ modified: 19.06.20, 01.07. 20.8
# date used: 01.07.20, 02.07, 03.07, 12.08, 20., 14.09. 
# Jonathan Jupke 
# Get Real WP2 
# Macroinvertebrates 


# Overview River types  ---------------------------------------------------

#   RT-01 Very large rivers
#   RT-02 Lowland, siliceous, medium-large
#   RT-03 Lowland, siliceous, very small-small
#   RT-04 Lowland, calcareous or mixed, medium-large
#   RT-05 Lowland, calcareous or mixed, very small-small
#   RT-06 Lowland, organic and siliceous, very small-large
#   RT-08 Mid-altitude, siliceous, medium-large
#   RT-09 Mid-altitude, siliceous, very small-small
#   RT-10 Mid-altitude, calcareous or mixed, medium-large
#   RT-11 Mid-altitude, calcareous or mixed, very small-small
#   RT-12 Mid-altitude, organic and siliceous, very small-large 
#   RT-13 Mid-altitude, organic and calcareous/mixed
#   RT-14 Highland (all Europe), siliceous, incl. organic (humic)
#   RT-15 Highland (all Europe), calcareous/mixed
#   RT-16 Glacial rivers (all Europe)
#   RT-17 Mediterranean, lowland, medium-Large, perennial
#   RT-18 Mediterranean, mid altitude, medium-large,
#   RT-19 Mediterranean, very small-small, perennial


# setup -------------------------------------------------------------------
pacman::p_load(dplyr, magrittr, data.table, stringr, taxize, purrr, ggplot2, here, viridis, fuzzySim, vegan)
setwd(here("003_processed_data"))

source("../002_r_scripts/F_001_Redundnat.R")

# load data ---------------------------------------------------------------
spe <- readRDS("006_2020-09-14_indicator_spe.RDS")
gen <- readRDS("006_2020-09-14_indicator_gen.RDS")
fol <- readRDS("006_2020-09-14_indicator_foh.RDS")


# create typical communities ----------------------------------------------
ta_spe <- spe[B > 0.25 | B > 0.20 & p_value <= 0.05 | A > 0.80]
ta_gen <- gen[B > 0.50 | B > 0.33 & p_value <= 0.05 | A > 0.95]
ta_fol <- fol[B > 0.95 | B > 0.80 & p_value <= 0.01 | A > 0.99]

ta_spe <- ta_spe[taxon != "Notonectidae"]

ta_all <- rbindlist(list(ta_spe, ta_gen, ta_fol))

rt_vector <- unique(ta_all$group)
n_types   <- length(rt_vector)

# redundancies 

redundant(x = 1, data = ta_all)
redundant(x = 2, data = ta_all)
redundant(x = 3, data = ta_all)
redundant(x = 4, data = ta_all)
redundant(x = 5, data = ta_all)
redundant(x = 6, data = ta_all)
redundant(x = 7, data = ta_all)
redundant(x = 8, data = ta_all)
redundant(x = 9, data = ta_all)
redundant(x = 10, data = ta_all)
redundant(x = 11, data = ta_all)
redundant(x = 12, data = ta_all)
