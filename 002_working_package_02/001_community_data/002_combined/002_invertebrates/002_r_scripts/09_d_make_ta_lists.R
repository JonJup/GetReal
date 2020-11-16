# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------- Macroinvertebrates ------- ###
### ---------- Make Lists   ---------- ###
# -------------------------------------- #

# 16.11.20
# GetReal
# Working Package 2 
# Macroinvertebrates

library(dplyr)
library(stringr)

dir_rs = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/002_r_scripts/")
call_ta_setup = file.path(dir_rs, "09_a_setup_ta_analysis.R")
source(call_ta_setup)

dt_bty$group %>% unique

var = "RT18"

dt_bty[group == var, taxon] %>%
        str_replace_all(pattern = "\\.", "\\ ") %>%
        paste(sep = ",", collapse = ", ") %>%
        writeClipboard()

dt_bta[group == var, taxon] %>%
        str_replace_all(pattern = "\\.", "\\ ") %>%
        paste(sep = ",", collapse = ", ") %>%
        writeClipboard()
