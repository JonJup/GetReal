# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------- Macroinvertebrates ------- ###
### ---------- Redundancy   ---------- ###
# -------------------------------------- #

# 06.11.20
# GetReal
# Working Package 2 
# Macroinvertebrates

dir_rs = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/002_r_scripts/")
call_ta_setup = file.path(dir_rs, "09_a_setup_ta_analysis.R")
source(call_ta_setup)
#function
call_redundant_function = file.path(dir_pd, "../002_r_scripts/f_01_redundant.R")
source(call_redundant_function)

for (j in ch_river_types){
        redundant(x = j, data = dt_bty)
        rm(j)
}
