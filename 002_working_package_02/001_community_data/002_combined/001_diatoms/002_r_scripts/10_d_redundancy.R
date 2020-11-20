# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------------ Diatoms ------------- ###
### ------------ Redundancy   -------- ###
# -------------------------------------- #

# 06.11.20
# GetReal
# Working Package 2 
# Diatoms

source(file.path(DIR$rs, "10_c_setup_ta_analysis.R"))
ch_river_types = unique(dt_bty$group)
#function
call_redundant_function = file.path(DIR$rs, "f_001_redundant.R")
source(call_redundant_function)

for (j in ch_river_types){
        redundant(j)
        rm(j)
}
print("#----------------------------------------#")
rm(ch_river_types, redundant)

