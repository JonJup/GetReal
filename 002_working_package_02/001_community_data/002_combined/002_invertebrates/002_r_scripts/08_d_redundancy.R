# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------- Macroinvertebrates ------- ###
### ---------- Redundancy   ---------- ###
# -------------------------------------- #

# 06.11.20
# GetReal
# Working Package 2 
# Macroinvertebrates

#function
call_redundant_function = file.path(DIR$pd, "../002_r_scripts/f_01_redundant.R")
source(call_redundant_function)
ch_river_types = unique(dt_bty$group)
for (j in ch_river_types){
        redundant(x = j, data = dt_bty)
        rm(j)
}

rm(call_redundant_function, redundant, ch_river_types)
print("#--------------------------------------------------------#")