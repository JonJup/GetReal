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
source(file.path(DIR$pd, "../002_r_scripts/f_01_redundant.R"))

ma_redundnat = redundant(dt_mzb)
print(ma_redundnat)
x11()
corrplot::corrplot.mixed(ma_redundnat, lower = "shade", upper = "number", is.corr = FALSE, order = "FPC") 
rm(redundant)

print("#--------------------------------------------------------#")