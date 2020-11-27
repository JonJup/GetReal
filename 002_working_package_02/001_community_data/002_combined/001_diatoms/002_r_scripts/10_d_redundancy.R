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
source(file.path(DIR$red, "f_01_redundant.R"))

ma_redundnat = redundant(dt_bty)
print(ma_redundnat)
x11()
corrplot::corrplot.mixed(ma_redundnat, lower = "shade", upper = "number",
                         is.corr = FALSE,  order = "FPC") 
rm(redundant)

print("#----------------------------------------#")


