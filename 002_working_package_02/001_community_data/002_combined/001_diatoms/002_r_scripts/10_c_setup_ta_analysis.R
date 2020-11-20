# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------------ Diatoms ------------- ###
### ------------ Setup   ------------- ###
# -------------------------------------- #

# 05.11.20
# GetReal
# Working Package 2 
# Diatoms

# load data ---------------------------------------------------------------
dt_spe = readRDS(file.path(DIR$pd, "11_indicator_spe.RDS"))
dt_gen = readRDS(file.path(DIR$pd, "11_indicator_gen.RDS"))

# Deriving initial TAs ---------------------------------------------------
dt_sty = dt_spe[B > 0.25 | A > 0.9]
dt_gty = dt_gen[B > 0.35 | A > 0.8]
dt_bty = rbindlist(list(dt_sty,
                         dt_gty))

rm(dt_spe, dt_gen)
