# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------ macroinvertebrates -------- ###
### ------------ Setup   ------------- ###
# -------------------------------------- #

# 06.11.20
# GetReal
# Working Package 2 
# Macroinvertebrates

# load data ---------------------------------------------------------------
dt_spe <- readRDS(file.path(DIR$pd, "06_indicator_spe.RDS"))
dt_gen <- readRDS(file.path(DIR$pd, "06_indicator_gen.RDS"))
dt_fol <- readRDS(file.path(DIR$pd, "06_indicator_fol.RDS"))

# Deriving initial TAs ---------------------------------------------------
dt_sty = dt_spe[B > .25|A > .9]
dt_gty = dt_gen[B > .35|A > .8]
dt_fty = dt_fol[B > .55|A > .7] 

dt_bty <- rbindlist(list(dt_sty,
                         dt_gty,
                         dt_fty))

rm(dt_spe, dt_gen, dt_fol)

