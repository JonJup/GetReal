# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------------ Diatoms ------------- ###
### ------------ Setup   ------------- ###
# -------------------------------------- #

# 05.11.20
# GetReal
# Working Package 2 
# Diatoms

if(!require(pacman)) install.packages("pacman")
p_load(data.table,
       here)

# directory
dir_pd = here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/")

# load data ---------------------------------------------------------------
dt_spe <- readRDS(file.path(dir_pd, "11_indicator_spe.RDS"))
dt_gen <- readRDS(file.path(dir_pd, "11_indicator_gen.RDS"))

# Deriving initial TAs ---------------------------------------------------
dt_sty = dt_spe[B > 0.20]
dt_gty = dt_gen[B > 0.33]
dt_bty <- rbindlist(list(dt_sty,
                         dt_gty))
dt_sta = dt_spe[A > 0.9]
dt_gta = dt_gen[A > 0.7] 
dt_bta = rbindlist(list(dt_sta, 
                        dt_gta))
ch_river_types = unique(dt_bty$group) 
