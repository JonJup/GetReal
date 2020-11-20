# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### -----  Macroinvertebrates -------- ###
### ------- level frequencies  ------- ###
# -------------------------------------- #

# 09.11.20
# GetReal
# Working Package 2 
# Macroinvertebrates

if(!require(pacman))install.packages("pacman")
p_load(data.table,
       dplyr,
       ggplot2,
       here, 
       magrittr)

DIR = list(rs = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/002_r_scripts/"),
           pd = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"))

call_ta_setup = file.path(DIR$rs, "08_c_setup_ta_analysis.R")
source(call_ta_setup)

ch_river_types = unique(dt_bty$group)

dt_ac_mzb <- data.table(
        river_type = character(length(ch_river_types)),
        species    =   integer(length(ch_river_types)),
        genus      =   integer(length(ch_river_types))
)

for (i in seq_along(ch_river_types)) {
        river_type_var <- ch_river_types[i]
        dt_ac_mzb[i, river_type := river_type_var]
        dt_ac_mzb[i, c("species", "genus", "fol") :=
                      .(dt_sty[group == river_type_var, .N],
                        dt_gty[group == river_type_var, .N],
                        dt_fty[group == river_type_var, .N])]
}

# drop stream types that were not considered
dt_ac_mzb <- dt_ac_mzb[!(species == 0 & genus == 0 & fol == 0)]
# reshape for plot
dt_acp_mzb <- melt(
        dt_ac_mzb,
        id.vars = c("river_type"),
        measure.vars = c("species", "genus", "fol")
)

dt_acp_mzb$river_type %<>% stringr::str_remove_all(pattern = "RT")
dt_acp_mzb[value == 0, value := NA]
#dt_acp_mzb[, river_type := factor(river_type, levels = c("1", "2_3", "4_5", "8-11_15_16", "12", "14", "18"))]
gg_mzb_per_level <-
        ggplot(data = dt_acp_mzb, aes(x = river_type, y = value)) +
        geom_point(aes(col = variable), size = 3) +
        ylab("number of taxa") +
        xlab("river type")+ 
        scale_color_brewer(palette = "Set2") +
        ggtitle("Macroinvertebrates")

# remove = rm(dir_rs, call_ta_setup, dt_ac, dt_acp_mzb, dir_pd, dt_bta, dt_bty, dt_fol, dt_fta, dt_fty, dt_gen, dt_gta,
#             ch_river_types, dt_gty, dt_spe, dt_sta, dt_sty,i,river_type_var)
# rm(list = remove)
# rm(remove)