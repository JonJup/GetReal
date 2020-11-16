# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------------ Diatoms ------------- ###
### ------- level frequencies  ------- ###
# -------------------------------------- #

# 06.11.20
# GetReal
# Working Package 2 
# Diatoms

if(!require(pacman))install.packages("pacman")
p_load(data.table,
       dplyr,
       ggplot2,
       here, 
       magrittr)

dir_rs = "~/01_Uni/02_getreal/002_working_package_02/001_community_data/002_combined/001_diatoms/002_r_scripts/"
call_ta_setup = file.path(dir_rs, "11_a_setup_ta_analysis.R")
source(call_ta_setup)

dt_ac_dia <- data.table(
        river_type = character(length(ch_river_types)),
        species    =   integer(length(ch_river_types)),
        genus      =   integer(length(ch_river_types))
)

for (i in seq_along(ch_river_types)) {
        river_type_var <- ch_river_types[i]
        dt_ac_dia[i, river_type := river_type_var]
        dt_ac_dia[i, c("species", "genus") :=
                      .(dt_sty[group == river_type_var, .N],
                        dt_gty[group == river_type_var, .N])]
}

# drop stream types that were not considered
dt_ac_dia <- dt_ac_dia[!(species == 0 & genus == 0)]
# reshape for plot
dt_acp_dia <- melt(
        dt_ac_dia,
        id.vars = c("river_type"),
        measure.vars = c("species", "genus")
)

dt_acp_dia$river_type %<>% stringr::str_remove_all(pattern = "RT")
dt_acp_dia[river_type=="_large", river_type := "large"]
dt_acp_dia[value == 0, value := NA]
dt_acp_dia[, river_type := factor(river_type, levels = c("large", "3","6","9","12"))]
gg_dia_per_level <-
        ggplot(data = dt_acp_dia, aes(x = river_type, y = value)) +
        geom_point(aes(col = variable), size = 3) +
        ylab("number of taxa") +
        xlab("river type") + 
        scale_color_brewer(palette = "Set2") +
        ggtitle("Diatoms")

# remove = rm(dir_rs, call_ta_setup, dt_ac, dt_acp_dia_dia, dir_pd, dt_bta, dt_bty, dt_gen, dt_gta,
#             ch_river_types, dt_gty, dt_spe, dt_sta, dt_sty,i,river_type_var)
# rm(list = remove)
# rm(remove)