# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### -----  Macroinvertebrates -------- ###
### --------- Ta uniqueness  --------- ###
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

source(file.path(DIR$rs, "08_c_setup_ta_analysis.R"))

utm <- sort(unique(dt_mzb$taxon))
# compute a "uniqueness score". Each taxon gets a score based on the number of
# TAs its part of. The score is 1 for taxa that only occur in one TA and 1/4 in
# those that occur in four. The score of a stream type is the mean score of its
# taxa.
for (i in seq_along(utm)) {
        dt_mzb[taxon == utm[i], score := 1/dt_mzb[taxon == utm[i], .N]]
}
for (i in seq_along(unique(dt_mzb$group))) {
        dt_mzb[group == unique(dt_mzb$group)[i], group_score := sum(score)/.N]
}

dt_mzb$group %<>% stringr::str_remove_all(pattern = "RT")

gg_mzb_uni <-
        ggplot(data = dt_mzb, aes(x = group, y = group_score))  +
        geom_point(size = 2) +
        ylim(c(0, 1)) +
        geom_hline(
                col = "red",
                yintercept = 1 / length(unique(dt_mzb$group)),
                linetype = 2
        ) +
        ylab("Uniqueness score") +
        xlab("River Type")


remove = rm(dir_rs, call_ta_setup, dt_ac, dt_acp, dir_pd, dt_bta, dt_mzb, dt_fol, dt_fta, dt_fty, dt_gen, dt_gta,
            ch_river_types, dt_gty, dt_spe, dt_sta, dt_sty,i,river_type_var, utm)
rm(list = remove)
rm(remove)