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
ls_dia = readRDS(file.path(DIR$pd, "09_indicator_list.RDS"))

if (!"x_ls_combine" %in% ls()){
        source(
                textConnection(
                        readLines(
                                file.path(DIR$rs, 
                                          "10_a_ta_master.R")
                        )[c(28,29)]
                )
        )
}

# Deriving initial TAs ---------------------------------------------------
ls_dia$spe = ls_dia$spe[B > x_ls_thesholds$spe$b | (A > x_ls_thesholds$spe$a & B > x_ls_thesholds$spe$b2)]
ls_dia$gen = ls_dia$gen[B > x_ls_thesholds$gen$b | (A > x_ls_thesholds$gen$a & B > x_ls_thesholds$spe$b2)]

dt_bty = rbindlist(ls_dia)

# dt_bty[A > 0.9 & B < .25, pathway := "A"]
# dt_bty[A > 0.9 & B > .25, pathway := "AB"]
# dt_bty[A < 0.9 & B > .25, pathway := "B"]
# 
# table(dt_bty[group == "RT3"]$pathway)