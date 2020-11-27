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
ls_mzb = readRDS(file.path(DIR$pd, "07_indicator_list.RDS"))

if (!"x_ls_thesholds" %in% ls()){
        source(
                textConnection(
                        readLines(
                                file.path(DIR$rs, 
                                          "08_a_ta_master.R")
                        )[c(25:27)]
                )
        )
}
# Deriving initial TAs ---------------------------------------------------
ls_mzb$spe = ls_mzb$spe[B > x_ls_thesholds$spe$b | (A > x_ls_thesholds$spe$a & B > x_ls_thesholds$spe$b2 )]
ls_mzb$gen = ls_mzb$gen[B > x_ls_thesholds$gen$b | (A > x_ls_thesholds$gen$a & B > x_ls_thesholds$spe$b2 )]
ls_mzb$foh = ls_mzb$foh[B > x_ls_thesholds$foh$b | (A > x_ls_thesholds$foh$a & B > x_ls_thesholds$spe$b2 )] 

dt_mzb <- rbindlist(ls_mzb)


