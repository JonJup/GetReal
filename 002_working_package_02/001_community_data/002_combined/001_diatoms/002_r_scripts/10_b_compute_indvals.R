# --------------------------------------- #
### --- Typical assemblages Diatoms --- ### 
# --------------------------------------- #

#date written\modified : 04.11.
#date used: 04.11. 
#Jonathan Jupke
# GetReal
# Working Package 2 
# Diatoms  




# load data  --------------------------------------------------------------
ls_dia = readRDS(file.path(DIR$pd, "08_sxs.RDS"))

# join data ---------------------------------------------------------------
ls_names = list(spe = names(ls_dia$spe)[-(1:2)], 
                gen = names(ls_dia$gen)[-(1:2)])

dt_all = ls_dia$spe[ls_dia$gen, on = "gr_sample_id"]
dt_all[, c("i.ls_bd_20") := NULL]

# Remove NAs 
for (j in seq_len(ncol(dt_all))) set(dt_all, which(is.na(dt_all[[j]])), j, 0)

# test that taxa vector will work 
rm(j );gc()


# clean data --------------------------------------------------------------
# How well a stream type is represented was decided visually based on sample site /stream type maps

# Comments: 
# 10 Actually has a lot of sites but they are almost all in France 
#-> Heavy bias 
#-> Could be moved to medium  
# 15 Similar to 10. Many streams do not fall into GR Countries. 
# -> Could be moved to medium

ch_medium_types <- paste0("RT", c(1, 2, 3, 4, 5, 6, 8, 9, 12, 16, 17, 18, 19))

# subset data sets according to the visual categorization of stream type representation
dt_all   <- dt_all[ls_bd_20 %in% ch_medium_types]
if (!"x_ls_combine" %in% ls()){
        source(
                textConnection(
                        readLines(
                                file.path(DIR$rs,
                                          "10_a_ta_master.R")
                                )[30]
                        )
                )
}
for (i in seq_along(x_ls_combine)){
        loop_var = as.character(x_ls_combine[[i]])
        ch_new_name = paste0(loop_var, collapse  = "_")
        ch_new_name = paste0("RT",ch_new_name)
        loop_var = paste0("RT", loop_var)
        dt_all[ls_bd_20 %in% loop_var, ls_bd_20 := ch_new_name]
        rm(ch_new_name, loop_var)
}


# dt_all[ls_bd_20 %in% c("RT1", "RT17", "RT18", "RT19"), ls_bd_20 := "RT1_17_18_19"]
# dt_all[ls_bd_20 %in% c("RT2", "RT4"), ls_bd_20 := "RT2_4"]
# dt_all[ls_bd_20 %in% c("RT8", "RT9"), ls_bd_20 := "RT8_9"]
# dt_all[ls_bd_20 %in% c("RT8_9", "RT2_4"), ls_bd_20 := "RT2_4_8_9"]
# dt_all[ls_bd_20 %in% c("RT2_4_8_9", "RT1_17_18_19"), ls_bd_20 := "RT1_2_4_8_9_17_18_19"]
# dt_all[ls_bd_20 %in% c("RT17", "RT4"), ls_bd_20 := "RT4_17"]
# dt_all[ls_bd_20 %in% c("RT4_17", "RT2"), ls_bd_20 := "RT2_4_17"]
# dt_all[ls_bd_20 %in% c("RT2_4_17", "RT1"), ls_bd_20 := "RT1_2_4_17"]
# dt_all[ls_bd_20 %in% c("RT1_2_4_17", "RT18"), ls_bd_20 := "RT1_2_4_17_18"]
# dt_all[ls_bd_20 %in% c("RT8", "RT19"), ls_bd_20 := "RT8_19"]
# dt_all[ls_bd_20 %in% c("RT1_2_4_17_18", "RT8_19"), ls_bd_20 := "RT_large"]
# dt_all[ls_bd_20 %in% c("RT16"), ls_bd_20 := "RT_large"]
# dt_all[ls_bd_20 %in% c("RT5"), ls_bd_20 := "RT_large"]

## -- quality check -- ## 
# if (dt_all[ls_bd_20 %in% c("RT17", "RT4", "RT2", "RT1", "RT18", "RT8", "RT19"), .N] != 0 |
#     dt_all[ls_bd_20 %in% c("RT_large"), .N] == 0) {
#         print("quality check failed")
# } else {
#         print("quality check passed")
# }

# before I run the analysis I have to drop the empty factor levels. If not this
# causes NaN and a completely NA sign matrix.
# Turn river type into factor. 
dt_all$ls_bd_20 %<>% factor
dt_all$ls_bd_20 %<>% droplevels()
ch_river_types = as.character(unique(dt_all$ls_bd_20))


# compute indval ----------------------------------------------------------

# increased number of permutations to get smaller p-values 

ls_groups = list()

for (i in seq_along(ch_river_types)) {
        
        group_var = ch_river_types[i]
        print(paste(group_var, "start @", format(Sys.time(), format="%H:%M:%S")))

        if (dt_all[ls_bd_20 == group_var, .N] == 0)
                next()

        ls_groups[[i]] = indicators(
                X         = dt_all[, -c(1:2)],
                cluster   = dt_all$ls_bd_20,
                group     = group_var,
                max.order = 1,
                verbose   = FALSE,
                At        = 0,
                Bt        = 0,
                func      = "IndVal.g",
                control   = how(nperm = 1)
        )
        
        names(ls_groups)[i] = group_var
        
        print(paste(group_var, "ended @", format(Sys.time(), format="%H:%M:%S")))
        
        rm(group_var)

}

for (i in seq_along(ls_groups)) {
        files_loop  <- ls_groups[[i]]
        files_loop <-
                data.table(
                        taxon   = row.names(files_loop[[7]]),
                        A       = files_loop[[9]],
                        B       = files_loop[[10]]
                        # sqrtIV  = files_loop[[11]],
                        # p_value = files_loop[[12]]
                )
        files_loop[, group := names(ls_groups)[i]]
        files_loop = files_loop[A != 0]
        ls_groups[[i]] <- files_loop
}

dt_groups = rbindlist(ls_groups)

## -- quality check -- ## 
# if (dt_groups[group %in% c("RT17", "RT4", "RT2", "RT1", "RT18", "RT8", "RT19"), .N] != 0 |
#     dt_groups[group %in% c("RT_large"), .N] == 0) {
#         print("quality check failed")
# } else {
#         print("quality check passed")
# }

ls_dia$spe = dt_groups[taxon %in% ls_names$spe]
ls_dia$gen = dt_groups[taxon %in% ls_names$gen]
## -- quality check -- ## 
if (nrow(ls_dia$spe) + nrow(ls_dia$gen) != nrow(dt_groups)) {
        print("quality check failed")
} else {
        print("quality check passed")
}

# save to file ------------------------------------------------------------

saveRDS(object = ls_dia, file = file.path(DIR$pd, paste0("09_indicator_list.RDS")))

# empty environment 
#if (readline("Delete all? ") == "yes") rm(list = ls())
print("#----------------------------------------#")
