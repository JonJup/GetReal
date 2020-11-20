# --------------------------------------- #
### --- Derive Typical assemblages  --- ### 
# --------------------------------------- #

#date written\modified : 01.04.20, 30.06. 05.11
#date used: 30.06.20, 01.07., 03.07, 03.11, 04.11
#Jonathan Jupke
# GetReal
# Working Package 2 
# Macroinvertebrates

# output: 06_indicator_fol.RDS
# output: 06_indicator_gen.RDS
# output: 06_indicator_spe.RDS

# load data  --------------------------------------------------------------
# load family speciesXsites with ls broad types 
dt_spe = readRDS(file.path(DIR$pd, "005_2020-11-04obs_low_impact_w_LS20_spe.RDS"))
dt_gen = readRDS(file.path(DIR$pd, "005_2020-11-04obs_low_impact_w_LS20_gen.RDS"))
dt_fol = readRDS(file.path(DIR$pd, "005_2020-11-04obs_low_impact_w_LS20_foh.RDS"))

# join data ---------------------------------------------------------------
dt_spe <- dt_spe[,(c(2:24,26)) := NULL]
dt_gen <- dt_gen[,(c(2:24,26)) := NULL]
dt_fol <- dt_fol[,(c(2:24,26)) := NULL]

ch_names_spe <- names(dt_spe)[-c(1,2)]
ch_names_gen <- names(dt_gen)[-c(1,2)]
ch_names_fol <- names(dt_fol)[-c(1,2)]

dt_all = dt_spe[dt_gen, on = "gr_sample_id"]
dt_all = dt_all[dt_fol, on = "gr_sample_id"]

dt_all[is.na(ls_bd_20) & !is.na(i.ls_bd_20), ls_bd_20 := i.ls_bd_20]
dt_all[is.na(ls_bd_20) & !is.na(i.ls_bd_20.1), ls_bd_20 := i.ls_bd_20.1]
dt_all[, c("i.ls_bd_20", "i.ls_bd_20.1") := NULL]

# Remove NAs 
for (j in seq_len(ncol(dt_all))) set(dt_all, which(is.na(dt_all[[j]])), j, 0)

# test that taxa vector will work 
rm(dt_spe, dt_gen, dt_fol, j);gc()

# clean data --------------------------------------------------------------
# How well a stream type is represented was decided visudt_ally based on sample site /stream type maps
good_types   <- paste0("RT", c(4,5,9:13, 16))
medium_types <- paste0("RT", c(1:3, 8, 14, 15, 18))
accepted_types = c(good_types,medium_types)

# subset data sets according to the visual categorization of stream type representation
dt_all <- dt_all[ls_bd_20 %in% accepted_types]

rm(accepted_types, good_types, medium_types)

dt_all = dt_all[ls_bd_20 != "RT13"]

# from round 1 
dt_all[ls_bd_20 %in% c("RT8", "RT9")  , ls_bd_20 := "RT8_9"]
dt_all[ls_bd_20 %in% c("RT10", "RT11"), ls_bd_20 := "RT10_11"]
dt_all[ls_bd_20 %in% c("RT15", "RT16"), ls_bd_20 := "RT15_16"]
dt_all[ls_bd_20 %in% c("RT2", "RT3")  , ls_bd_20 := "RT2_3"]
dt_all[ls_bd_20 %in% c("RT4", "RT5")  , ls_bd_20 := "RT4_5"]
dt_all[ls_bd_20 %in% c("RT10_11", "RT15_16"), ls_bd_20 := "RT10_11_15_16"]
dt_all[ls_bd_20 %in% c("RT8_9", "RT10_11_15_16"), ls_bd_20 := "RT8_9_10_11_15_16"]

# dt_all[ls_bd_20 %in% c("RT10_11", "RT15_16"), ls_bd_20 := "RT10_11_15_16"]
# dt_all[ls_bd_20 %in% c("RT2", "RT3")  , ls_bd_20 := "RT2_3"]
# dt_all[ls_bd_20 %in% c("RT4", "RT5")  , ls_bd_20 := "RT4_5"]
# dt_all[ls_bd_20 %in% c("RT8_9", "RT10_11_15_16"), ls_bd_20 := "RT8_9_10_11_15_16"]
# dt_all[ls_bd_20 %in% c("RT2_3", "RT8_9_10_11_15_16"), ls_bd_20 := "RT2_3_8_9_10_11_15_16"]
# # from round 2
# dt_all[ls_bd_20 %in% c("RT10", "RT11"), ls_bd_20 := "RT10_11"]
# dt_all[ls_bd_20 %in% c("RT4", "RT5"), ls_bd_20 := "RT4_5"]
# # from round 3 
# dt_all[ls_bd_20 %in% c("RT2", "RT3"), ls_bd_20 := "RT2_3"]
# dt_all[ls_bd_20 %in% c("RT10_11", "RT8_9"), ls_bd_20 := "RT8-11"]
# # from round 4 
# dt_all[ls_bd_20 %in% c("RT8-11", "RT15_16"), ls_bd_20 := "RT8-11_15_16"]


# before I run the analysis I have to drop the empty factor levels. If not this
# causes NaN and a completly NA sign matrix.
# Turn river type into factor. 
dt_all$ls_bd_20 %<>% factor
dt_all$ls_bd_20 %<>% droplevels

ch_rt = unique(dt_all$ls_bd_20)

# compute indval ----------------------------------------------------------

# increased number of permutations to get smdt_aller p-values 

ch_data_sets <- c("dt_all")

# for (k in seq_along(ch_data_sets)) {
#         print(paste("K", k, "start @", Sys.time()))
#         loop_data <- get(ch_data_sets[k])
loop_data = dt_all
for (i in seq_along(ch_rt)) {
        group_var <- ch_rt[i]
        print(paste(group_var, "start @", Sys.time()))
        
        if (nrow(loop_data[ls_bd_20 == group_var]) == 0)
                next()
        
        assign(
                #x = paste0(ch_data_sets[k], "_sc_", group_var),
                x = paste0("dt_all_", group_var),
                value = indicators(
                        X = loop_data[, -c(1:2)],
                        cluster = loop_data$ls_bd_20,
                        group = group_var,
                        max.order = 1,
                        verbose = FALSE,
                        At = 0,
                        Bt = 0,
                        func = "IndVal.g",
                        control = how(nperm = 1)
                )
        )
        print(paste(group_var, "ended @", Sys.time()))
        rm(group_var, i )
}  
#         print(paste("K", k, "end @", Sys.time()))
# }; beepr::beep()
rm(loop_data)

ch_files = ls()[which(str_detect(ls(), "dt_all_RT[0-9].*$"))]
ls_output <- list()
for (i in seq_along(ch_files)) {
        dt_loop = get(ch_files[i])
        dt_loop =
                data.table(
                        taxon   = row.names(dt_loop[[7]]),
                        A       = dt_loop[[9]],
                        B       = dt_loop[[10]]
                        #sqrtIV  = dt_loop[[11]],
                        #p_value = dt_loop[[12]]
                )
        
        # dt_loop[, n_taxa := unlist(lapply(
        #         str_extract_all(
        #         string = dt_loop$taxon, pattern = "\\+"),
        # FUN = length))]
        # files_loop2[, n_taxa := n_taxa + 1]
        dt_loop[, group := str_extract(ch_files[i], pattern = "RT[0-9].*")]
        
        ls_output[[i]] <- dt_loop
        rm(dt_loop, i)
}

dt_bind   <- rbindlist(ls_output)

dt_spe  <- dt_bind[taxon %in% ch_names_spe]
dt_gen  <- dt_bind[taxon %in% ch_names_gen]
dt_fol  <- dt_bind[taxon %in% ch_names_fol]

ch_qs = ifelse(nrow(dt_bind) == sum(nrow(dt_spe), nrow(dt_gen), nrow(dt_fol)), "passed", "failed")
print(paste("Quality check", ch_qs))
rm(ch_qs)

# save to file ------------------------------------------------------------

if(OPT$save_one) {
        saveRDS(object = dt_spe,   file = file.path(DIR$pd, paste0("06_indicator_spe.RDS")))
        saveRDS(object = dt_gen,   file = file.path(DIR$pd, paste0("06_indicator_gen.RDS")))
        saveRDS(object = dt_fol,   file = file.path(DIR$pd, paste0("06_indicator_fol.RDS")))
}

rm_files = ls()[grepl(pattern = "dt_", x = ls())]
rm(list = rm_files)
rm_files = ls()[grepl(pattern = "ch_", x = ls())]
rm(list = rm_files)
rm_files = ls()[grepl(pattern = "ls_", x = ls())]
rm(list = rm_files)
rm(rm_files)

print("#--------------------------------------------------------#")
# if (readline("delete dt_all? ") == "yes") rm(list = ls())
