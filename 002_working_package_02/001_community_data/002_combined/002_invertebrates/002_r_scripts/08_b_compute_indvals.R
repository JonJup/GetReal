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
ls_mzb = readRDS(file.path(DIR$pd, "06_sxs_w_LS20.RDS"))

ls_names = list()

# join data ---------------------------------------------------------------
ls_mzb$spe[,(c(2:24,26,27)) := NULL]
ls_mzb$gen[,(c(2:24,26,27)) := NULL]
ls_mzb$foh[,(c(2:24,26,27)) := NULL]

ls_names$spe <- names(ls_mzb$spe)[-c(1,2)]
ls_names$gen <- names(ls_mzb$gen)[-c(1,2)]
ls_names$foh <- names(ls_mzb$foh)[-c(1,2)]

if(any(ls_names$spe %in% ls_names$gen) |
   any(ls_names$foh %in% ls_names$gen) |
   any(ls_names$foh %in% ls_names$spe)) {
        print("quality check failed")
}


dt_all = ls_mzb$spe[ls_mzb$gen, on = "gr_sample_id"]
dt_all = dt_all[ls_mzb$foh, on = "gr_sample_id"]

dt_all[is.na(ls_bd_20) & !is.na(i.ls_bd_20), ls_bd_20 := i.ls_bd_20]
dt_all[is.na(ls_bd_20) & !is.na(i.ls_bd_20.1), ls_bd_20 := i.ls_bd_20.1]
dt_all[, c("i.ls_bd_20", "i.ls_bd_20.1") := NULL]

# Remove NAs 
for (j in seq_len(ncol(dt_all))) set(dt_all, which(is.na(dt_all[[j]])), j, 0)

# test that taxa vector will work 
rm(j, ls_mzb);gc()

# clean data --------------------------------------------------------------
# How well a stream type is represented was decided visudt_ally based on sample site /stream type maps
ch_g   = paste0("RT", c(4,5,9:13, 16))
ch_m   = paste0("RT", c(1:3, 8, 14, 15, 18))
ch_acc = c(ch_g,ch_m)

# subset data sets according to the visual categorization of stream type representation
dt_all <- dt_all[ls_bd_20 %in% ch_acc]

rm(ch_acc, ch_g, ch_m)
if (!"x_ls_combine" %in% ls()){
        source(
                textConnection(
                        readLines(
                                file.path(DIR$rs, 
                                          "08_a_ta_master.R")
                        )[29]
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

# from round 1 
# dt_all[ls_bd_20 %in% c("RT8", "RT9")  , ls_bd_20 := "RT8_9"]
# dt_all[ls_bd_20 %in% c("RT10", "RT11"), ls_bd_20 := "RT10_11"]
# dt_all[ls_bd_20 %in% c("RT15", "RT16"), ls_bd_20 := "RT15_16"]
# dt_all[ls_bd_20 %in% c("RT2", "RT3")  , ls_bd_20 := "RT2_3"]
# dt_all[ls_bd_20 %in% c("RT4", "RT5")  , ls_bd_20 := "RT4_5"]
# dt_all[ls_bd_20 %in% c("RT10_11", "RT15_16"), ls_bd_20 := "RT10_11_15_16"]
# dt_all[ls_bd_20 %in% c("RT8_9", "RT10_11_15_16"), ls_bd_20 := "RT8_9_10_11_15_16"]

# before I run the analysis I have to drop the empty factor levels. If not this
# causes NaN and a completly NA sign matrix.
# Turn river type into factor. 
dt_all$ls_bd_20 %<>% factor
dt_all$ls_bd_20 %<>% droplevels

ch_rt = unique(dt_all$ls_bd_20)

# compute indval ----------------------------------------------------------

# increased number of permutations to get smdt_aller p-values 

# ch_data_sets <- c("dt_all")

# for (k in seq_along(ch_data_sets)) {
#         print(paste("K", k, "start @", Sys.time()))
#         loop_data <- get(ch_data_sets[k])
ls_indval = vector("list", length = length(ch_rt))
names(ls_indval) = ch_rt
for (i in seq_along(ch_rt)) {
        
        group_var <- ch_rt[i]
        print(paste(group_var, "start @", Sys.time()))
        
        if (nrow(dt_all[ls_bd_20 == group_var]) == 0)
                next()
        
        ls_indval[[i]] = indicators(
                X = dt_all[,-c(1:2)],
                cluster = dt_all$ls_bd_20,
                group = group_var,
                max.order = 1,
                verbose = FALSE,
                At = 0,
                Bt = 0,
                func = "IndVal.g",
                control = how(nperm = 1)
        )
               
        print(paste(group_var, "ended @", Sys.time()))
        rm(group_var, i )
}  

for (i in seq_along(ls_indval)) {
        dt_loop = ls_indval[[i]]
        dt_loop =
                data.table(
                        taxon   = row.names(dt_loop[[7]]),
                        A       = dt_loop[[9]],
                        B       = dt_loop[[10]]
                        #sqrtIV  = dt_loop[[11]],
                        #p_value = dt_loop[[12]]
                )
        dt_loop = dt_loop[A != 0]
        # dt_loop[, n_taxa := unlist(lapply(
        #         str_extract_all(
        #         string = dt_loop$taxon, pattern = "\\+"),
        # FUN = length))]
        # files_loop2[, n_taxa := n_taxa + 1]
        dt_loop[, group := names(ls_indval)[i]]
        
        ls_indval[[i]] <- dt_loop
        rm(dt_loop, i)
}

dt_bind   <- rbindlist(ls_indval)

ls_mzb = list()
ls_mzb$spe = dt_bind[taxon %in% ls_names$spe]
ls_mzb$gen = dt_bind[taxon %in% ls_names$gen]
ls_mzb$foh = dt_bind[taxon %in% ls_names$foh]

ch_qs = ifelse(nrow(dt_bind) == sum(nrow(ls_mzb$spe), nrow(ls_mzb$gen), nrow(ls_mzb$foh)), "passed", "failed")
print(paste("Quality check", ch_qs))
rm(ch_qs)

# save to file ------------------------------------------------------------

if(OPT$save_one) {
        saveRDS(object = ls_mzb,   file = file.path(DIR$pd, paste0("07_indicator_list.RDS")))
}

rm_files = ls()[grepl(pattern = "^dt_", x = ls())]
rm(list = rm_files)
rm_files = ls()[grepl(pattern = "^ch_", x = ls())]
rm(list = rm_files)
rm_files = ls()[grepl(pattern = "^ls_", x = ls())]
rm(list = rm_files)
rm(rm_files)

print("#--------------------------------------------------------#")
# if (readline("delete dt_all? ") == "yes") rm(list = ls())
