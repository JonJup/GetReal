# --------------------------------------- #
### --- Derive Typical assemblages  --- ### 
# --------------------------------------- #

#date written\modified : 01.04.20, 30.06. 05.11
#date used: 30.06.20, 01.07., 03.07, 03.11, 04.11
#Jonathan Jupke
# GetReal
# Working Package 2 
# Macroinvertebrates

# output: 06_2020-11-04_indicator_fol.RDS
# output: 06_2020-11-04_indicator_gen.RDS
# output: 06_2020-11-04_indicator_spe.RDS

save = FALSE

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, 
               dplyr,
               here, 
               indicspecies, 
               magrittr,   
               stringr)

dir_pd = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/")

# load data  --------------------------------------------------------------
# load family speciesXsites with ls broad types 
spe = readRDS(file.path(dir_pd, "005_2020-11-04obs_low_impact_w_LS20_spe.RDS"))
gen = readRDS(file.path(dir_pd, "005_2020-11-04obs_low_impact_w_LS20_gen.RDS"))
fol = readRDS(file.path(dir_pd, "005_2020-11-04obs_low_impact_w_LS20_foh.RDS"))

# join data ---------------------------------------------------------------
spe <- spe[,(c(2:24,26)) := NULL]
gen <- gen[,(c(2:24,26)) := NULL]
fol <- fol[,(c(2:24,26)) := NULL]

names_spe <- names(spe)[-c(1,2)]
names_gen <- names(gen)[-c(1,2)]
names_fol <- names(fol)[-c(1,2)]

sg <- spe[gen, on = "gr_sample_id"]
all <- sg[fol, on = "gr_sample_id"]

all[is.na(ls_bd_20) & !is.na(i.ls_bd_20), ls_bd_20 := i.ls_bd_20]
all[is.na(ls_bd_20) & !is.na(i.ls_bd_20.1), ls_bd_20 := i.ls_bd_20.1]
all[, c("i.ls_bd_20", "i.ls_bd_20.1") := NULL]

# Remove NAs 
for (j in seq_len(ncol(all))) set(all, which(is.na(all[[j]])), j, 0)

# test that taxa vector will work 
rm(spe, gen, fol, j ,sg);gc()


# clean data --------------------------------------------------------------
# How well a stream type is represented was decided visually based on sample site /stream type maps
good_types   <- paste0("RT", c(4,5,9:13, 16))
medium_types <- paste0("RT", c(1:3, 8, 14, 15, 18))

# Turn river type into factor. 
all$ls_bd_20 %<>% factor

# subset data sets according to the visual categorization of stream type representation
all <- all[ls_bd_20 %in% good_types | ls_bd_20 %in% medium_types]

# from round 1 
all[ls_bd_20 %in% c("RT8", "RT9")  , ls_bd_20 := "RT8_9"]
all[ls_bd_20 %in% c("RT15", "RT16"), ls_bd_20 := "RT15_16"]
# from round 2
all[ls_bd_20 %in% c("RT10", "RT11"), ls_bd_20 := "RT10_11"]
all[ls_bd_20 %in% c("RT4", "RT5"), ls_bd_20 := "RT4_5"]
# from round 3 
all[ls_bd_20 %in% c("RT2", "RT3"), ls_bd_20 := "RT2_3"]
all[ls_bd_20 %in% c("RT10_11", "RT8_9"), ls_bd_20 := "RT8-11"]
# from round 4 
all[ls_bd_20 %in% c("RT8-11", "RT15_16"), ls_bd_20 := "RT8-11_15_16"]


# before I run the analysis I have to drop the empty factor levels. If not this
# causes NaN and a completly NA sign matrix.
all$ls_bd_20 %<>% droplevels()

rm(good_types, medium_types)

# compute indval ----------------------------------------------------------

# increased number of permutations to get smaller p-values 

data_sets <- c("all")

for (k in seq_along(data_sets)) {
        print(paste("K", k, "start @", Sys.time()))
        loop_data <- get(data_sets[k])
        
        for (i in 1:20) {
                print(paste("i", i, "start @", Sys.time()))
                i_print <- ifelse(i < 10, paste0("0",i), i)
                group_var <- paste0("RT", i)
                #if (nrow(mzb[ls_bd_20 == group_var]) < thresh) next()
                if (nrow(loop_data[ls_bd_20 == group_var]) == 0) next()
                assign(x = paste0(data_sets[k],"_sc_", i_print),
                       value = indicators(X = loop_data[,-c(1:2)],
                                          cluster = loop_data$ls_bd_20, 
                                          group = group_var,
                                          max.order = 1, 
                                          verbose = FALSE,
                                          At = 0,
                                          Bt = 0,
                                          func = "IndVal.g", 
                                          control = how(nperm = 999)
                       ))
                rm(group_var)
                print(paste("i", i, "end @", Sys.time()))
        }  
        print(paste("K", k, "end @", Sys.time()))
}; beepr::beep()


files  <- ls()[which(str_detect(ls(), "all_sc_[0-9]*$"))]


loop_list_all   <- list()


lists_vector <- c("loop_list_all")
files_vector <- c("files")

overal_output_list <- list("loop_list_all" = list())


for (k in 1) {
        
        loop_list_files <- files_vector[k] 
        
        for (i in seq_along(get(loop_list_files))) {
                files_loop  <- get(loop_list_files)[i]
                files_loop2 <- data.table( taxon   = row.names(get(files_loop)[[7]]), 
                                           A       = get(files_loop)[[9]],
                                           B       = get(files_loop)[[10]], 
                                           sqrtIV  = get(files_loop)[[11]],
                                           p_value = get(files_loop)[[12]])
                
                files_loop2[, n_taxa := unlist(lapply(str_extract_all(string = files_loop2$taxon, pattern = "\\+"), 
                                                      FUN = length))]
                files_loop2[, n_taxa := n_taxa + 1]
                files_loop2[,group := paste0("RT", str_extract(files_loop,pattern = "[0-9]*$"))]
                
                overal_output_list[[k]][[i]] <- files_loop2
        }
}


loop_list_all   <- overal_output_list[[1]]


loop_dt_all   <- rbindlist(loop_list_all)


spe  <- loop_dt_all[taxon %in% names_spe]
gen  <- loop_dt_all[taxon %in% names_gen]
fol  <- loop_dt_all[taxon %in% names_fol]

nrow(loop_dt_all) == sum(nrow(gen), nrow(spe), nrow(fol))

# save to file ------------------------------------------------------------

if(save){

saveRDS(object = spe,   file = file.path(dir_pd, paste0("006_",Sys.Date(),"_indicator_spe.RDS")))
saveRDS(object = gen,   file = file.path(dir_pd, paste0("006_",Sys.Date(),"_indicator_gen.RDS")))
saveRDS(object = fol,   file = file.path(dir_pd, paste0("006_",Sys.Date(),"_indicator_fol.RDS")))

}

if (readline("delete all? ") == "yes") rm(list = ls())
