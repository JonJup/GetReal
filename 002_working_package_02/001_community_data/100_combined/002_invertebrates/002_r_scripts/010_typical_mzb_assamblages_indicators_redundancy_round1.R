# -------------------------------------------------- #
### --- Typical assemblages Macroinvertebrates --- ### 
### --- Redundancy Round 1                     --- ### 
# -------------------------------------------------- #

#date written\modified : 20.08.20 + 14.09 
#date used: 20.08.20 + 14.09
#Jonathan Jupke
# GetReal WP2
# Macroinvertebrates 

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, magrittr, dplyr, indicspecies, stringr, here)
setwd(here())

# load data  --------------------------------------------------------------
# load family speciesXsites with ls broad types 
spe = readRDS("003_processed_data/005_2020-07-03obs_low_impact_w_LS20_spe.RDS")
gen = readRDS("003_processed_data/005_2020-07-03obs_low_impact_w_LS20_gen.RDS")
fol = readRDS("003_processed_data/005_2020-07-03obs_low_impact_w_LS20_foh.RDS")

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

# I removed 13 because it was redundant with 2_3 but the resulting 2_3_13 had no
# genera, or foh, only a couple (<10) species
gm_types <- paste0("RT", c(1:5, 8:12, 14:16, 18))

# subset data sets according to the visual categorization of stream type representation
all <- all[ls_bd_20 %in% gm_types]

rm(gm_types);gc()

all[ls_bd_20 %in% c("RT2", "RT3")  , ls_bd_20 := "RT2_3"]
all[ls_bd_20 %in% c("RT4", "RT5")  , ls_bd_20 := "RT4_5"]
all[ls_bd_20 %in% c("RT8", "RT9")  , ls_bd_20 := "RT8_9"]
all[ls_bd_20 %in% c("RT10", "RT11"), ls_bd_20 := "RT10_11"]
all[ls_bd_20 %in% c("RT15", "RT16"), ls_bd_20 := "RT15_16"]

# Turn river type into factor. Before I run the analysis I have to drop the
# empty factor levels. If not this causes NaN and a completely NA sign matrix.
all$ls_bd_20 %<>% factor %>% droplevels()
rtv <- sort(unique(all$ls_bd_20))

# compute IndVal ----------------------------------------------------------

data_sets <- c("all")

for (k in 1:length(data_sets)) {
        
        print(paste("K", k, "start @", Sys.time()))
        
        loop_data <- get(data_sets[k])
        
        for (i in 1:length(rtv)) {
                print(paste("i", i, "start @", Sys.time()))
                group_var <- rtv[i]
                #if (nrow(mzb[ls_bd_20 == group_var]) < thresh) next()
                if (nrow(loop_data[ls_bd_20 == group_var]) == 0) next()
                assign(x = paste0(data_sets[k],"_", group_var),
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


files_all      <- ls()[which(str_detect(ls(), "all_RT[0-9]*"))]
ll_all
lists_vector <- c("ll_all")
files_vector <- c("files_all")
overal_output_list <- list("ll_all" = list()
                           )

for (k in 1) {
        
        loop_list_files <- files_vector[k] 
        
        for (i in seq_along(get(loop_list_files))) {
                files_loop  <- get(loop_list_files)[i]
                files_loop2 <- data.table( taxon   = row.names(get(files_loop)[[7]]), 
                                           A       = get(files_loop)[[9]],
                                           B       = get(files_loop)[[10]], 
                                           sqrtIV  = get(files_loop)[[11]],
                                           p_value = get(files_loop)[[12]])
                group_var <- files_loop 
                group_var <- str_split(string = group_var, pattern = "_")
                group_var <- paste(unlist(group_var)[-1], collapse = "_")
                
                files_loop2[,group := group_var]
                
                overal_output_list[[k]][[i]] <- files_loop2
        }
}

ll_all   <- overal_output_list[[1]] %>% rbindlist


spe <- ll_all[taxon %in% names_spe]
gen <- ll_all[taxon %in% names_gen]
fol <- ll_all[taxon %in% names_fol]


# save to file ------------------------------------------------------------
setwd("003_processed_data/")

saveRDS(object = spe,   file = paste0("007_",Sys.Date(),"_indicator_spe_reduntant_1.RDS"))
saveRDS(object = gen,   file = paste0("007_",Sys.Date(),"_indicator_gen_reduntant_1.RDS"))
saveRDS(object = fol,   file = paste0("007_",Sys.Date(),"_indicator_foh_reduntant_1.RDS"))

rm(list = ls());gc()
