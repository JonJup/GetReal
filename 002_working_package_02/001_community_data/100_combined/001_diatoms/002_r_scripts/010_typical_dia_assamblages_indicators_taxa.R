# --------------------------------------- #
### --- Typical assemblages Diatoms --- ### 
# --------------------------------------- #

#date written\modified : 04.08.20
#date used: 04.08.20, 05.08. 18.08
#Jonathan Jupke
#Diatoms GetReal WP2 

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, magrittr, dplyr, indicspecies, stringr, here)
setwd(here())

# load data  --------------------------------------------------------------
spe = readRDS("003_processed_data/009_2020-08-18diatom_species_w_rare_w_LSRT.RDS")
gen = readRDS("003_processed_data/009_2020-08-18diatom_genus_w_rare_w_LSRT.RDS")
# spr = readRDS("003_processed_data/010_2020-08-05diatom_species_wo_rare_w_LSRT.RDS.RDS")
# ger = readRDS("003_processed_data/010_2020-08-05diatom_genus_wo_rare_w_LSRT.RDS.RDS")



# join data ---------------------------------------------------------------

spe_names <- names(spe)[-(1:5)]
gen_names <- names(gen)[-(1:5)]

all <- spe[gen, on = "gr_sample_id"]
all[, c("i.nn_distance", "i.ls_bd_20", "i.WSO1_ID", "i.geometry" ) := NULL]

# Remove NAs 
for (j in seq_len(ncol(all))) set(all, which(is.na(all[[j]])), j, 0)

# test that taxa vector will work 
ncol(all) == 5 + n_spe + n_gen
rm(spe, gen, j );gc()


# clean data --------------------------------------------------------------
# How well a stream type is represented was decided visually based on sample site /stream type maps

# Comments: 
# 10 Actually has a lot of sites but they are almost all in France 
#-> Heavy bias 
#-> Could be moved to medium  
# 15 Similar to 10. Many streams do not fall into GR Countries. 
# -> Could be moved to medium

medium_types <- paste0("RT", c(1, 2, 3, 4, 5, 6, 8, 9, 12, 16, 17, 18, 19))
# bad_types    <- paste0("RT", c(7, 10, 11, 15))

# Turn river type into factor. 
all$ls_bd_20 %<>% factor

# subset data sets according to the visual categorization of stream type representation
all   <- all[ls_bd_20 %in% medium_types]

# before I run the analysis I have to drop the empty factor levels. If not this
# causes NaN and a completely NA sign matrix.
all$ls_bd_20 %<>% droplevels()

# compute indval ----------------------------------------------------------

# increased number of permutations to get smaller p-values 
#data_sets <- c("spe_medium","gen_medium", "spr_medium", "ger_medium")
data_sets <- c("all")
#data_sets <- c("spr_medium", "ger_medium")

for (k in 1:length(data_sets)) {

        print(paste("K", k, "start @", Sys.time()))
        loop_data <- get(data_sets[k])
        
        for (i in 1:20) {
                print(paste("i", i, "start @", Sys.time()))
                i_print <- ifelse(i < 10, paste0("0",i), i)
                group_var <- paste0("RT", i)
                #if (nrow(mzb[ls_bd_20 == group_var]) < thresh) next()
                if (nrow(loop_data[ls_bd_20 == group_var]) == 0) next()
                assign(x = paste0(data_sets[k],"_sc_", i_print),
                       value = indicators(X = loop_data[,-c(1:5)],
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

files_all <- ls()[which(str_detect(ls(), "all_sc_[0-9]*$"))]
loop_list_all <- list()

lists_vector <- c("loop_list_all")
files_vector <- c("files_all")

overal_output_list <- list(
                           "loop_list_all" = list()
                           )


for (k in 1:length(data_sets)) {
        
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


loop_list_all <- overal_output_list[[1]] 
loop_dt_all   <- rbindlist(loop_list_all)

spe <- loop_dt_all[taxon %in% spe_names]
gen <- loop_dt_all[taxon %in% gen_names]

nrow(spe) + nrow(gen) == nrow(loop_dt_all)


# save to file ------------------------------------------------------------
setwd("003_processed_data/")

saveRDS(object = spe, file = paste0("010_",Sys.Date(),"_indicator_spe_medi.RDS"))
saveRDS(object = gen, file = paste0("010_",Sys.Date(),"_indicator_gen_medi.RDS"))


if (readline("Delete all? ") == "yes") rm(list = ls())

