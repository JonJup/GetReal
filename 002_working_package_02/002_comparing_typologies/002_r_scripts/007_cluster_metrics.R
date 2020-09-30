# ------------------------------------------- #
### --- Compute cluster metrics for all --- ### 
# ------------------------------------------- #

# Setup -------------------------------------------------------------------

pacman::p_load(labdsv, data.table, magrittr, dplyr, indicspecies, optpart, parallelDist)
setwd("~/01_Uni/03_GetReal/002_WP_02/002_comparing_typologies/003_processed_data/")

# load data  --------------------------------------------------------------

mzb     <- readRDS("003_2020-03-26_all_ls_mzb_with_cluster.RDS")
# diatoms <- readRDS("2020-03-03_all_dia_with_cluster.RDS")

# clean data  -------------------------------------------------------------
mzb$ls_btype12 <- as.character(mzb$ls_btype12)
mzb$ls_btype20 <- as.character(mzb$ls_btype20)
# diatoms$ls_btype12 <- as.character(diatoms$ls_btype12)
# diatoms$ls_btype20 <- as.character(diatoms$ls_btype20)

# loop --------------------------------------------------------------------

# prepare list for loop 

clustering_names <- colnames(mzb)[c(4:5)]

mzb <- mzb[ls_btype20 != "RT2"]

level_one_list <- list(
        list(),
        list()
)
# 1. loop over diatom vs invertebrat
for (i in 2) {  # BEGIN LOOP LVL 1: Select data set 
        
        # select data set 
        if (i == 1) {
                loop_data <- diatoms
        } else {
                loop_data <- mzb
        }
        
        print(paste("Selected data set", i, "@", Sys.time()))
        
        for (k in 1:length(clustering_names)) { # BEGIN LOOP LVL 2: Select Clustering 
                
                # select cluster column name 
                clustering_name_loop <- clustering_names[k]

                
                # subset to no NA
                loop_data_no_na <- loop_data[!is.na(get(clustering_name_loop))]
                
                # pull cluster column from data.table 
                clustering_ids_loop <- loop_data_no_na[,get(clustering_name_loop)]
                
                # number of cluster levels 
                n_cluster <- length(unique(clustering_ids_loop))
                
                # compute sorensen distance matrix 
                print(paste("Start computing distance", i, "- ", k, "@", Sys.time()))
                # dist_loop <- dsvdis(loop_data_no_na[,-(1:8)], 
                #                     index = "sorensen") 
                dist_loop <- parDist(x = as.matrix(loop_data_no_na[,-(1:7)]),
                                    method = "dice", 
                                    threads = 2)
                print(paste("Finished  computing distance", i, "- ", k, "@", Sys.time()))
                print(paste("Start computing Invdval", i, "- ", k, "@", Sys.time()))
                indval = multipatt(loop_data_no_na[,-(1:7)],
                                   cluster = clustering_ids_loop,
                                   control = how(nperm = 100),
                                   max.order = 1)
                inval_significat_id = which(indval$sign["p.value"] <= 0.05)
                indval_statistic = sum(apply(indval$str[inval_significat_id,1:n_cluster],
                                             1,
                                             max))
                print(paste("Finished computing IndVal", i, "- ", k, "@", Sys.time()))
                ismaic_statistic = mean(
                        isamic(
                                comm = loop_data_no_na[,-(1:7)],
                                clustering = clustering_ids_loop
                        )
                )
                disdiam_stat = disdiam(x = clustering_ids_loop,
                                       dist = dist_loop,
                )
                disdiam_stat = disdiam_stat$mean
                partana_statistic = partana(c = clustering_ids_loop,
                                            dist = dist_loop)
                partana_statistic = partana_statistic$ratio
                
                level_one_list[[i]][[k]] <- data.table(indval_statistic, 
                                              ismaic_statistic,
                                              disdiam_stat,
                                              partana_statistic)
                
                rm(partana_statistic, disdiam_stat, ismaic_statistic, indval_statistic, inval_significat_id, 
                   indval,dist_loop, n_cluster, clustering_ids_loop, clustering_name_loop, 
                   loop_data_no_na)
                gc()
        } # END LOOP LVL 2: Select Clustering
      
      
}  # END LOOP LEVEL 1: Select data set 

# zwischenergebnisse

saveRDS(level_one_list[[1]][[1]], 
        file = "Comparison/Zwischenergebnisse/diatoms_ls_btype_12.RDS")

diatom_results <- level_one_list[[1]]
out_diatoms <- rbindlist(diatom_results)
saveRDS(object = out_diatoms, 
        file = paste0("Comparison/", Sys.Date(), "_diatoms.RDS"))
mzb_restults   <- level_one_list[[2]]
out_mzb <- rbindlist(mzb_restults)
rownames(out_mzb) <- clustering_names
saveRDS(object = out_mzb, 
        file = paste0("Comparison/", Sys.Date(), "_results_mzb.RDS"))


        