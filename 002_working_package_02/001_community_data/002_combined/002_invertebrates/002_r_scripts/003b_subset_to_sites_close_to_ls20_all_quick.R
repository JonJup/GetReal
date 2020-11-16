#### --------------------------------------- ####
### --- Assign ls rivers to invertebrates --- ### 
#### --------------------------------------- ####


#date written: ? 


# 01 Setup ----------------------------------------------------------------
pacman::p_load(data.table, dplyr, magrittr, sf, purrr, here, beepr)
setwd(here("002_working_package_02/001_community_data/002_combined/002_invertebrates/"))

# 02 load data  -----------------------------------------------------------
old_sites = readRDS("003_processed_data/003_2020-07-03_mzb_sites_close_to_ls.RDS")
sites     = readRDS("003_processed_data/002_2020-11-03all_inv_sites_15_85.RDS") 
observations = readRDS("003_processed_data/002_2020-11-03all_inv_15_85.RDS") 

# carpeting ---------------------------------------------------------------
setDT(sites)
setDT(observations)
#old_sites %<>% st_drop_geometry()
sites2 <- sites[gr_sample_id %in% old_sites$gr_sample_id]
observations2 <- observations[gr_sample_id %in% sites2$gr_sample_id]

sites2        <- left_join(x = sites2,        
                           y = old_sites[,c("ls_bd_20", "gr_sample_id")],
                           by = "gr_sample_id")
observations2 <- left_join(x = observations2, 
                           y = old_sites[,c("ls_bd_20", "gr_sample_id")], 
                           by = "gr_sample_id")

# Save to File  -----------------------------------------------------------

saveRDS(object = sites2,
        file = paste0("003_processed_data/",
                      "003_",
                      Sys.Date(),
                      "_mzb_sites_close_to_ls.RDS"))

saveRDS(object = observations2,
        file = paste0("003_processed_data/003_",
                      Sys.Date(),
                      "_mzb_data_close_to_ls.RDS"))
