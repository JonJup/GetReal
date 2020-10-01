#### -------------------------------- ####
### --- Assign ls rivers to diatom --- ### 
### --------- quick version --------- ###
#### -------------------------------- ####

# date written: 30.06.20
# date used: 30.06.20
# Jonathan Jupke 

# This is the quicl version of "002_subset_to_sites_close_to_ls20_all.R".
# Instead of conducting the nearest neighbor analysis and sorting sites out by
# distance I refer to an earlier version of the result and subset to the same
# sites.

# 01 Setup ----------------------------------------------------------------
pacman::p_load(data.table, dplyr, magrittr, sf, purrr, here)
setwd(here())

# 02 load data  -----------------------------------------------------------
dia_sites <- st_read("003_processed_data/001_2020-06-29_all_dia_sites_w_org_names.gpkg")
dia_all   <- readRDS("003_processed_data/001_2020-06-29_all_dia_data_w_org_names.RDS")
old_dia_sites_ls20 <- readRDS("003_processed_data/002_2020-04-03_dia_sites_close_to_ls_large.RDS")
 
# 03 clean data -----------------------------------------------------------
old_dia_sites_ls20 %<>% st_drop_geometry()
dia_sites_join <- left_join(dia_sites, 
                            old_dia_sites_ls20,
                            by = "gr_sample_id")
dia_sites_join %<>% filter(!is.na(ls_bd_20))
dia_all <- dia_all[gr_sample_id %in% dia_sites_join$gr_sample_id]

# Save to File  -----------------------------------------------------------
saveRDS(object = dia_sites_join,
        file = paste0("003_processed_data/",
                      "002_",
                      Sys.Date(),
                      "_dia_sites_close_to_ls_large.RDS"))

saveRDS(object = dia_all,
        file = paste0("003_processed_data/002_",
                      Sys.Date(),
                      "_dia_data_close_to_ls_large.RDS"))
