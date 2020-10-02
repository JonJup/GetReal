### ------------------------------------- ### 
### --- Create species X sites tables --- ### 
### --- FOR HMSC COURSE               --- ### 
### ------------------------------------- ### 

# date: 08.07.20
# used: 08.07.20
#Jonathan Jupke 

# setup  --------------------------------------------------------------

pacman::p_load(data.table,
               fuzzySim,
               dplyr,
               magrittr,
               stringr,
               here
               )

setwd(here())

# load data  ------------------------------------------------
mzb <- readRDS("003_processed_data/HMSC/001_2020-07-08_mzb_data1585_all_impact.RDS")

# carpenting  -------------------------------------------------------------
mzb <- mzb[!is.na(genus)]

mzb$gr_sample_id %<>% as.character()
general_subset <- mzb[(is.na(year) | year >= 2000)]
bio2_all       <-  general_subset[,.(gr_sample_id, genus)]
rm(general_subset)

# turn to site X species matrix --------------------------------------------------------
bio2_all %<>% splist2presabs(sites.col = 1, sp.col = 2)

# remove rare species/ sites --------------------------------------------------------
# i.e. < 5 taxa per site or < 5 occurences in dataset 
# all  
# -- rare taxa -- #
rare_id_all      <- which(colSums(x = bio2_all[,-1]) < 5) + 1
rare_names_all   <- names(rare_id_all) %>% str_replace("\\.", " ")
bio2_all         <- bio2_all[, -rare_id_all]
rm(list = ls()[grepl(pattern = "rare", x = ls())])

# -- low richness sites -- #
low_taxon_site_id        <- which(rowSums(x = bio2_all[,-1]) < 5)
low_taxon_site_names_all <- bio2_all$gr_sample_id[low_taxon_site_id]
bio2_all                 <- bio2_all[-low_taxon_site_id,]
rm(low_taxon_site_id, low_taxon_site_names_all)


# save data to file ---------------------------------------------------
saveRDS(bio2_all, paste0("003_processed_data/HMSC/002_", Sys.Date(), "_species_x_site.RDS"))
