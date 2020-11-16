        ### --- Create species X sites tables --- ### 

# date written/modified: 19.06.2020 01.07 + 14.09
# date used: ..., 01.07.20, 03.07.20 + 14.09 + 03.11
#Jonathan Jupke 

# 01. Setup  --------------------------------------------------------------

pacman::p_load(data.table,
               dplyr,
               fuzzySim,
               here,
               magrittr,
               stringr)

dir_pd = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/")

# read in and prepare data ------------------------------------------------
# load data 
mzb  = readRDS(file.path(dir_pd, "004_2020-11-03_mzb_data1585_low_impact.RDS"))
# trim names 
mzb[, final_taxon := str_trim(final_taxon, side = "both")]

unique(mzb$final_taxon) %>% sort

mzb[final_taxon == "Stratiomyiidae", final_taxon := "Stratiomyidae"]

mzb$gr_sample_id %<>% as.character()
general_subset <- mzb[(is.na(year) | year >= 2000)]


# 03. Drop columns --------------------------------------------------------

bio2_all <-  general_subset[,.(gr_sample_id, final_taxon, final_taxon_level)]
# in the last run (03.07.20) I did not consider seasonal stuff 
# bio2_spr <-      sub_spring[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_sum <-      sub_summer[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_aut <-      sub_autumn[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_win <-      sub_winter[,.(gr_sample_id, final_taxon, final_taxon_level)]

rm(general_subset)

## -- different levels 
bio2_all_spe <- bio2_all[final_taxon_level == "species"]
bio2_all_gen <- bio2_all[final_taxon_level == "genus"]
bio2_all_foh <- bio2_all[final_taxon_level %in% c("family", "order", "subclass")]
bio2_all_spe[, final_taxon_level := NULL] 
bio2_all_gen[, final_taxon_level := NULL] 
bio2_all_foh[, final_taxon_level := NULL]

rm(bio2_all)

# 04. Turn to site X species matrix --------------------------------------------------------
bio2_all_spe %<>% splist2presabs(sites.col = 1, sp.col = 2)
bio2_all_gen %<>% splist2presabs(sites.col = 1, sp.col = 2)
bio2_all_foh %<>% splist2presabs(sites.col = 1, sp.col = 2)

# in the last run (03.07.20) I did not consider seasonal stuff 
# bio2_spr %<>% splist2presabs(sites.col = 1, sp.col = 2) 
# bio2_sum %<>% splist2presabs(sites.col = 1, sp.col = 2) 
# bio2_aut %<>% splist2presabs(sites.col = 1, sp.col = 2) 
# bio2_win %<>% splist2presabs(sites.col = 1, sp.col = 2) 

# 05. remove rare species/ sites --------------------------------------------------------

# 1% cutoff
nu_rate_cutoff = round(length(unique(mzb$gr_sample_id)) * 0.01)

# -- rare taxa -- #
rare_id_all_spe      <- which(colSums(x = bio2_all_spe[,-1]) < nu_rate_cutoff) + 1
rare_id_all_gen      <- which(colSums(x = bio2_all_gen[,-1]) < nu_rate_cutoff) + 1
rare_id_all_foh      <- which(colSums(x = bio2_all_foh[,-1]) < nu_rate_cutoff) + 1

rare_names_all_spe   <- names(rare_id_all_spe) %>% str_replace("\\.", " ")
rare_names_all_gen   <- names(rare_id_all_gen) %>% str_replace("\\.", " ")
rare_names_all_foh   <- names(rare_id_all_foh) %>% str_replace("\\.", " ")

ls_rare = list(rare_names_all_spe, rare_names_all_gen, rare_names_all_foh)
saveRDS(ls_rare, file.path(dir_pd, "0x_rare_taxa_list.RDS"))


bio2_all_spe <- bio2_all_spe[, -rare_id_all_spe]
bio2_all_gen <- bio2_all_gen[, -rare_id_all_gen]
bio2_all_foh <- bio2_all_foh[, -rare_id_all_foh]

rm(list = ls()[grepl(pattern = "rare", x = ls())])

# -- low richness sites -- #
# in the last run (03.07.20) I did not drop sites with low richness. This is artifically increased by me splitting the dataset into different taxon levels.
# low_taxon_site_id        <- which(rowSums(x = bio2_all_spe[,-1]) < 5)
# low_taxon_site_names_all <- bio2_all$gr_sample_id[low_taxon_site_id]
# bio2_all                 <- bio2_all[-low_taxon_site_id,]
# 
# # # species spring 
# rare_species_id            <- which(colSums(x = bio2_spr[,-1]) < 5) + 1
# rare_species_names_spr     <- names(rare_species_id) %>% str_replace("\\.", " ")
# bio2_spr                 <- bio2_spr[, -rare_species_id]
# low_taxon_site_id          <- which(rowSums(x = bio2_spr[,-1]) < 5)
# low_taxon_site_names_spr   <- bio2_spr$gr_sample_id[low_taxon_site_id]
# bio2_spr                 <- bio2_spr[-low_taxon_site_id,]
# 
# # # species summer 
# rare_species_id            <- which(colSums(x = bio2_sum[,-1]) < 5) + 1
# rare_species_names_sum     <- names(rare_species_id) %>% str_replace("\\.", " ")
# bio2_sum                 <- bio2_sum[, -rare_species_id]
# low_taxon_site_id          <- which(rowSums(x = bio2_sum[,-1]) < 5)
# low_taxon_site_names_sum   <- bio2_sum$gr_sample_id[low_taxon_site_id]
# bio2_sum                 <- bio2_sum[-low_taxon_site_id,]
# 
# # # species autumn
# rare_species_id            <- which(colSums(x = bio2_aut[,-1]) < 5) + 1
# rare_species_names_aut     <- names(rare_species_id) %>% str_replace("\\.", " ")
# bio2_aut                 <- bio2_aut[, -rare_species_id]
# low_taxon_site_id          <- which(rowSums(x = bio2_aut[,-1]) < 5)
# low_taxon_site_names_aut   <- bio2_aut$gr_sample_id[low_taxon_site_id]
# bio2_aut                 <- bio2_aut[-low_taxon_site_id,]
# 
# # # species winter 
# rare_species_id            <- which(colSums(x = bio2_win[,-1]) < 5) + 1
# rare_species_names_win     <- names(rare_species_id) %>% str_replace("\\.", " ")
# bio2_win                 <- bio2_win[, -rare_species_id]
# low_taxon_site_id          <- which(rowSums(x = bio2_win[,-1]) < 5)
# low_taxon_site_names_win   <- bio2_win$gr_sample_id[low_taxon_site_id]
# bio2_win                 <- bio2_win[-low_taxon_site_id,]

# 07. Extract site list  --------------------------------------------------

# I save them in seperate files for the look up table.  
sites_bio2_all_spe <- as.character(bio2_all_spe$gr_sample_id)
sites_bio2_all_gen <- as.character(bio2_all_gen$gr_sample_id)
sites_bio2_all_foh <- as.character(bio2_all_foh$gr_sample_id)

# in the last run (03.07.20) I did not consider seasonal stuff 
# sites_bio2_spr <- as.character(bio2_spr$gr_sample_id)
# sites_bio2_sum <- as.character(bio2_sum$gr_sample_id)
# sites_bio2_aut <- as.character(bio2_aut$gr_sample_id)
# sites_bio2_win <- as.character(bio2_win$gr_sample_id)

# 08. Save data to file ---------------------------------------------------

bio2_files = ls()[grepl(x = ls(), pattern = "bio2_")]

#bio2_files = bio2_files[-1]

for (i in seq_along(bio2_files)) {
        save.name = paste0(bio2_files[i],".RDS")
        save.file = get(bio2_files[i])
        saveRDS(
                object = save.file,
                file = file.path(dir_pd, 
                                 paste0("001_speciesXsites_tables/",
                              Sys.Date(),
                              "_",
                              save.name))
        )
} 
