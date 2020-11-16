# -------------------------------------------------------- #
### --- establish optimal taxonomic resolution -02-  --- ### 
# -------------------------------------------------------- #

# written: 03.08.20
# used: 03.08.20 + 18.08 + 04.11.
# GetReal 
# Diatoms

# Setup -------------------------------------------------------------------
pacman::p_load(data.table,
               dplyr,
               here, 
               magrittr, 
               sf, 
               tmap)

dir_pd = here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/")

# data IO  ----------------------------------------------------------------
set_all  = readRDS(file.path(dir_pd, "005_2020-08-17_clean_diatoms_observations.RDS"))
order_t  = readRDS(file.path(dir_pd, "006_2020-11-04_taxon_list_order.RDS"))
family_t = readRDS(file.path(dir_pd, "006_2020-11-04_taxon_list_family.RDS"))
genus_t  = readRDS(file.path(dir_pd, "006_2020-11-04_taxon_list_genus.RDS"))

set_all <- set_all[- which(set_all$species == "Fontigonium rectangulare")]
set_all[, c("genus_check2", "genus_new2", "double_checked", "species_new", "species_clean", "comment", "species_old") := NULL]
set_all[, c("final_taxon", "final_taxon_level") := .(character(), character())]
requiered_percent   <- 75 
                
# order  ------------------------------------------------------------------
# I remove all orders that were observed less than 10 observations  
remove_orders <-  order_t[n_observations <= 10, order_name]
set_all <- set_all[!order %in% remove_orders]
               
order_id   <- which(order_t[, (species + genus + family) < requiered_percent])
rm(order_id, remove_orders, order_t); gc()

# family ------------------------------------------------------------------
family_lvl_id <- family_t[,(species + genus) < requiered_percent]
family_taxa   <- family_t[family_lvl_id, family_name]

rm(family_lvl_id, family_taxa, family_t);gc()

# genus -------------------------------------------------------------------
genus_lvl_id   <- genus_t[,species < requiered_percent]
genus_taxa     <- genus_t[genus_lvl_id, genus_name]

set_all[genus %in% genus_taxa & is.na(final_taxon_level), 
            c("final_taxon", "final_taxon_level") := .(genus, "genus") ]
## -- qa -- ## 
set_all[final_taxon_level == "genus"]

## --    -- ## 
rm(genus_lvl_id, genus_taxa);gc()

# species -----------------------------------------------------------------
species_lvl_id <- genus_t[,species >= requiered_percent]
species_taxa   <- genus_t[species_lvl_id, genus_name]

set_all[genus %in% species_taxa & is.na(final_taxon_level), 
            c("final_taxon", "final_taxon_level") := .(species, "species") ]

rm(species_lvl_id, species_taxa);gc()

# left over  --------------------------------------------------------------
set_all[is.na(final_taxon) & is.na(species) & final_taxon_level == "species" & !is.na(genus), 
            c("final_taxon", "final_taxon_level") := 
                    .(genus, "genus")]
set_all[is.na(final_taxon)]

# save to file  -------------------------------------------------------

saveRDS(set_all,
        file.path(paste0("007_", Sys.Date(), "all_dia_optimal_taxon_75.RDS")))

