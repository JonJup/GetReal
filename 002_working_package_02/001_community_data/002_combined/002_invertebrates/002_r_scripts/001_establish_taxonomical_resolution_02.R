# --------------------------------- #
### --- Combine percent lists --- ### 
# --------------------------------- #

# 15.06.20
# used: 03.07.20, 03.11.20
# Invertebrates GetReal 

# Rules Phylum and Class in all 15 data sets 
# Taxon level must keep 75% of observations 

# Setup -------------------------------------------------------------------

pacman::p_load(here, dplyr, data.table, magrittr, sf, tmap)
setwd(here(... = "002_working_package_02/001_community_data/002_combined/002_invertebrates/"))

# data IO  ----------------------------------------------------------------
set_all  <- readRDS("003_processed_data/001_2020-11-03_all_mzb_combined.RDS")
phylum   <- readRDS("003_processed_data/001_2020-11-03_taxon_list_phylum.RDS")
class_t  <- readRDS("003_processed_data/001_2020-11-03_taxon_list_class.RDS")
subclass <- readRDS("003_processed_data/001_2020-11-03_taxon_list_subclass.RDS")
order_t  <- readRDS("003_processed_data/001_2020-11-03_taxon_list_order.RDS")
family   <- readRDS("003_processed_data/001_2020-11-03_taxon_list_family.RDS")
genus    <- readRDS("003_processed_data/001_2020-11-03_taxon_list_genus.RDS")

# add x and y coordinates as variables to extract unqiue sites 
set_all_sf  <- st_as_sf(set_all)
set_all_sf$x_coord <- st_coordinates(set_all_sf)[,1]
set_all_sf$y_coord <- st_coordinates(set_all_sf)[,2]
set_all <- set_all_sf
setDT(set_all)
# st_write(set_all_sf,
#          dsn = paste0(
#                  "003_processed_data/002_", Sys.Date(),"_all_inv.gpkg")
# )
# st_write(set_all_sites,
#          dsn = paste0("003_processed_data/002_", Sys.Date(), "_all_inv_sites.gpkg")
# )
rm(set_all_sf)

# Create Lists  ----------------------------------------------------------- this
# code is only relevant if you want to run this as a loop 
# for (l1 in c(15)) { # BEGIN FOR-LOOP 1 OVER How many datasets must the phylum or class be in?

#      for (l2 in c(85,86)) { BEGIN FOR-LOOP 2 NESTED IN 1 (N1) OVER Threshold for how many percent of data must be below the current level for it to continue downward.

                # set cut off data_sets (loop)
                # requiered_data_sets <- l1
                # requiered_percent   <- l2 

                # set number of requiered data sets and theshold (manually)
                requiered_data_sets <- 15
                requiered_percent   <- 85 

# Phylum ------------------------------------------------------------------
# Remove all phyla that occur in less than 'requiered_data_sets' data sets 

phyl_out <- c()
for (i in 1:nrow(phylum)) { # BEGIN FL 3 (N2) data sets per phylum
        
        loop_p <- phylum$phylum_name[i]
        phyl_out[i]    <- set_all[phylum == loop_p, length(unique(data.set))]
        
} # END FL 3

# which phyla to keep?  
(phyl_keep   <- phylum$phylum_name[which(phyl_out >= requiered_data_sets)])
# subset 'set_all' to only those observations + NAs (none present)
# set_all[is.na(phylum)]                
set_all_mod <- set_all[phylum %in% phyl_keep | is.na(phylum)]

# quality check - only 'phyl_keep' elements? 
set_all_mod[, unique(phylum)]
rm(phyl_keep, phyl_out, loop_p);gc()

# Class -------------------------------------------------------------------
class_out <- c()
for (i in 1:nrow(class_t)) { # BEGIN FL 4 (N2) data sets per class
        
        loop_p <- class_t$class_name[i]
        class_out[i]    <- set_all[class == loop_p, length(unique(data.set))]
        
}# END FL 4 (N2) data sets per class
# Which classes to keep 
(class_keep <- class_t$class_name[which(class_out >= requiered_data_sets)])
# subset 'set_all' to only those observations + NAs (none present)
set_all_mod[is.na(class)]
set_all_mod <- set_all_mod[class %in% class_keep | is.na(class)]

# how many observations are at class level? 
class_t[class_name %in% class_keep]
# none 
(class_id <- which(class_t[class_name %in% class_keep, (species + genus + family + order + subclass) < requiered_percent]))
class_taxa <- class_t[class_id, class_name]
set_all_mod[class %in% class_taxa, c("final_taxon", "final_taxon_level") := .(class, "class")]

# quality check - let's have a look at the 'final_taxon' and 'final_taxon_level columns'
set_all_mod[!is.na(final_taxon),]
set_all_mod[!is.na(final_taxon_level),]
set_all_mod[,unique(class)]
rm(class_id, class_taxa, loop_p, class_out);gc()

# subclass ----------------------------------------------------------------
# Pteriomorpha is only in Edwin Peters and Landau. The sublcass contains
# Mytiulus edulis and I suspect them to be brackish. A check of the data in QGIS
# confirms this. Other data sets do not have costal waters so I remove these
# observations.

set_all_mod <- set_all_mod[subclass != "Pteriomorphia" | is.na(subclass)]

subclass_id <- which(subclass[, (species + genus + family + order) < requiered_percent])
subclass_taxa <- subclass[subclass_id, subclass_name]
set_all_mod[subclass %in% subclass_taxa & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(subclass, "subclass")]

# quality check 
set_all_mod[final_taxon_level == "subclass"]
set_all_mod[subclass == "Oligochaeta", unique(final_taxon_level)]
set_all_mod[!is.na(final_taxon_level), unique(final_taxon)]
rm(subclass_id, subclass_taxa);gc()

# order  ------------------------------------------------------------------
order_id   <- which(order_t[, (species + genus + family) < requiered_percent])
order_taxa <- order_t[order_id, order_name]
# Are any left? 
set_all_mod[order %in% order_taxa, unique(order)]
# indeed "Branchiobdellida"
set_all_mod[order %in% order_taxa & is.na(final_taxon_level), 
            c("final_taxon", "final_taxon_level") := .(order, "order") ]
# quality check 
set_all_mod[final_taxon_level == "order"]
set_all_mod[order == "Branchiobdellida", unique(final_taxon_level)]
set_all_mod[!is.na(final_taxon_level), unique(final_taxon)]
rm(order_id, order_taxa);gc()

# family ------------------------------------------------------------------
family_lvl_id <- family[,(species + genus) < requiered_percent]
family_taxa   <- family[family_lvl_id, family_name]
# Are any left? - Plenty (78) 
set_all_mod[family %in% family_taxa, unique(family)]
set_all_mod[family %in% family_taxa & is.na(final_taxon_level), 
            c("final_taxon", "final_taxon_level") := .(family, "family") ]
# All observations that are in general at species or genus level but are not highliy resolved 
set_all_mod[!(family %in% family_taxa) & is.na(species) & is.na(genus) & !is.na(family) & is.na(final_taxon),
            c("final_taxon", "final_taxon_level") := .(family, "family") ]

# quality check 
set_all_mod[family == "Chironomidae" & is.na(genus)]
set_all_mod[final_taxon_level == "family"]
set_all_mod[!is.na(final_taxon_level), unique(final_taxon)]
rm(family_lvl_id, family_taxa);gc()

# genus -------------------------------------------------------------------
genus_lvl_id   <- genus[,species < requiered_percent]
genus_taxa     <- genus[genus_lvl_id, genus_name]
# Are any left? - Plenty (842) 
set_all_mod[genus %in% genus_taxa, unique(genus)]
set_all_mod[genus %in% genus_taxa & is.na(final_taxon_level), 
            c("final_taxon", "final_taxon_level") := .(genus, "genus") ]
# All observations that are in general at species or genus level but are not highliy resolved 
set_all_mod[!(genus %in% genus_taxa) & is.na(species) & !is.na(genus) & is.na(final_taxon),
            c("final_taxon", "final_taxon_level") := .(genus, "genus") ]
# quality check 
set_all_mod[final_taxon_level == "genus"]
rm(genus_lvl_id, genus_taxa);gc()

# species -----------------------------------------------------------------
species_lvl_id <- genus[,species >= requiered_percent]
species_taxa   <- genus[species_lvl_id, genus_name]
# Are any left? - Plenty (132) 
set_all_mod[genus %in% species_taxa, unique(species)]
set_all_mod[genus %in% species_taxa & is.na(final_taxon_level), 
            c("final_taxon", "final_taxon_level") := .(species, "species") ]

rm(species_lvl_id, species_taxa);gc()

# remove what is left  ----------------------------------------------------
# Now there are some left that have no final taxon yet. They are all only at order level. This I consider too high. 

set_all_mod <- set_all_mod[!is.na(final_taxon)]

# save to file  -------------------------------------------------------

save_out  <- st_as_sf(set_all_mod)
save_out$x_coord <- st_coordinates(save_out)[,1]
save_out$y_coord <- st_coordinates(save_out)[,2]
sites_out <- unique(save_out, by = c("x_coord", "y_coord")) 

setDT(sites_out)


# st_write(obj = save_out,
#          dsn = paste0("003_processed_data/002_", Sys.Date(), "all_inv_", l1 , "_", l2, ".gpkg"))
# st_write(obj = sites_out,
#          dsn = paste0("003_processed_data/002_", Sys.Date(), "all_inv_sites_", l1 , "_", l2, ".gpkg"))
saveRDS(object = save_out,
         file = paste0("003_processed_data/002_", Sys.Date(), "all_inv_", requiered_data_sets , "_", requiered_percent, ".RDS"))
saveRDS(object = sites_out,
         file = paste0("003_processed_data/002_", Sys.Date(), "all_inv_sites_", requiered_data_sets , "_", requiered_percent, ".RDS"))

