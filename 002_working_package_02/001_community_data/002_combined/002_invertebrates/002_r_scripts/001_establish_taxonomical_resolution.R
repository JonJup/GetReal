# --------------------------------- #
### --- Combine percent lists --- ### 
# --------------------------------- #

# 27.05.20
# used: 03.11.20 
# Invertebrates GetReal 

# Setup -------------------------------------------------------------------

pacman::p_load(here, dplyr, data.table, magrittr, taxize, beepr, stringr, purrr)
setwd(here("002_working_package_02/001_community_data/001_individual_data_sets/"))

# data IO  ----------------------------------------------------------------
# I excluded the following data sets because they contained only very few taxa 
# -> Rumania from LD 
# -> Oscar Belmar - only data set not to include Annelida 
# -> Mirela Cimpea 

set01 <- readRDS("001_ld/003_processed_data/mzb/09_2020-05-27_MZB_Landau_higher_taxa.RDS")         
set03 <- readRDS("002_mi/003_processed_data/mzb/09_2020-05-27_MZB_Miguel_Iglesias_PA_high_phyla.RDS")                   
set04 <- readRDS("002_mi/003_processed_data/mzb/09_2020-05-27_MZB_Miguel_Iglesias_PA_high_phyla.RDS")                   
set05 <- readRDS("004_na/003_processed_data/mzb/09_2020-05-27_MZB_Naiades_high_phyl.RDS")                               
set06 <- readRDS("005_pb/003_processed_data/09_2020-05-28_MZB_canta_Pepe_high_phyla.RDS")                           
set07 <- readRDS("005_pb/003_processed_data/09_2020-05-28_MZB_Picos_Pepe_high_phyla.RDS")                           
set08 <- readRDS("007_ds/003_processed_data/09_2020-05-28_MZB_Denes_Schmera_high_phyl.RDS")                         
set09 <- readRDS("009_hd/003_processed_data/mzb/09_2020-05-28_MZB_Ecosurv_high_phyla.RDS")                              
set10 <- readRDS("011_rp/003_processed_data/09_2020-05-28_MZB_Rivpacs_high_phyla.RDS")                              
set11 <- readRDS("012_cf/003_processed_data/mzb/09_2020-05-28_MZB_STARS_high_phyla.RDS")                                
set12 <- readRDS("012_cf/003_processed_data/mzb/09_2020-05-28_MZB_mzb_WISER_High_phyla.RDS")                            
set14 <- readRDS("014_sk/003_processed_data/mzb/09_2020-05-28_MZB_Leonard_Sandin_high_phyl.RDS")                      
set15 <- readRDS("015_kh/003_processed_data/09_2020-05-28_MZB_Kaisa-Leena_Huttunen_high_phylum.RDS")              
set16 <- readRDS("016_pp/003_processed_data/09_2020-05-28_MZB_Philippe_Usseglio_Polatera_high_phyla.RDS")  
set17 <- readRDS("019_ep/003_processed_data/mzb/09_200515_MZB_ediwn_peters_high_phyla.RDS")                    
set18 <- readRDS("020_ph/003_processed_data/09_200518_MZB_peter_haase_high_phyla.RDS")                     

## -- prepare data for combination 

# set DT 
ch_files <- ls()


for(i in seq_along(ch_files)) {
        setDT(get(ch_files[i]))
}

# Edwin Peters and Peter Haase data set still contains n. columns which need to be removed 
set17[, c("n.species", "n.genus", "n.family", "n.order") := NULL]
set18[, c("n.species", "n.genus", "n.family", "n.order") := NULL]

# Data sets where date = NA have a different class in that column than those with actual dates. 
# To avoid having to check each one separately I made sure each date is of class date. 
set03[, date := as.Date(date)]
set04[, date := as.Date(date)]
set06[, date := as.Date(date)]
set07[, date := as.Date(date)]
set08[, date := as.Date(date)]
set09[, date := as.Date(date)]
set10[, date := as.Date(date)]
set11[, date := as.Date(date)]
set12[, date := as.Date(date)]
set14[, date := as.Date(date)]
set15[, date := as.Date(date)]
set16[, date := as.Date(date)]
set17[, date := as.Date(date)]
set18[, date := as.Date(date)]

## -- combine data sets 
set_all <- rbindlist(list(
        set01,
        set03,
        set04,
        set05,
        set06,
        set07,
        set08,
        set09,
        set10,
        set11,
        set12,
        set14,
        set15,
        set16,
        set17,
        set18
                          ))


# change hydranida 
set_all[genus == "Hydraenida", genus := NA]

rm(list = ch_files)

# Create Lists  -----------------------------------------------------------
## -- Manual pre checks -- ## 

unique(set_all$phylum) %>% sort
unique(set_all$class)  %>% sort

## -- species without genus -- ## 
species_without_genus <- set_all[!is.na(species) & is.na(genus), unique(species)]

for (i in seq_along(species_without_genus)) {
        set_all[species == species_without_genus[i], genus := word(species_without_genus[i],1)]
        print(i)
}

## -- Small fixes -- ## 
set_all[family   == "Aeolosomatidae",      class    := "Polychaeta"]
set_all[genus    == "Bothrioneurum",       family   := "Naididae"]
set_all[genus    == "Bothrioneurum",       order    := "Haplotaxida"]
set_all[genus    == "Bothrioneurum",       subclass := NA]
set_all[genus    == "Bothrioneurum",       class    := "Clitellata"]
set_all[genus    == "Bothrioneurum",       phylum    := "Annelida"]
set_all[order    == "Diplostraca",         subclass := "Phyllopoda"]
set_all[family   == "Dorydrilidae",        order    := "Haplotaxida"]
set_all[family   == "Dorydrilidae",        class    := "Clitellata"]
set_all[genus    == "Dugesia",             class    := "Turbellaria"]
set_all[order    == "Enchytraeida",        class    := "Clitellata"]
set_all[subclass == "Hirudinea",           class    := "Clitellata"]
set_all[genus    == "Gammarus",            family   := "Gammaridae"]
set_all[genus    == "Gammarus",            order    := "Amphipoda"]
set_all[genus    == "Gammarus",            subclass := "Eumalacostraca"]
set_all[genus    == "Gammarus",            class    := "Malacostraca"]
set_all[genus    == "Gammarus",            phylum   := "Arthropoda"]
set_all[genus    == "Leptocerus",          family   := "Leptoceridae"]
set_all[genus    == "Leptocerus",          order    := "Trichoptera"]
set_all[family   == "Libellulinae",        family   := "Libellulidae"]
set_all[family   == "Libellulidae",        order    := "Odonata"]
set_all[genus    == "Lumbricus",           family   := "Lumbricidae"]
set_all[genus    == "Lumbricus",           order    := "Crassiclitellata"]
set_all[family   == "Lymnaeidae",          subclass := "Heterobranchia"]
set_all[order    == "Mytilida",            subclass := "Pteriomorphia"]
set_all[subclass == "Oligochaeta",         genus    := NA]
set_all[subclass == "Oligochaeta",         family   := NA]
set_all[subclass == "Oligochaeta",         order    := NA]
set_all[genus    == "Oxygastra",           family   := "Synthemistidae"]
set_all[family   == "Physidae",            subclass := "Heterobranchia"]
set_all[genus    == "Piguetiella",         family   := "Randiellidae"]
set_all[genus    == "Piguetiella",         order    := "Enchytraeida"]
set_all[genus    == "Piguetiella",         class    := "Clitellata"]
set_all[genus    == "Sperchon",            family   := "Sperchonidae"]
set_all[genus    == "Sperchon",            order    := "Trombidiformes"]
set_all[genus    == "Sperchon",            subclass := "Acari"]
set_all[genus    == "Sperchon",            class    := "Arachnida"]
set_all[genus    == "Sperchon",            phylum   := "Arthropoda"]
set_all[order    == "Tricladida",          class    := "Turbellaria"]

## -- Check for site duplicates 

# new dataset without duplicates 

set_all <- unique(set_all , by = c("gr_sample_id", "species", "genus", "family", "order", "subclass", "class", "phylum"))

# # - example site with duplicate 
# set_all[gr_sample_id == "site_02818_date_04375_mzb_Landau"]
# setorderv(set_all, cols = c("species", "genus", "family", "order"))
# setorderv(set_all2, cols = c("species", "genus", "family", "order"))
# controll_sites <- set_all$gr_sample_id[runif(n = 10, min = 1, max = nrow(set_all))]
# set_all[gr_sample_id == controll_sites[1]] # 13 + 14 == Dytiscidae 
# set_all[gr_sample_id == controll_sites[2]] # 1:3 == Chironomidae 
# set_all[gr_sample_id == controll_sites[3]] # clean 
# set_all[gr_sample_id == controll_sites[4]] # 1 + 2 == Chironomidae 
# set_all2[gr_sample_id == controll_sites[1]] # 13 + 14 == Dytiscidae 
# set_all2[gr_sample_id == controll_sites[2]] # 1:3 == Chironomidae 
# set_all2[gr_sample_id == controll_sites[3]] # clean 
# set_all2[gr_sample_id == controll_sites[4]] # 1 + 2 == Chironomidae 
# set_all <- set_all2 
# rm(set_all2)

###### ------ BEGIN PHYLUM ------ ######   
        n_phyl <- length(set_all[,unique(phylum)])
        
        level_data_phylum <- data.table(phylum_name       = character(n_phyl), 
                                        species           = numeric(n_phyl),
                                        genus             = numeric(n_phyl),
                                        family            = numeric(n_phyl),
                                        order             = numeric(n_phyl),
                                        subclass          = numeric(n_phyl),
                                        class             = numeric(n_phyl),
                                        phylum            = numeric(n_phyl),
                                        n_observations    = numeric(n_phyl))
        
        for (i in 1:n_phyl) {
                
                level_data_phylum[i, phylum_name := set_all[,unique(phylum)][i]]
                
                loop_sub <- set_all[phylum == set_all[,unique(phylum)][i]]
                loop_obs <- nrow(loop_sub)
                level_data_phylum[i, species        := round(loop_sub[!is.na(species),.N] / loop_obs * 100,2)]
                level_data_phylum[i, genus          := round(loop_sub[ is.na(species) & !is.na(genus), .N] / loop_obs * 100,2)]
                level_data_phylum[i, family         := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),.N] / loop_obs * 100,2)]
                level_data_phylum[i, order          := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order), .N] / loop_obs * 100,2)]
                level_data_phylum[i, subclass       := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) & !is.na(subclass),.N] / loop_obs * 100,2)]
                level_data_phylum[i, class          := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) &  is.na(subclass) & !is.na(class), .N] / loop_obs * 100,2)]
                level_data_phylum[i, phylum         := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) &  is.na(subclass) &  is.na(class) & !is.na(phylum), .N] / loop_obs * 100,2)]
                level_data_phylum[i, n_obseravtions := nrow(loop_sub)]
                
        }
###### ------ END PHYLUM ------ ######
###### ------ BEGIN CLASS ------ ######
        n_class <- length(set_all[!is.na(class),unique(class)])
        
        level_data_class <- data.table(
                phylum_name      = character(n_class),
                class_name       = character(n_class),
                species          = numeric(n_class),
                genus            = numeric(n_class),
                family           = numeric(n_class),
                order            = numeric(n_class),
                subclass         = numeric(n_class),
                class            = numeric(n_class),
                n_observations   = numeric(n_class)
        )
        
        for (i in 1:n_class) {
                
                loop_class <- set_all[!is.na(class),unique(class)][i]
                
                level_data_class[i, class_name :=  loop_class]
                level_data_class[i, phylum_name := set_all[class == class_name, unique(phylum)]]
                
                loop_sub <- set_all[class == loop_class]
                loop_obs <- nrow(loop_sub)
                
                level_data_class[i, species        := round(loop_sub[!is.na(species),.N] / loop_obs * 100,2)]
                level_data_class[i, genus          := round(loop_sub[ is.na(species) & !is.na(genus),.N] / loop_obs * 100,2)]
                level_data_class[i, family         := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),.N] / loop_obs * 100,2)]
                level_data_class[i, order          := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),.N] / loop_obs * 100,2)]
                level_data_class[i, subclass       := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) & !is.na(subclass),.N] / loop_obs * 100,2)]
                level_data_class[i, class          := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) &  is.na(subclass) & !is.na(class),.N] / loop_obs * 100,2)]
                level_data_class[i, n_observations := nrow(loop_sub)]
                }
        setorderv(level_data_class, cols = c("phylum_name", "class_name"))
###### ------ END CLASS ------ ######
###### ------ BEGIN SUBCLASS ------ ######
        n_subclass <- length(set_all[!is.na(subclass), unique(subclass)])
        
        level_data_subclass <- data.table(
                phylum_name      = character(n_subclass),
                class_name       = character(n_subclass),
                subclass_name    = character(n_subclass),
                species          = numeric(n_subclass),
                genus            = numeric(n_subclass),
                family           = numeric(n_subclass),
                order            = numeric(n_subclass),
                subclass         = numeric(n_subclass),
                n_observations   = numeric(n_subclass)
        )
        
        for (i in 1:n_subclass) {
                
                loop_subclass <- set_all[!is.na(subclass),unique(subclass)][i]
                
                level_data_subclass[i, subclass_name := loop_subclass]
                level_data_subclass[i, class_name    := set_all[subclass == loop_subclass, unique(class)]]
                level_data_subclass[i, phylum_name   := set_all[subclass == subclass_name, unique(phylum)]]
                
                loop_sub <- set_all[subclass == loop_subclass]
                loop_obs <- nrow(loop_sub)
                
                level_data_subclass[i, species        := round(loop_sub[!is.na(species),.N] / loop_obs * 100,2)]
                level_data_subclass[i, genus          := round(loop_sub[ is.na(species) & !is.na(genus),.N] / loop_obs * 100,2)]
                level_data_subclass[i, family         := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),.N] / loop_obs * 100,2)]
                level_data_subclass[i, order          := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),.N] / loop_obs * 100,2)]
                level_data_subclass[i, subclass       := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) & !is.na(subclass),.N] / loop_obs * 100,2)]
                level_data_subclass[i, n_observations := nrow(loop_sub)]
        }
        
        setorderv(level_data_subclass, cols = c("phylum_name", "class_name", "subclass_name"))
###### ------ END SUBCLASS ------ ######
###### ------ BEGIN ORDER ------ ###### 
        n_order <- length(set_all[!is.na(order), unique(order)])
        
        level_data_order <- data.table(
                phylum_name    = character(n_order),
                class_name     = character(n_order),
                subclass_name  = character(n_order),
                order_name     = character(n_order),
                species        = numeric(n_order),
                genus          = numeric(n_order),
                family         = numeric(n_order),
                order          = numeric(n_order),
                n_observations = numeric(n_order)
        )
        
        for (i in 1:n_order) {
                
                loop_order <- set_all[!is.na(order),unique(order)][i]
                
                level_data_order[i, order_name    := loop_order]
                level_data_order[i, subclass_name := set_all[order == loop_order, unique(subclass) ]]
                level_data_order[i, class_name    := set_all[order == loop_order, unique(class)]]
                level_data_order[i, phylum_name   := set_all[order == loop_order, unique(phylum)]]
                
                loop_sub <- set_all[order == loop_order]
                loop_obs <- nrow(loop_sub)
                
                level_data_order[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100,2)]
                level_data_order[i, genus          := round(loop_sub[ is.na(species) & !is.na(genus), .N] / loop_obs * 100,2)]
                level_data_order[i, family         := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family), .N] / loop_obs * 100,2)]
                level_data_order[i, order          := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order), .N] / loop_obs * 100,2)]
                level_data_order[i, n_observations := nrow(loop_sub)]
        }
        setorderv(level_data_order, cols = c("phylum_name", "class_name", "subclass_name", "order_name"));beep()
        
###### ------ END ORDER ------ ######
###### ------ BEGIN FAMILY ------ ###### 
        n_family <- length(set_all[!is.na(family), unique(family)])
        
        level_data_family <- data.table(
                phylum_name    = character(n_family),
                class_name     = character(n_family),
                subclass_name  = character(n_family),
                order_name     = character(n_family),
                family_name    = character(n_family),
                species        = numeric(n_family),
                genus          = numeric(n_family),
                family         = numeric(n_family),
                n_observations = numeric(n_family)
                
        )
        
        for (i in 1:n_family) {
                
                loop_family <- set_all[!is.na(family),unique(family)][i]
                
                level_data_family[i, family_name   := loop_family]
                level_data_family[i, order_name    := set_all[family == loop_family, unique(order)]]
                level_data_family[i, subclass_name := set_all[family == loop_family, unique(subclass) ]]
                level_data_family[i, class_name    := set_all[family == loop_family, unique(class)]]
                level_data_family[i, phylum_name   := set_all[family == loop_family, unique(phylum)]]
                
                loop_sub <- set_all[family == loop_family]
                loop_obs <- nrow(loop_sub)
                
                level_data_family[i, species        := round(loop_sub[!is.na(species),.N] / loop_obs * 100,2)]
                level_data_family[i, genus          := round(loop_sub[ is.na(species) & !is.na(genus),.N] / loop_obs * 100,2)]
                level_data_family[i, family         := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),.N] / loop_obs * 100,2)]
                level_data_family[i, n_observations := nrow(loop_sub)]
        }
        setorderv(level_data_family, cols = c("phylum_name", "class_name", "subclass_name", 
                                              "order_name", "family_name"));beep()

###### ------ END FAMILY ------ ######
###### ------ BEGIN GENUS ------ ######

n_genus <- length(set_all[!is.na(genus), unique(genus)])

level_data_genus <- data.table(
        phylum_name    = character(n_genus),
        class_name     = character(n_genus),
        subclass_name  = character(n_genus),
        order_name     = character(n_genus),
        family_name    = character(n_genus),
        genus_name     = character(n_genus),
        species          = numeric(n_genus),
        genus            = numeric(n_genus),
        n_observations   = numeric(n_genus)
)

for (i in 1:n_genus) {
        
        loop_genus <- set_all[!is.na(genus),unique(genus)][i]
        
        level_data_genus[i, genus_name    :=  loop_genus]
        level_data_genus[i, family_name   := set_all[genus == loop_genus, unique(family)]]
        level_data_genus[i, order_name    := set_all[genus == loop_genus, unique(order)]]
        level_data_genus[i, subclass_name := set_all[genus == loop_genus, unique(subclass) ]]
        level_data_genus[i, class_name    := set_all[genus == loop_genus, unique(class)]]
        level_data_genus[i, phylum_name   := set_all[genus == loop_genus, unique(phylum)]]
        
        loop_sub <- set_all[genus == loop_genus]
        loop_obs <- nrow(loop_sub)
        
        level_data_genus[i, species        := round(loop_sub[!is.na(species),.N] / loop_obs * 100,2)]
        level_data_genus[i, genus          := round(loop_sub[is.na(species) & !is.na(genus),.N] / loop_obs * 100,2)]
        level_data_genus[i, n_observations := nrow(loop_sub)]
        print(paste(i,n_genus))
} 
setorderv(level_data_genus, cols = c("phylum_name", "class_name", 
                                     "subclass_name", "order_name", "family_name", "genus_name")); beep()

###### ------ END GENUS ------ ######
###### ------ BEGIN SPECIES ------ ######

n_species <- length(set_all[!is.na(species), unique(species)])

level_data_species <- data.table(
        phylum_name    = character(n_species),
        class_name     = character(n_species),
        subclass_name  = character(n_species),
        order_name     = character(n_species),
        family_name    = character(n_species),
        genus_name     = character(n_species),
        species_name   = character(n_species),
        species          = numeric(n_species),
        n_observations   = numeric(n_species)
)

for (i in 1:n_species) {
        
        loop_species <- set_all[!is.na(species),unique(species)][i]
        
        level_data_species[i, species_name  :=  loop_species]
        level_data_species[i, genus_name    := set_all[species == loop_species, unique(genus)]]
        level_data_species[i, family_name   := set_all[species == loop_species, unique(family)]]
        level_data_species[i, order_name    := set_all[species == loop_species, unique(order)]]
        level_data_species[i, subclass_name := set_all[species == loop_species, unique(subclass) ]]
        level_data_species[i, class_name    := set_all[species == loop_species, unique(class)]]
        level_data_species[i, phylum_name   := set_all[species == loop_species, unique(phylum)]]
        
        loop_sub <- set_all[species == loop_species]
        loop_obs <- nrow(loop_sub)
        
        level_data_species[i, species        := round(loop_sub[!is.na(species),.N] / loop_obs * 100,2)]
        level_data_species[i, n_observations := nrow(loop_sub)]
        print(paste(i,n_species))
}
setorderv(level_data_genus, cols = c("phylum_name", "class_name", "subclass_name", 
                                     "order_name", "family_name", "genus_name", "species_name")); beep()


###### ------ END SPECIES ------ ######

# Save to file  -----------------------------------------------------------
setwd(here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"))
saveRDS(object = set_all,             file = paste0("001_", Sys.Date(), "_all_mzb_combined.RDS"))
saveRDS(object = level_data_genus,    file = paste0("001_", Sys.Date(), "_taxon_list_genus.RDS"))
saveRDS(object = level_data_family,   file = paste0("001_", Sys.Date(), "_taxon_list_family.RDS"))
saveRDS(object = level_data_order,    file = paste0("001_", Sys.Date(), "_taxon_list_order.RDS"))
saveRDS(object = level_data_subclass, file = paste0("001_", Sys.Date(), "_taxon_list_subclass.RDS"))
saveRDS(object = level_data_class,    file = paste0("001_", Sys.Date(), "_taxon_list_class.RDS"))
saveRDS(object = level_data_phylum,   file = paste0("001_", Sys.Date(), "_taxon_list_phylum.RDS"))
