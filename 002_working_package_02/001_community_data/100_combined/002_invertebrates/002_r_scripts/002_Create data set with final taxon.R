# -------------------------------------------- #
### --- Create data set with final taxon --- ### 
# -------------------------------------------- #

# date: 23.06.20 

# setup -------------------------------------------------------------------
pacman::p_load(here, dplyr, data.table, magrittr, sf, tmap, ggplot2)
setwd(here("003_processed_data"))

# load data  --------------------------------------------------------------
set_all  <- readRDS("001_2020-06-18_all_mzb_combined.RDS")
phylum   <- readRDS("001_2020-06-18_taxon_list_phylum.RDS")
class_t  <- readRDS("001_2020-06-18_taxon_list_class.RDS")
subclass <- readRDS("001_2020-06-18_taxon_list_subclass.RDS")
order_t  <- readRDS("001_2020-06-18_taxon_list_order.RDS")
family   <- readRDS("001_2020-06-18_taxon_list_family.RDS")
genus    <- readRDS("001_2020-06-18_taxon_list_genus.RDS")

# clean data  -------------------------------------------------------------
set_all_sf  <- st_as_sf(set_all)
set_all_sf$x.coord <- st_coordinates(set_all_sf)[,1] 
set_all_sf$y.coord <- st_coordinates(set_all_sf)[,2] 
set_all <- set_all_sf
setDT(set_all) 

# Create Lists  -----------------------------------------------------------

# Note: there is a more commented version of this text in the 002_Inspect_thresholds_with_higher_resolution.R script 

# set cut off data_sets
requiered_data_sets <- 15
requiered_percent   <- 85

# how many data sets per phylum?
data_sets_per_phylum <- c()
for (i in 1:nrow(phylum)) {
        loop_p <- phylum$phylum_name[i]
        data_sets_per_phylum[i]    <-
                set_all[phylum == loop_p, length(unique(data.set))]
        
}
phyl_keep   <- phylum$phylum_name[which(data_sets_per_phylum >= requiered_data_sets)]
set_all_mod <- set_all[phylum %in% phyl_keep | is.na(phylum)]
# class
data_sets_per_class <- c()
for (i in 1:nrow(class_t)) {
        loop_p <- class_t$class_name[i]
        data_sets_per_class[i]    <-
                set_all[class == loop_p, length(unique(data.set))]
        
}
keep_id     <- which(data_sets_per_class >= requiered_data_sets)
(class_keep <- class_t$class_name[keep_id])
set_all_mod <- set_all_mod[class %in% class_keep | is.na(class)]
class_t[class_name %in% class_keep]
class_id   <- which(class_t[class_name %in% class_keep, (species + genus + family + order + subclass) < requiered_percent])
class_taxa <- class_t[class_id, class_name]
set_all_mod[class %in% class_taxa, c("final_taxon", "final_taxon_level") := .(class, "class")]
#subclass
set_all_mod <- set_all_mod[subclass != "Pteriomorphia" | is.na(subclass)]
subclass_id <- which(subclass[, (species + genus + family + order) < requiered_percent])
subclass_taxa <- subclass[subclass_id, subclass_name]
added_subclasses <- c()
for (i in seq_along(subclass_taxa)) {
        if (!subclass_taxa[i] %in% set_all_mod$subclass) {
                print(paste(subclass_taxa[i], "skipped"))
                next()
        }
        set_all_mod[subclass == subclass_taxa[i] &
                            is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(subclass, "subclass")]
        added_subclasses <-
                append(added_subclasses, subclass_taxa[i])
        print(paste(subclass_taxa[i], "added"))
}
#order
order_id    <- which(order_t[, (species + genus + family) < requiered_percent])
order_taxa  <- order_t[order_id, order_name]
added_order <- c()
for (i in seq_along(order_taxa)) {
        # check if still present?
        if (!order_taxa[i] %in% set_all_mod$order) {
                print(paste(
                        order_taxa[i],
                        "skipped because not present anymore"
                ))
                next()
        }
        if (order_t[order_name == order_taxa[i], subclass_name] %in% added_subclasses) {
                print(paste(
                        order_taxa[i],
                        "skipped because already as suborder"
                ))
                next()
        }
        
        set_all_mod[order == order_taxa[i] &
                            is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(order, "order")]
        added_order <- append(added_order, order_taxa[i])
        print(paste(order_taxa[i], "added"))
        
        
}
#family
family_id    <- family[, (species + genus) <= requiered_percent]
family_taxa  <- family[family_id, family_name]
added_family <- c()
for (i in seq_along(family_taxa)) {
        # check if still present?
        if (!family_taxa[i] %in% set_all_mod$family) {
                print(paste(
                        family_taxa[i],
                        "skipped because not present anymore"
                ))
                next()
        }
        if (family[family_name == family_taxa[i], subclass_name] %in% added_subclasses) {
                print(paste(
                        family_taxa[i],
                        "skipped because already as suborder"
                ))
                next()
        }
        if (family[family_name == family_taxa[i], order_name] %in% added_order) {
                print(paste(
                        family_taxa[i],
                        "skipped because already as order"
                ))
                next()
        }
        
        set_all_mod[family == family_taxa[i] &
                            is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(family, "family")]
        added_family <- append(added_family, family_taxa[i])
        print(paste(family_taxa[i], "added"))
        
        
}
genus_id   <- genus[, species <= requiered_percent]
species_id <- genus[, species > requiered_percent]

genus_taxa     <- genus[genus_id, genus_name]
species_taxa   <- genus[species_id, genus_name]

added_genus <- c()
for (i in seq_along(genus_taxa)) {
        if (!genus_taxa[i] %in% set_all_mod$genus) {
                print(paste(
                        genus_taxa[i],
                        "skipped because not present anymore"
                ))
                next()
        }
        if (genus[genus_name == genus_taxa[i], subclass_name] %in% added_subclasses) {
                print(paste(
                        genus_taxa[i],
                        "skipped because already as suborder"
                ))
                next()
        }
        if (genus[genus_name == genus_taxa[i], order_name] %in% added_order) {
                print(paste(genus_taxa[i], "skipped because already as order"))
                next()
        }
        if (genus[genus_name == genus_taxa[i], family_name] %in% added_family) {
                print(paste(
                        genus_taxa[i],
                        "skipped because already as family"
                ))
                next()
        }
        
        set_all_mod[genus == genus_taxa[i] &
                            is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(genus, "genus")]
        added_genus <- append(added_genus, genus_taxa[i])
        print(paste(genus_taxa[i], "added"))
        
}
for (i in seq_along(species_taxa)) {
        if (!species_taxa[i] %in% set_all_mod$genus) {
                print(paste(
                        species_taxa[i],
                        "skipped because not present anymore"
                ))
                next()
        }
        if (genus[genus_name == species_taxa[i], subclass_name] %in% added_subclasses) {
                print(paste(
                        genus_taxa[i],
                        "skipped because already as suborder"
                ))
                next()
        }
        if (genus[genus_name == species_taxa[i], order_name] %in% added_order) {
                print(paste(genus_taxa[i], "skipped because already as order"))
                next()
        }
        if (genus[genus_name == species_taxa[i], family_name] %in% added_family) {
                print(paste(
                        genus_taxa[i],
                        "skipped because already as family"
                ))
                next()
        }
        if (genus[genus_name == species_taxa[i], genus_name] %in% added_genus) {
                print(paste(genus_taxa[i], "skipped because already as genus"))
                next()
        }
        set_all_mod[genus == species_taxa[i] &
                            is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(species, "species")]
        added_genus <- append(added_genus, genus_taxa[i])
        print(paste(genus_taxa[i], "added"))
        
}

# add genus and family manually 
for (x in c("genus", "family")) {
        fun_sub  <- set_all_mod[is.na(final_taxon), unique(get(x))]
        fun_sub  <- na.omit(fun_sub)
        set_all_mod[is.na(final_taxon) & get(x) %in% fun_sub, c("final_taxon", "final_taxon_level") := .(get(x), x)]
        rm(fun_sub)
}


set_all_mod[is.na(final_taxon)]
set_all_mod2 <- set_all_mod[!is.na(final_taxon_level)]
set_all_mod2_sites <- unique(set_all_mod2, by = "gr_sample_id")


# save to file  -----------------------------------------------------------
saveRDS(set_all_mod2, paste0("002_", Sys.Date(), "_all_inv_",requiered_data_sets,"_",requiered_percent,".RDS"))
saveRDS(set_all_mod2_sites, paste0("002_", Sys.Date(), "_all_inv_sites",requiered_data_sets,"_",requiered_percent,".RDS"))
