# -------------------------------------------------- #
### --- Inspect threshold with high resolution --- ###
# -------------------------------------------------- #

# date: 23.06.20 + 09.11.20

# setup -------------------------------------------------------------------
if(!require(pacman)) {install.packages("pacman");library(pacman)}
pacman::p_load(here, dplyr, data.table, magrittr, sf, tmap, ggplot2)

dir_pd = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/")

# load data  --------------------------------------------------------------
set_all  <- readRDS(file.path(dir_pd, "01_2020-11-03_all_mzb_combined.RDS"))
phylum   <- readRDS(file.path(dir_pd, "01_2020-11-03_taxon_list_phylum.RDS"))
class_t  <- readRDS(file.path(dir_pd, "01_2020-11-03_taxon_list_class.RDS"))
subclass <- readRDS(file.path(dir_pd, "01_2020-11-03_taxon_list_subclass.RDS"))
order_t  <- readRDS(file.path(dir_pd, "01_2020-11-03_taxon_list_order.RDS"))
family   <- readRDS(file.path(dir_pd, "01_2020-11-03_taxon_list_family.RDS"))
genus    <- readRDS(file.path(dir_pd, "01_2020-11-03_taxon_list_genus.RDS"))
        
# clean data  -------------------------------------------------------------
set_all_sf  <- st_as_sf(set_all)
set_all_sf$x.coord <- st_coordinates(set_all_sf)[,1] 
set_all_sf$y.coord <- st_coordinates(set_all_sf)[,2] 
set_all <- set_all_sf
setDT(set_all) 

# Create Lists  -----------------------------------------------------------
ls_loop_out <- list()
counter = 0
for (l2 in c(99:50)) {
        counter = counter + 1
        # for manual loop 
        #l2 = 85 
        # set cut off data_sets
        requiered_data_sets <- 15
        requiered_percent   <- l2
        
        # how many data sets per phylum? 
        data_sets_per_phylum <- c()
        for (i in 1:nrow(phylum)) {
                loop_p <- phylum$phylum_name[i]
                data_sets_per_phylum[i]    <- set_all[phylum == loop_p, length(unique(data.set))]
                
        }
        
        # Remove
        (phyl_keep   <-
                        phylum$phylum_name[which(data_sets_per_phylum >= requiered_data_sets)])
        # Remove all observations from other phyla while keeping all observations with Phylum = NA 
        # In the current dataset (23.06.20) there are no phylum == NA 
        set_all_mod <- set_all[phylum %in% phyl_keep | is.na(phylum)]
        
        # how many data sets per class?
        data_sets_per_class <- c()
        for (i in 1:nrow(class_t)) {
                loop_p <- class_t$class_name[i]
                data_sets_per_class[i]    <-
                        set_all[class == loop_p, length(unique(data.set))]
                
        }
        keep_id     <- which(data_sets_per_class >= requiered_data_sets)
        (class_keep <- class_t$class_name[keep_id])
        # Remove all observations from other classes while keeping all observations with Class = NA 
        set_all_mod <- set_all_mod[class %in% class_keep | is.na(class)]
        class_t[class_name %in% class_keep]
        # Should any taxa be resolved at class level? 
        class_id   <- which(class_t[class_name %in% class_keep, (species + genus + family + order + subclass) < requiered_percent])
        class_taxa <- class_t[class_id, class_name]
        set_all_mod[class %in% class_taxa, c("final_taxon", "final_taxon_level") := .(class, "class")]
        
        # subclass ----------------------------------------------------------------
        # subclass <- subclass[phylum_name %in% phyl_keep  | is.na(phylum_name)]
        # subclass <- subclass[class_name  %in% class_keep | is.na(class_name)]
        
        # Pteriomorpha is only in Edwin Peters and Landau. The sublcass contains
        # Mytiulus edulis and I suspect them to be brackish. A check of the data in QGIS
        # confirms this. Other data sets do not have costal waters so I remove these
        # observations.
        
        set_all_mod <- set_all_mod[subclass != "Pteriomorphia" | is.na(subclass)]
        
        subclass_id <- which(subclass[, (species + genus + family + order) < requiered_percent])
        subclass_taxa <- subclass[subclass_id, subclass_name]
        added_subclasses <- c()
        for (i in seq_along(subclass_taxa)) {
                if (!subclass_taxa[i] %in% set_all_mod$subclass) {
                        print(paste(subclass_taxa[i], "skipped"))
                        next()
                }
                set_all_mod[subclass == subclass_taxa[i] & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(subclass, "subclass") ]
                added_subclasses <- append(added_subclasses, subclass_taxa[i])
                print(paste(subclass_taxa[i], "added"))
        }
        
        # order  ------------------------------------------------------------------
        
        order_id    <- which(order_t[, (species + genus + family) < requiered_percent])
        order_taxa  <- order_t[order_id, order_name]
        added_order <- c() 
        for (i in seq_along(order_taxa)) {
                # check if still present? 
                if (!order_taxa[i] %in% set_all_mod$order) {
                        print(paste(order_taxa[i], "skipped because not present anymore"))
                        next()
                }
                if (order_t[order_name == order_taxa[i], subclass_name] %in% added_subclasses) {
                        print(paste(order_taxa[i], "skipped because already as suborder"))
                        next()
                }
                
                set_all_mod[order == order_taxa[i] & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(order, "order") ]
                added_order <- append(added_order, order_taxa[i])
                print(paste(order_taxa[i], "added"))
                
                
        }

        # family ------------------------------------------------------------------

        family_id   <- family[, (species + genus) <= requiered_percent]
        family_taxa <- family[family_id, family_name]

        added_family <- c() 
        for (i in seq_along(family_taxa)) {
                # check if still present? 
                if (!family_taxa[i] %in% set_all_mod$family) {
                        print(paste(family_taxa[i], "skipped because not present anymore"))
                        next()
                }
                if (family[family_name == family_taxa[i], subclass_name] %in% added_subclasses) {
                        print(paste(family_taxa[i], "skipped because already as suborder"))
                        next()
                }
                if (family[family_name == family_taxa[i], order_name] %in% added_order) {
                        print(paste(family_taxa[i], "skipped because already as order"))
                        next()
                }
                
                set_all_mod[family == family_taxa[i] & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(family, "family") ]
                added_family <- append(added_family, family_taxa[i])
                print(paste(family_taxa[i], "added"))
                
                
        }


        #lower_lvl_id  <- family[, (species + genus) >= requiered_percent]
        
        # lower_than_family_taxa   <-
        #         family[lower_lvl_id, family_name]
        # set_all_mod2 <-
        #         set_all_mod[family %in% family_taxa |
        #                             (family %in% lower_than_family_taxa &
        #                                      !is.na(species) | !is.na(genus))]
        
        # genus -------------------------------------------------------------------
        
        genus_id   <- genus[, species <= requiered_percent]
        species_id <- genus[, species > requiered_percent]
        
        genus_taxa     <- genus[genus_id, genus_name]
        species_taxa   <- genus[species_id, genus_name]
        
        added_genus <- c() 
        for (i in seq_along(genus_taxa)) {
                # check if still present? 
                if (!genus_taxa[i] %in% set_all_mod$genus) {
                        print(paste(genus_taxa[i], "skipped because not present anymore"))
                        next()
                }
                if (genus[genus_name == genus_taxa[i], subclass_name] %in% added_subclasses) {
                        print(paste(genus_taxa[i], "skipped because already as suborder"))
                        next()
                }
                if (genus[genus_name == genus_taxa[i], order_name] %in% added_order) {
                        print(paste(genus_taxa[i], "skipped because already as order"))
                        next()
                }
                if (genus[genus_name == genus_taxa[i], family_name] %in% added_family) {
                        print(paste(genus_taxa[i], "skipped because already as family"))
                        next()
                }
                
                set_all_mod[genus == genus_taxa[i] & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(genus, "genus") ]
                added_genus <- append(added_genus, genus_taxa[i])
                print(paste(genus_taxa[i], "added"))
        
        }
        for (i in seq_along(species_taxa)) {
                # check if still present? 
                if (!species_taxa[i] %in% set_all_mod$genus) {
                        print(paste(species_taxa[i], "skipped because not present anymore"))
                        next()
                }
                if (genus[genus_name == species_taxa[i], subclass_name] %in% added_subclasses) {
                        print(paste(genus_taxa[i], "skipped because already as suborder"))
                        next()
                }
                if (genus[genus_name == species_taxa[i], order_name] %in% added_order) {
                        print(paste(genus_taxa[i], "skipped because already as order"))
                        next()
                }
                if (genus[genus_name == species_taxa[i], family_name] %in% added_family) {
                        print(paste(genus_taxa[i], "skipped because already as family"))
                        next()
                }
                if (genus[genus_name == species_taxa[i], genus_name] %in% added_genus) {
                        print(paste(genus_taxa[i], "skipped because already as genus"))
                        next()
                }
                set_all_mod[genus == species_taxa[i] & is.na(final_taxon_level), c("final_taxon", "final_taxon_level") := .(species, "species") ]
                added_genus <- append(added_genus, genus_taxa[i])
                print(paste(genus_taxa[i], "added"))
                
        }
        
        set_all_mod2 <- set_all_mod[!is.na(final_taxon_level)]
        # assign(x = paste0("d", l2,"_t"), 
        #        value = data.table(taxon_level = names(table(set_all_mod2$final_taxon_level)), 
        #                       number = as.vector(table(set_all_mod2$final_taxon_level)), 
        #                       percent = as.vector(table(set_all_mod2$final_taxon_level)) / nrow(set_all_mod2), 
        #                       thresholds = l2)
        # )
        ls_loop_out[[counter]] = data.table(taxon_level = names(table(set_all_mod2$final_taxon_level)), 
                                 number = as.vector(table(set_all_mod2$final_taxon_level)), 
                                 percent = as.vector(table(set_all_mod2$final_taxon_level)) / nrow(set_all_mod2), 
                                 thresholds = l2)
                
        rm(set_all_mod2, species_taxa, genus_taxa, family_taxa, order_id, genus_lvl_id, species_lvl_id, family_lvl_id, lower_lvl_id, order_id)
        print(l2)
}
 
#loop_results <- ls()[grep(pattern = "d[0-9]*_t", x = ls())]

d_all_t <- rbindlist(ls_loop_out)

# d_all_t <- rbindlist(list(
#         d70_t,
#         d71_t,
#         d72_t,
#         d73_t,
#         d74_t,
#         d75_t,
#         d76_t,
#         d77_t,
#         d78_t,
#         d79_t,
#         d80_t,
#         d81_t,
#         d82_t,
#         d83_t,
#         d84_t,
#         d85_t,
#         d86_t,
#         d87_t,
#         d88_t,
#         d89_t,
#         d90_t,
#         d91_t,
#         d92_t,
#         d93_t,
#         d94_t,
#         d95_t
# ))


d_all_t[, total_observations := number/percent]

saveRDS(d_all_t, file = file.path(dir_pd, "01_data_threshold_plot"))

d_all_t %>% 
        #filter(taxon_level %in% c("genus", "family")) %>%  
        ggplot(aes(x = thresholds, y = number, col = taxon_level)) + 
        geom_point() + 
        geom_line() 
        #facet_wrap(.~taxon_level) + 
        #scale_y_log10()
d_all_t %>% 
        group_by(taxon_level)  %>%  
        ggplot(aes(x = thresholds, y = total_observations)) + 
        geom_point()


# Where does the drop between 77 and 78 come from? 
mzb85 <- readRDS("002_2020-06-23_all_inv_15_85.gpkg")
mzb86 <- st_read("003_processed_data/002_2020-06-18all_inv_15_86.gpkg") %>% setDT

## test if the error with multiple final_taxon_level for one taxon are still present
ftl <- sort(unique(mzb85$final_taxon))
for (i in seq_along(ftl)) { 

        ftlvl <- mzb85[final_taxon == ftl[i], unique(final_taxon_level)]
        if (length(ftlvl) > 1) print(ftl[i])        
}

 ## -> yes fixed! 


mzb86[family == "Chironomidae"]
mzb85[family == "Chironomidae"]

gr_ids <- mzb86$gr_sample_id[which(!mzb85$gr_sample_id %in% mzb86$gr_sample_id)]

drops <- mzb86[gr_sample_id %in% gr_ids]
unique(drops$family)


# Here we loose al family level observations of Chironomidae 
