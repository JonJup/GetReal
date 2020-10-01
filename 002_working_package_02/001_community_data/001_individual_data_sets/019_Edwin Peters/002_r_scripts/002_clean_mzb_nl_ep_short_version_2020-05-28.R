# ----------------------------------- #
# --- Clean Edwin Peters MZB data --- # 
# --------- Short Version ----------  # 
# ----------------------------------- #

# Jonathan Jupke 
# date: 28.05.2020

# In this script I create a harmonized spatial data set from the raw data
# provided by Edwin Peters.

# EPSG based on projfinder: 28992 or 4258 ? 

## -- OVERVIEW -- ## 
# 01. Setup
# 02. Clean data
# 03. Taxonomic Cleaning
## -------------- ##

# 01. Setup -------------------------------------------------------------------

pacman::p_load(
        here,
        taxize,
        data.table,
        sf,
        dplyr,
        stringr,
        lubridate,
        readr,
        purrr
)
# other required packages: lubridate
setwd(here("019_Edwin Peters"))

data2      <- readRDS("03_FinalData/01_2020-05-28_data_before_taxon_clean_mzb_Edwin_Peters.RDS")
taxontable <- readRDS("03_FinalData/03_191129_initial_taxon_clean_mzb_Edwin Peters.RDS")

data3 = left_join(data2,
                  taxontable, 
                  by = "taxon") %>% 
        setDT

data3 <- data3[year >= 2000]

# Highlevel Taxonomic Groups  ---------------------------------------------
what_family <- list()
what_order  <- list()
# list to hold each order in data sets and the respective taxize results. 
source("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/add_higher_taxa.R")

# load previous solutions 
pre_order <- readRDS("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/data_fix_ord.RDS")
pre_famil <- readRDS("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/data_fix_fam.RDS")

for (i in 1:nrow(pre_order)) {
        if (pre_order[i,1] %in% c(data3$order)) {
                ids <- which(data3$order == as.character(pre_order[i,1]))
                if (NA %in% data3[ids, unique(kingdom)]) {
                        data3[ids, "subclass"] <- pull(pre_order[i,2])
                        data3[ids, "class"]    <- pull(pre_order[i,3])
                        data3[ids, "phylum"]   <- pull(pre_order[i,4])
                        data3[ids, "kingdom"]  <- pull(pre_order[i,5])
                }
        }
}
for (i in 1:nrow(pre_famil)) {
        if (pre_famil[i,1] %in% c(data3$family)) {
                if (NA %in% data3[ids, unique(kingdom)]) {
                        ids <- which(data3$family == pull(pre_famil[i,1]))
                        data3[ids, "subclass"] <- pull(pre_order[i,2])
                        data3[ids, "class"]    <- pull(pre_order[i,3])
                        data3[ids, "phylum"]   <- pull(pre_order[i,4])
                        data3[ids, "kingdom"]  <- pull(pre_order[i,5])
                }
        }
}

(yes_order <- sort(data3[!is.na(order) & is.na(kingdom), unique(order)]))
(yes_famil <- sort(data3[!is.na(family) & is.na(kingdom), unique(family)]))

## -- clean up 
unique(data3$kingdom)
unique(data3$phylum) %>% sort
unique(data3$class) %>% sort
unique(data3$subclass) %>% sort

data3[kingdom == "NA"]
data3[is.na(kingdom), unique(taxon)]

## -- there is tons of stuff still not determined so we need to fix this .... 
TU2 <- data3[is.na(kingdom), unique(taxon)]
for (i in 1:length(TU2)) {
        if (str_detect(string = TU2[i], pattern = "_indet$")) {
               irem <-  str_remove(string = TU2[i], pattern = "_indet")
               TU2[i] <- irem
        }
}
TU2 <- sort(TU2)
tu_list <- list()
for (j in 1:length(TU2)) {
        loop_taxon <- TU2[j]
        what_family[[j]] <- classification(loop_taxon, "eol")
}
# broke at Rhithrogena
which(TU2 == "Rhithrogena")
for (j in 240:length(TU2)) {
        loop_taxon <- TU2[j]
        what_family[[j]] <- classification(loop_taxon, "eol")
}
# which have not worked? 
na_id <- c()
for (i in 1:length(TU2)) {
        if(is.na(what_family[[i]])){
                na_id <- append(na_id, i)        
        }
}

# fix them by "hand" 
TU2[na_id]

# ok now for the rest of them 
for (i in 229:length(what_family)) {
        print(i)
        loop_object <- what_family[[i]][[1]]
        # skip if na 
        if (is.na(loop_object)) next()
        data3_ids   <- which(data3$taxon == TU2[i])
        if ("genus"    %in% loop_object[,2]) data3[data3_ids, genus    := str_split_fixed(loop_object[which(loop_object[,2] == "genus")   ,1], "\\s", 2)[,1] ]
        if ("family"   %in% loop_object[,2]) data3[data3_ids, family   := str_split_fixed(loop_object[which(loop_object[,2] == "family")  ,1], "\\s", 2)[,1] ]
        if ("order"    %in% loop_object[,2]) data3[data3_ids, order    := str_split_fixed(loop_object[which(loop_object[,2] == "order")   ,1], "\\s", 2)[,1] ]
        if ("subclass" %in% loop_object[,2]) data3[data3_ids, subclass := str_split_fixed(loop_object[which(loop_object[,2] == "subclass"),1], "\\s", 2)[,1] ]
        if ("class"    %in% loop_object[,2]) data3[data3_ids, class    := str_split_fixed(loop_object[which(loop_object[,2] == "class")   ,1], "\\s", 2)[,1] ]
        if ("phylum"   %in% loop_object[,2]) data3[data3_ids, phylum   := str_split_fixed(loop_object[which(loop_object[,2] == "phylum")  ,1], "\\s", 2)[,1] ]
        if ("kingdom"  %in% loop_object[,2]) data3[data3_ids, kingdom  := str_split_fixed(loop_object[which(loop_object[,2] == "kingdom") ,1], "\\s", 2)[,1] ]
        
        print(i)
        
}
# Amphibalanus  - fixed 
# Balanus       - fixed 
# Hiatella      - fixed  
# Medicorophium - fixed 
# Microdeutopus - fixed 
# Monocorophium - fixed 
# Mya           - fixed 
# Porcellio     - fixed 


## -- clean up 
# - kingdom 
unique(data3$kingdom)
data3[kingdom == "Fungi"]
data3[kingdom == "Metazoa", kingdom := "Animalia"]
data3[kingdom == "Archaeplastida", unique(taxon)]
data3 <- data3[kingdom != "Fungi"]

unique(data3$phylum) %>% sort
unique(data3$class) %>% sort
unique(data3$subclass) %>% sort

data3[subclass == "", subclass := NA]

data3[species == "NA", species := NA]
data3[genus == "NA", genus  := NA]
data3[family == "NA", family := NA]
data3[order == "NA", order := NA]

data4 = data3

saveRDS(data4, paste0("03_FinalData/05b_",Sys.Date(),"_final_taxon_join_mzb_edwin_peters_high_phyla.RDS"))
data4 = readRDS("03_FinalData/05b")

# Determine Levels for Taxa -----------------------------------------------

## -- Phylum 
n_phyl <- length(data4[,unique(phylum)])

level_data_phylum <- data.table(phylum_name = character(n_phyl), 
                                species  = numeric(n_phyl),
                                genus    = numeric(n_phyl),
                                family   = numeric(n_phyl),
                                order    = numeric(n_phyl),
                                subclass = numeric(n_phyl),
                                class    = numeric(n_phyl),
                                phylum   = numeric(n_phyl), 
                                data_set = "EP")

for (i in 1:n_phyl) {
        
        level_data_phylum[i, phylum_name := data4[,unique(phylum)][i]]
        
        loop_sub <- data4[phylum == data4[,unique(phylum)][i]]
        loop_obs <- nrow(loop_sub)
        level_data_phylum[i, species   := round(loop_sub[!is.na(species),                                                                                                      .N]/ loop_obs * 100,2)]
        level_data_phylum[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                                                                                      .N]/ loop_obs * 100,2)]
        level_data_phylum[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),                                                                     .N]/ loop_obs * 100,2)]
        level_data_phylum[i, order     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),                                                     .N]/ loop_obs * 100,2)]
        level_data_phylum[i, subclass  := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) & !is.na(subclass),                                  .N]/ loop_obs * 100,2)]
        level_data_phylum[i, class     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) &  is.na(subclass) & !is.na(class),                  .N]/ loop_obs * 100,2)]
        level_data_phylum[i, phylum    := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) &  is.na(subclass) &  is.na(class) & !is.na(phylum), .N]/ loop_obs * 100,2)]
        
        
}

## -- Class 
n_class <- length(data4[!is.na(class),unique(class)])

level_data_class <- data.table(phylum_name = character(n_class),
                               class_name  = character(n_class), 
                               species    = numeric  (n_class),
                               genus      = numeric  (n_class),
                               family     = numeric  (n_class),
                               order      = numeric  (n_class),
                               subclass   = numeric  (n_class),
                               class      = numeric  (n_class), 
                               data_set = "EP")

for (i in 1:n_class) {
        
        loop_class <- data4[!is.na(class),unique(class)][i]
        
        level_data_class[i, class_name :=  loop_class]
        level_data_class[i, phylum_name := data4[class == class_name, unique(phylum)]]
        
        loop_sub <- data4[class == loop_class]
        loop_obs <- nrow(loop_sub)
        
        level_data_class[i, species   := round(loop_sub[!is.na(species),                                                                                    .N]/ loop_obs * 100,2)]
        level_data_class[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                                                                    .N]/ loop_obs * 100,2)]
        level_data_class[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),                                                   .N]/ loop_obs * 100,2)]
        level_data_class[i, order     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),                                   .N]/ loop_obs * 100,2)]
        level_data_class[i, subclass  := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) & !is.na(subclass),                .N]/ loop_obs * 100,2)]
        level_data_class[i, class     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) &  is.na(subclass) & !is.na(class),.N]/ loop_obs * 100,2)]
}
setorderv(level_data_class, cols = c("phylum_name", "class_name"))

## -- sublass 
n_subclass <- length(data4[!is.na(subclass), unique(subclass)])

level_data_subclass <- data.table(
        phylum_name    = character(n_subclass),
        class_name     = character(n_subclass),
        subclass_name  = character(n_subclass),
        species        = numeric   (n_subclass),
        genus          = numeric   (n_subclass),
        family         = numeric   (n_subclass),
        order          = numeric   (n_subclass),
        subclass       = numeric   (n_subclass), 
        data_set = "EP"
)

for (i in 1:n_subclass) {
        
        loop_subclass <- data4[!is.na(subclass),unique(subclass)][i]
        
        level_data_subclass[i, subclass_name := loop_subclass]
        level_data_subclass[i, class_name    := data4[subclass == loop_subclass, unique(class)]]
        level_data_subclass[i, phylum_name   := data4[subclass == subclass_name, unique(phylum)]]
        
        loop_sub <- data4[subclass == loop_subclass]
        loop_obs <- nrow(loop_sub)
        
        level_data_subclass[i, species   := round(loop_sub[!is.na(species),                                                                                    .N]/ loop_obs * 100,2)]
        level_data_subclass[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                                                                    .N]/ loop_obs * 100,2)]
        level_data_subclass[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),                                                   .N]/ loop_obs * 100,2)]
        level_data_subclass[i, order     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),                                   .N]/ loop_obs * 100,2)]
        level_data_subclass[i, subclass  := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) & !is.na(subclass),                .N]/ loop_obs * 100,2)]
}
setorderv(level_data_subclass, cols = c("phylum_name", "class_name", "subclass_name"))

## -- order 
n_order <- length(data4[!is.na(order), unique(order)])

level_data_order <- data.table(
        phylum_name    = character (n_order),
        class_name     = character (n_order),
        subclass_name  = character (n_order),
        order_name     = character (n_order),
        species        = numeric   (n_order),
        genus          = numeric   (n_order),
        family         = numeric   (n_order),
        order          = numeric   (n_order), 
        data_set = "EP"
)

for (i in 1:n_order) {
        
        loop_order <- data4[!is.na(order),unique(order)][i]
        
        level_data_order[i, order_name    := loop_order]
        level_data_order[i, subclass_name := data4[order == loop_order, unique(subclass) ]]
        level_data_order[i, class_name    := data4[order == loop_order, unique(class)]]
        level_data_order[i, phylum_name   := data4[order == loop_order, unique(phylum)]]
        
        loop_sub <- data4[order == loop_order]
        loop_obs <- nrow(loop_sub)
        
        level_data_order[i, species   := round(loop_sub[!is.na(species),                                                                                    .N]/ loop_obs * 100,2)]
        level_data_order[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                                                                    .N]/ loop_obs * 100,2)]
        level_data_order[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),                                                   .N]/ loop_obs * 100,2)]
        level_data_order[i, order     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),                                   .N]/ loop_obs * 100,2)]
}
setorderv(level_data_order, cols = c("phylum_name", "class_name", "subclass_name", "order_name"))

## -- family 
n_family <- length(data4[!is.na(family), unique(family)])

level_data_family <- data.table(
        phylum_name    = character (n_family),
        class_name     = character (n_family),
        subclass_name  = character (n_family),
        order_name     = character (n_family),
        family_name    = character (n_family),
        species        = numeric   (n_family),
        genus          = numeric   (n_family),
        family         = numeric   (n_family), 
        data_set = "EP"
        
)

for (i in 1:n_family) {
        
        loop_family <- data4[!is.na(family),unique(family)][i]
        
        level_data_family[i, family_name   := loop_family]
        level_data_family[i, order_name    := data4[family == loop_family, unique(order)]]
        level_data_family[i, subclass_name := data4[family == loop_family, unique(subclass) ]]
        level_data_family[i, class_name    := data4[family == loop_family, unique(class)]]
        level_data_family[i, phylum_name   := data4[family == loop_family, unique(phylum)]]
        
        loop_sub <- data4[family == loop_family]
        loop_obs <- nrow(loop_sub)
        
        level_data_family[i, species   := round(loop_sub[!is.na(species),                                 .N]/ loop_obs * 100,2)]
        level_data_family[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                 .N]/ loop_obs * 100,2)]
        level_data_family[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),.N]/ loop_obs * 100,2)]
        
}
setorderv(level_data_family, cols = c("phylum_name", "class_name", "subclass_name", "order_name", "family_name"))

level_list <- list(level_data_phylum, level_data_class, level_data_subclass,level_data_order, level_data_family)
saveRDS(level_list, 
        paste0(
        "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/019_Edwin Peters/03_FinalData/08_",Sys.Date(),"_level_list_mzb_EP.RDS"
        ))



# 04. Final Touches --------------------------------------------------------

# assign gr_site_id

# I need to go beyond the loooop 

unique_sites = unique(data4$original_site_name)
unique_dates = unique(data4$date)

data4[, c("site_id", "date_id") := list(
        map_int(data4$original_site_name, ~ which(unique_sites == .x )),
        map_int(data4$date, ~ which(unique_dates == .x ))
        )]


data4[, site_id2 := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data4[, date_id2 := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(date_id))]

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_MZB_edwin_peters")]

n.sample.data = data4[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data4 = data3[n.sample.data, on = "site_id2"]

# for (i in seq_along(unique(data4$gr_sample_id))) {
#         
#         id = unique(data4$gr_sample_id)[i]
#         temp.reduced.data = data4[gr_sample_id == id]
#         n.spec = nrow(temp.reduced.data)
#         n.gen1 = unique(temp.reduced.data$gen)
#         n.gen2 = length(n.gen1)
#         n.gen3 = sum(is.na(temp.reduced.data$gen))
#         n.gen = ifelse(n.gen3 > 0, sum(n.gen2, n.gen3, -1), n.gen2)
#         n.fam1 = unique(temp.reduced.data$fam)
#         n.fam2 = length(n.fam1)
#         n.fam3 = sum(is.na(temp.reduced.data$fam))
#         n.fam = ifelse(n.fam3 > 0, sum(n.fam2, n.fam3, -1), n.fam2)
#         n.ord1 = unique(temp.reduced.data$ord)
#         n.ord2 = length(n.ord1)
#         n.ord3 = sum(is.na(temp.reduced.data$ord))
#         n.ord = ifelse(n.ord3 > 0, sum(n.ord2, n.ord3, -1), n.ord2)
#         
#         data4[gr_sample_id == id, c("n.species", "n.genus", "n.family", "n.order") := 
#                       list(n.spec, n.gen, n.fam, n.ord)]
# 
#         print(i)        
# };beepr::beep()


cor(data4$n.species, data4$n.order)
cor(data4$n.species, data4$n.genus)
cor(data4$n.species, data4$n.family)

for (i in seq_along(colnames(data4))) {
        x = pull(data4[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data4)[i])
        
}

data5 = data4[, list(
        gr_sample_id,
        original_site_name,
        date,
        year,
        season,
        site_id,
        date_id,
        species,
        genus,
        family,
        order,
        subclass,
        class,
        phylum,
        kingdom,
        abundance,
        pristine,
        n.species,
        n.genus,
        n.family,
        n.order,
        x.coord = x.coord / 100,
        y.coord = y.coord / 100,
        EPSG = 28992,
        data.set = "ediwn_peters_mzb",
        n.samples
        
)]



# there are no NAs in the coordinates 
is.na(data5$y.coord) %>% sum
is.na(data5$x.coord) %>%  sum

final = st_as_sf(data5, coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

test1 = final %>% setDT

test1 = unique(test1, by = "site_id")

test1 = st_as_sf(test1, crs = test1$EPSG[1])

library(tmap)
tmap_mode("view")
tm_shape(test1) + tm_dots()


st_write(final, paste0("03_FinalData/09_",Sys.Date(),"_MZB_ediwn_peters_high_phyla.gpkg"))   
saveRDS (final, paste0("03_FinalData/09_",Sys.Date(),"_MZB_ediwn_peters_high_phyla.RDS") )  
