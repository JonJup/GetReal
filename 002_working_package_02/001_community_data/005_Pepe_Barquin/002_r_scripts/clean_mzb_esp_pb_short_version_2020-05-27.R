### -------------------------- ### 
# --- Clean MZB Pepe Barquin --- #
# ------ Short Version --------  # 
### -------------------------- ### 

## 27.05.2020
## GR WP2 
## Here I clean and harmonize the biological data from Pepe Barquin from Cantabria and Picos
# EPSG: 23030

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Clean Data 


# 01. Setup -------------------------------------------------------------------
#libraries
pacman::p_load(dplyr,stringr, readxl, sf, data.table,tidyr, taxize, magrittr, purrr, here)

setwd(here("005_Pepe_Barquin"))

data.cant2 = readRDS("03_FinalData/01_191216_data_before_taxon_clean_cantabria_Pepe.RDS")
data.pico2 = readRDS("03_FinalData/01_191216_data_before_taxon_clean_picos_Pepe.RDS")
taxontable = readRDS("03_FinalData/04_191216_post_correction_taxontable_Pepe.RDS")
spatial    = st_read("01_OriginalData/Cantabria55.shp") 
spatial2   = st_read("01_OriginalData/Picos13_sites.shp")
spatial2$Nom_rio = as.character(spatial2$Nom_rio)

data.cant3 = left_join(data.cant2,
                       taxontable, 
                        by = c("Taxa" = "taxon")) %>% 
        setDT

data.pico3 = left_join(data.pico2,
                       taxontable, 
                       by = c("Taxa" = "taxon")) %>% 
        setDT


# Higher Taxa -------------------------------------------------------------

what_family <- list()
what_order  <- list()

# load previous solutions 
pre_order <- readRDS("../999_quality_check/data_fix_ord.RDS")
pre_famil <- readRDS("../999_quality_check/data_fix_fam.RDS")


# Two data sets. First Cantabria than Picos 

## -- Cantabria 

data3.cant <- data.table(site      = data.cant3$Site, 
                         taxon     = data.cant3$Taxa,
                         abundance = data.cant3$Abundance,
                         year      = data.cant3$year, 
                         species   = data.cant3$species,
                         genues    = data.cant3$genus,
                         family    = data.cant3$family,
                         order     = data.cant3$order
                         )

data3 <- data3.cant 


# source yourself 
source("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/add_higher_taxa.R")


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

# source yourself 
source("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/add_higher_taxa.R")

# none 
(yes_order <- sort(data3[!is.na(order) & is.na(kingdom), unique(order)]))
(yes_famil <- sort(data3[!is.na(family) & is.na(kingdom), unique(order)]))

## -- clean up 
unique(data3$kingdom)
unique(data3$phylum)   %>% sort
unique(data3$class)    %>% sort
unique(data3$subclass) %>% sort

data3[kingdom == "NA"]
data3[is.na(kingdom)]

#saveRDS(data3, paste0("03_FinalData/05b_",Sys.Date(),"_final_taxon_join_mzb_Pepe_Cantabria_high_phyla.RDS"))
data.cant4 <- data3


## -- switch to Picos 
data3 <- data.pico3
names(data3)[6] <- "taxon"

# I did not copy the code again (DRY!) just go up and rerun the code for the new data3 

#saveRDS(data3, paste0("03_FinalData/05b_",Sys.Date(),"_final_taxon_join_mzb_Pepe_Picos_high_phyla.RDS"))
data.pico4 <- data3

data.cant4[species == "NA", species := NA]
data.cant4[genus   == "NA"  , genus := NA]
data.cant4[family  == "NA" , family := NA]
data.cant4[order   == "NA"  , order := NA]
data.pico4[species == "NA", species := NA]
data.pico4[genus   == "NA"  , genus := NA]
data.pico4[family  == "NA" , family := NA]
data.pico4[order   == "NA"  , order := NA]

spatial$ESTACION = as.character(spatial$ESTACION)
spatial2$Nom_rio = as.character(spatial2$Nom_rio) 

data.cant4 = left_join(data.cant4, 
                       spatial, 
                       by = c("site" = "ESTACION")) %>% 
        setDT
data.pico4 = left_join(data.pico4, 
                       spatial2, 
                       by = c("Río" = "Nom_rio"))  %>% 
        setDT

saveRDS(data.pico4,  paste0("03_FinalData/05b_",Sys.Date(),"_final_taxon_join_mzb_Pepe_Pico_high_phyla.RDS"))
data.pico4 = readRDS("03_FinalData/05b_2020-05-28_final_taxon_join_mzb_Pepe_Pico_high_phyla.RDS")

saveRDS(data.cant4, paste0("03_FinalData/05b_",Sys.Date(),"_final_taxon_join_mzb_Pepe_Cantabria_high_phyla.RDS"))
data.cant4 = readRDS("03_FinalData/05b_2020-05-28_final_taxon_join_mzb_Pepe_Cantabria_high_phyla.RDS")

# Determine Levels for Taxa -----------------------------------------------

### --- Picos --- ### 

## -- Phylum 
n_phyl <- length(data.pico4[,unique(phylum)])

level_data_phylum <- data.table(phylum_name = character(n_phyl), 
                                species  = numeric(n_phyl),
                                genus    = numeric(n_phyl),
                                family   = numeric(n_phyl),
                                order    = numeric(n_phyl),
                                subclass = numeric(n_phyl),
                                class    = numeric(n_phyl),
                                phylum   = numeric(n_phyl), 
                                data_set = "Pepe_Picos")

for (i in 1:n_phyl) {
  
  level_data_phylum[i, phylum_name := data.pico4[,unique(phylum)][i]]
  
  loop_sub <- data.pico4[phylum == data.pico4[,unique(phylum)][i]]
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
n_class <- length(data.pico4[!is.na(class),unique(class)])

level_data_class <- data.table(phylum_name = character(n_class),
                               class_name  = character(n_class), 
                               species    = numeric  (n_class),
                               genus      = numeric  (n_class),
                               family     = numeric  (n_class),
                               order      = numeric  (n_class),
                               subclass   = numeric  (n_class),
                               class      = numeric  (n_class), 
                               data_set = "Pepe_Picos")

for (i in 1:n_class) {
  
  loop_class <- data.pico4[!is.na(class),unique(class)][i]
  
  level_data_class[i, class_name :=  loop_class]
  level_data_class[i, phylum_name := data.pico4[class == class_name, unique(phylum)]]
  
  loop_sub <- data.pico4[class == loop_class]
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
n_subclass <- length(data.pico4[!is.na(subclass), unique(subclass)])

level_data_subclass <- data.table(
  phylum_name    = character(n_subclass),
  class_name     = character(n_subclass),
  subclass_name  = character(n_subclass),
  species        = numeric   (n_subclass),
  genus          = numeric   (n_subclass),
  family         = numeric   (n_subclass),
  order          = numeric   (n_subclass),
  subclass       = numeric   (n_subclass), 
  data_set = "Pepe_Picos"
)

for (i in 1:n_subclass) {
  
  loop_subclass <- data.pico4[!is.na(subclass),unique(subclass)][i]
  
  level_data_subclass[i, subclass_name := loop_subclass]
  level_data_subclass[i, class_name    := data.pico4[subclass == loop_subclass, unique(class)]]
  level_data_subclass[i, phylum_name   := data.pico4[subclass == subclass_name, unique(phylum)]]
  
  loop_sub <- data.pico4[subclass == loop_subclass]
  loop_obs <- nrow(loop_sub)
  
  level_data_subclass[i, species   := round(loop_sub[!is.na(species),                                                                                    .N]/ loop_obs * 100,2)]
  level_data_subclass[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                                                                    .N]/ loop_obs * 100,2)]
  level_data_subclass[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),                                                   .N]/ loop_obs * 100,2)]
  level_data_subclass[i, order     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),                                   .N]/ loop_obs * 100,2)]
  level_data_subclass[i, subclass  := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) & !is.na(subclass),                .N]/ loop_obs * 100,2)]
}
setorderv(level_data_subclass, cols = c("phylum_name", "class_name", "subclass_name"))

## -- order 
n_order <- length(data.pico4[!is.na(order), unique(order)])

level_data_order <- data.table(
  phylum_name    = character (n_order),
  class_name     = character (n_order),
  subclass_name  = character (n_order),
  order_name     = character (n_order),
  species        = numeric   (n_order),
  genus          = numeric   (n_order),
  family         = numeric   (n_order),
  order          = numeric   (n_order), 
  data_set = "Pepe_Picos"
)

for (i in 1:n_order) {
  
  loop_order <- data.pico4[!is.na(order),unique(order)][i]
  
  level_data_order[i, order_name    := loop_order]
  level_data_order[i, subclass_name := data.pico4[order == loop_order, unique(subclass) ]]
  level_data_order[i, class_name    := data.pico4[order == loop_order, unique(class)]]
  level_data_order[i, phylum_name   := data.pico4[order == loop_order, unique(phylum)]]
  
  loop_sub <- data.pico4[order == loop_order]
  loop_obs <- nrow(loop_sub)
  
  level_data_order[i, species   := round(loop_sub[!is.na(species),                                                                                    .N]/ loop_obs * 100,2)]
  level_data_order[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                                                                    .N]/ loop_obs * 100,2)]
  level_data_order[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),                                                   .N]/ loop_obs * 100,2)]
  level_data_order[i, order     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),                                   .N]/ loop_obs * 100,2)]
}
setorderv(level_data_order, cols = c("phylum_name", "class_name", "subclass_name", "order_name"))

## -- family 
n_family <- length(data.pico4[!is.na(family), unique(family)])

level_data_family <- data.table(
  phylum_name    = character (n_family),
  class_name     = character (n_family),
  subclass_name  = character (n_family),
  order_name     = character (n_family),
  family_name    = character (n_family),
  species        = numeric   (n_family),
  genus          = numeric   (n_family),
  family         = numeric   (n_family), 
  data_set = "Pepe_Picos"
  
)

for (i in 1:n_family) {
  
  loop_family <- data.pico4[!is.na(family),unique(family)][i]
  
  level_data_family[i, family_name   := loop_family]
  level_data_family[i, order_name    := data.pico4[family == loop_family, unique(order)]]
  level_data_family[i, subclass_name := data.pico4[family == loop_family, unique(subclass) ]]
  level_data_family[i, class_name    := data.pico4[family == loop_family, unique(class)]]
  level_data_family[i, phylum_name   := data.pico4[family == loop_family, unique(phylum)]]
  
  loop_sub <- data.pico4[family == loop_family]
  loop_obs <- nrow(loop_sub)
  
  level_data_family[i, species   := round(loop_sub[!is.na(species),                                 .N]/ loop_obs * 100,2)]
  level_data_family[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                 .N]/ loop_obs * 100,2)]
  level_data_family[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),.N]/ loop_obs * 100,2)]
  
}
setorderv(level_data_family, cols = c("phylum_name", "class_name", "subclass_name", "order_name", "family_name"))

level_list <- list(level_data_phylum, level_data_class, level_data_subclass,level_data_order, level_data_family)
saveRDS(level_list, paste0("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/005_Pepe_Barquin/03_FinalData/08_",Sys.Date(),"_level_list_Pepe_Picos.RDS"))

### --- Cantabria --- ### 

## -- Phylum 
n_phyl <- length(data.cant4[,unique(phylum)])

level_data_phylum <- data.table(phylum_name = character(n_phyl), 
                                species  = numeric(n_phyl),
                                genus    = numeric(n_phyl),
                                family   = numeric(n_phyl),
                                order    = numeric(n_phyl),
                                subclass = numeric(n_phyl),
                                class    = numeric(n_phyl),
                                phylum   = numeric(n_phyl), 
                                data_set = "Pepe_Cant")

for (i in 1:n_phyl) {
  
  level_data_phylum[i, phylum_name := data.cant4[,unique(phylum)][i]]
  
  loop_sub <- data.cant4[phylum == data.cant4[,unique(phylum)][i]]
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
n_class <- length(data.cant4[!is.na(class),unique(class)])

level_data_class <- data.table(phylum_name = character(n_class),
                               class_name  = character(n_class), 
                               species    = numeric  (n_class),
                               genus      = numeric  (n_class),
                               family     = numeric  (n_class),
                               order      = numeric  (n_class),
                               subclass   = numeric  (n_class),
                               class      = numeric  (n_class), 
                               data_set = "Pepe_Cant")

for (i in 1:n_class) {
  
  loop_class <- data.cant4[!is.na(class),unique(class)][i]
  
  level_data_class[i, class_name :=  loop_class]
  level_data_class[i, phylum_name := data.cant4[class == class_name, unique(phylum)]]
  
  loop_sub <- data.cant4[class == loop_class]
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
n_subclass <- length(data.cant4[!is.na(subclass), unique(subclass)])

level_data_subclass <- data.table(
  phylum_name    = character(n_subclass),
  class_name     = character(n_subclass),
  subclass_name  = character(n_subclass),
  species        = numeric   (n_subclass),
  genus          = numeric   (n_subclass),
  family         = numeric   (n_subclass),
  order          = numeric   (n_subclass),
  subclass       = numeric   (n_subclass), 
  data_set = "Pepe_Cant"
)

for (i in 1:n_subclass) {
  
  loop_subclass <- data.cant4[!is.na(subclass),unique(subclass)][i]
  
  level_data_subclass[i, subclass_name := loop_subclass]
  level_data_subclass[i, class_name    := data.cant4[subclass == loop_subclass, unique(class)]]
  level_data_subclass[i, phylum_name   := data.cant4[subclass == subclass_name, unique(phylum)]]
  
  loop_sub <- data.cant4[subclass == loop_subclass]
  loop_obs <- nrow(loop_sub)
  
  level_data_subclass[i, species   := round(loop_sub[!is.na(species),                                                                                    .N]/ loop_obs * 100,2)]
  level_data_subclass[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                                                                    .N]/ loop_obs * 100,2)]
  level_data_subclass[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),                                                   .N]/ loop_obs * 100,2)]
  level_data_subclass[i, order     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),                                   .N]/ loop_obs * 100,2)]
  level_data_subclass[i, subclass  := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) & !is.na(subclass),                .N]/ loop_obs * 100,2)]
}
setorderv(level_data_subclass, cols = c("phylum_name", "class_name", "subclass_name"))

## -- order 
n_order <- length(data.cant4[!is.na(order), unique(order)])

level_data_order <- data.table(
  phylum_name    = character (n_order),
  class_name     = character (n_order),
  subclass_name  = character (n_order),
  order_name     = character (n_order),
  species        = numeric   (n_order),
  genus          = numeric   (n_order),
  family         = numeric   (n_order),
  order          = numeric   (n_order), 
  data_set = "Pepe_Cant"
)

for (i in 1:n_order) {
  
  loop_order <- data.cant4[!is.na(order),unique(order)][i]
  
  level_data_order[i, order_name    := loop_order]
  level_data_order[i, subclass_name := data.cant4[order == loop_order, unique(subclass) ]]
  level_data_order[i, class_name    := data.cant4[order == loop_order, unique(class)]]
  level_data_order[i, phylum_name   := data.cant4[order == loop_order, unique(phylum)]]
  
  loop_sub <- data.cant4[order == loop_order]
  loop_obs <- nrow(loop_sub)
  
  level_data_order[i, species   := round(loop_sub[!is.na(species),                                                                                    .N]/ loop_obs * 100,2)]
  level_data_order[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                                                                    .N]/ loop_obs * 100,2)]
  level_data_order[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),                                                   .N]/ loop_obs * 100,2)]
  level_data_order[i, order     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),                                   .N]/ loop_obs * 100,2)]
}
setorderv(level_data_order, cols = c("phylum_name", "class_name", "subclass_name", "order_name"))

## -- family 
n_family <- length(data.cant4[!is.na(family), unique(family)])

level_data_family <- data.table(
  phylum_name    = character (n_family),
  class_name     = character (n_family),
  subclass_name  = character (n_family),
  order_name     = character (n_family),
  family_name    = character (n_family),
  species        = numeric   (n_family),
  genus          = numeric   (n_family),
  family         = numeric   (n_family), 
  data_set = "Pepe_Cant"
  
)

for (i in 1:n_family) {
  
  loop_family <- data.cant4[!is.na(family),unique(family)][i]
  
  level_data_family[i, family_name   := loop_family]
  level_data_family[i, order_name    := data.cant4[family == loop_family, unique(order)]]
  level_data_family[i, subclass_name := data.cant4[family == loop_family, unique(subclass) ]]
  level_data_family[i, class_name    := data.cant4[family == loop_family, unique(class)]]
  level_data_family[i, phylum_name   := data.cant4[family == loop_family, unique(phylum)]]
  
  loop_sub <- data.cant4[family == loop_family]
  loop_obs <- nrow(loop_sub)
  
  level_data_family[i, species   := round(loop_sub[!is.na(species),                                 .N]/ loop_obs * 100,2)]
  level_data_family[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                 .N]/ loop_obs * 100,2)]
  level_data_family[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),.N]/ loop_obs * 100,2)]
  
}
setorderv(level_data_family, cols = c("phylum_name", "class_name", "subclass_name", "order_name", "family_name"))

level_list <- list(level_data_phylum, level_data_class, level_data_subclass,level_data_order, level_data_family)
saveRDS(level_list, paste0("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/005_Pepe_Barquin/03_FinalData/08_",Sys.Date(),"_level_list_Pepe_Cant.RDS"))


# 04. Final touches -------------------------------------------------------

## now finish data set and make spatial 
# common structure: gr_id = abbreviation_ DIA or MZB _ Countrycode_ for each site 
# variables= gr_id, sites x.coord, y.coord, order, family, genus, species, year, season, epsg
      
unique_sites_picos = unique(data.pico4$Río)
unique_dates_picos = unique(data.pico4$FechaMuestra) %>% sort
unique_sites_cant = unique(data.cant4$site)

data.pico4[, c("site_id", "date_id") := list(
        map_int(data.pico4$Río, ~ which(unique_sites_picos == .x )),
        map_int(data.pico4$FechaMuestra, ~ which(unique_dates_picos == .x ))
)]
data.cant4[, site_id := map_int(data.cant4$site, ~ which(unique_sites_cant == .x))]

data.pico4[, site_id2 := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data.cant4[, site_id2 := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]
data.pico4[, date_id2 := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(date_id))]

data.pico4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_picos_Pepe")]
data.cant4[,gr_sample_id := paste0("site_", site_id2, "_date_", "XXX","_cantabria_Pepe")]

n.sample.data = data.pico4[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data.pico5 = data.pico4[n.sample.data, on = "site_id2"]
data.cant4[, n.samples := 1]
data.cant5 = data.cant4

# for (i in seq_along(unique(data.pico5$gr_sample_id))) {
#         
#         temp.reduced.data = data.pico5[gr_sample_id == unique(gr_sample_id)[i]]
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
#         data.pico5[gr_sample_id == unique(gr_sample_id)[i],
#               c("n.species", "n.genus", "n.family", "n.order") := 
#                       list(n.spec, n.gen, n.fam, n.ord)
#               ]
#         
#         print(i)        
# };beepr::beep()
# 
# for (i in seq_along(unique(data.cant5$gr_sample_id))) {
#         
#         temp.reduced.data = data.cant5[gr_sample_id == unique(gr_sample_id)[i]]
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
#         data.cant5[gr_sample_id == unique(gr_sample_id)[i],
#                    c("n.species", "n.genus", "n.family", "n.order") := 
#                            list(n.spec, n.gen, n.fam, n.ord)
#                    ]
#         
#         print(i)        
# };beepr::beep()



for (i in seq_along(colnames(data.cant5))) {
        x = pull(data.cant5[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data.cant5)[i])
        
} 
for (i in seq_along(colnames(data.pico5))) {
        x = pull(data.pico5[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data.pico5)[i])
        
} 
data.pico6 = data.pico5[, list(
        gr_sample_id,
        original_site_name = Río,
        date = FechaMuestra,
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
        abundance = Abundance,
        pristine = NA,
        # n.species,
        # n.genus,
        # n.family,
        # n.order,
        x.coord = X_Coord,
        y.coord = Y_Coord,
        EPSG = 23030,
        data.set = "Picos_Pepe",
        n.samples
        
)]

data.pico6 = data.pico6[!is.na(x.coord)]

final = st_as_sf(data.pico6, coords = c("x.coord", "y.coord"), crs = data.pico6$EPSG[1])
final = st_transform(final, crs = 4326)
# test1 = final %>% setDT
# test1 = unique(test1, by = "site_id")
# test1 = st_as_sf(test1, crs = 4326)
# tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, paste0("03_FinalData/09_",Sys.Date(),"_MZB_Picos_Pepe_high_phyla.gpkg"))          
saveRDS(final,  paste0("03_FinalData/09_",Sys.Date(),"_MZB_Picos_Pepe_high_phyla.RDS"))        

rm(final)      

data.cant6 = data.cant5[, list(
        gr_sample_id,
        original_site_name = site,
        date = NA,
        year,
        season = "summer",
        site_id,
        date_id = NA,
        species,
        genus,
        family,
        order,
        subclass,
        class,
        phylum,
        kingdom,
        abundance = abundance,
        pristine = NA,
        # n.species,
        # n.genus,
        # n.family,
        # n.order,
        x.coord = X_COORD,
        y.coord = Y_COORD,
        EPSG = 23030,
        data.set = "Cantabria_Pepe",
        n.samples
        
)]

data.cant6 = data.cant6[!is.na(x.coord)]
final = st_as_sf(data.cant6, coords = c("x.coord", "y.coord"), crs = data.cant6$EPSG[1])
final = st_transform(final, crs = 4326)

# test1 = final %>% setDT
# test1 = unique(test1, by = "site_id")
# test1 = st_as_sf(test1, crs = 4326)
# tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, paste0("03_FinalData/09_",Sys.Date(),"_MZB_canta_Pepe_high_phyla.gpkg") )         
saveRDS(final,  paste0("03_FinalData/09_",Sys.Date(),"_MZB_canta_Pepe_high_phyla.RDS")  )   
