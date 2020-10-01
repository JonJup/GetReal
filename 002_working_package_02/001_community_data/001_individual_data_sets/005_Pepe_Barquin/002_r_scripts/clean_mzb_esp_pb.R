### -------------------------- ### 
# --- Clean MZB Pepe Barquin --- #
### -------------------------- ### 

## 22.10.19
## GR WP2 
## Here I clean and harmonize the biological data from Pepe Barquin from Cantabria and Picos
# EPSG: 23030

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Clean Data 


# 01. Setup -------------------------------------------------------------------
#libraries
pacman::p_load(dplyr,stringr, readxl, sf, data.table,tidyr, taxize, magrittr, lubridate, purrr, tmap, here)
tmap_mode("view")
# also required: here 
setwd(here("005_Pepe_Barquin"))
# - Cantabria 
data.cant = read_excel("01_OriginalData/Cantabria_55sites_2005.xls", skip = 2) %>% 
        setDT()
spatial = st_read("01_OriginalData/Cantabria55.shp")        
# - Picos 

data.pico = read_excel("01_OriginalData/Picos_13Sites_2015_2017.xlsx", skip = 2) %>% 
        setDT()
spatial2 = st_read("01_OriginalData/Picos13_sites.shp")


# 02. Clean Data ----------------------------------------------------------

## -- Cantabria -- ## 
# there are nine empty rows at the end which need to be removed  
data.cant = data.cant[-c(56:64)]
data.cant2 = data.cant %>% 
        gather(key = "Taxa", value = "Abundance", -c(1,2)) %>%
        setDT
# remove zero abundance observations 
data.cant2 = data.cant2[Abundance != 0]
# adapt site-id to spatial data 
SiteString2 <- c()
for (i in 1:length(data.cant2$Site)) {
                
                
        a = data.cant2$Site[i]
        if (is.na(a)) {SiteString2[i] = NA } else {
                        
                        
                front = str_split(a, regex("[0-9]"))[[1]][1]
                back = str_split(a, regex("[A-Z]"))[[1]][3]
                        
                new = paste0(front,0,back)
                SiteString2[i] = new
        }
                
}
        
data.cant2$Site = SiteString2
        
## adjust date 
date_split = str_split(data.cant2$Date, " ")
data.cant2$season = lapply(date_split, "[", 1) %>%  
        unlist()        
data.cant2$year = lapply(date_split, "[", 2) %>%  
        unlist()
data.cant2[,Date := NULL]
data.cant2 = data.cant2[!(Taxa %in% paste0("...", 175:209))]     

saveRDS(data.cant2, "03_FinalData/01_191216_data_before_taxon_clean_cantabria_Pepe.RDS")
data.cant2 = readRDS("03_FinalData/01_191216_data_before_taxon_clean_cantabria_Pepe.RDS")


## -- Picos -- ## 
# convert to long format
data.pico2 <- 
        data.pico %>% 
        gather(key = "Taxa", value = "Abundance", -c(1:5)) %>% 
        setDT

## remove zero abundance observations 
data.pico2 <-  data.pico2[Abundance != 0]

# clean site names to join with spatial 
data.pico2$Río[which(data.pico2$Río == "ARENAL")] = "Río Arenal"
data.pico2$Río[which(data.pico2$Río == "BULNES")] = "Bulnes"
data.pico2$Río[which(data.pico2$Río == "CARES-VALDEÓN")] = "Cares Valdeón"
data.pico2$Río[which(data.pico2$Río == "DUJE")] = "Duje"
data.pico2$Río[which(data.pico2$Río == "CASAÑO")] = "Casaño"
data.pico2$Río[which(data.pico2$Río == "FARFADA")] = "Manantial Farfada"
data.pico2$Río[which(data.pico2$Río == "FUENTE DÉ")] = "Fuente Dé"
data.pico2$Río[which(data.pico2$Río == "MANANTIAL FARFADA")] = "Manantial Farfada"
data.pico2$Río[which(data.pico2$Río == "MANANTIAL PONGA")] = "Manantial Ponga"
data.pico2$Río[which(data.pico2$Río == "RÍO PONGA")] = "Ponga"
data.pico2$Río[which(data.pico2$Río == "SALVORÓN")] = "Salvorón"
data.pico2$Río[which(data.pico2$Río == "SECO")] = "Seco"
data.pico2$Río[which(data.pico2$Río == "SELLA")] = "Sella"
data.pico2$Río[which(data.pico2$Río == "TIELVE")] = "Duje Tielve"
data.pico2$Río[which(data.pico2$Río == "TIELVE-DUJE")] = "Duje Tielve"
data.pico2$Río[which(data.pico2$Río == "VALDEÓN")] = "Cares Valdeón" 

spatial2$Nom_rio = as.character(spatial2$Nom_rio)

data.pico2[, c("year", "season") := list(
        A?o,
        if_else(month(data.pico2$FechaMuestra) %in% c(1,2,12),  "Winter",
         if_else(month(data.pico2$FechaMuestra) %in% c(3,4,5),   "Spring",
         if_else(month(data.pico2$FechaMuestra) %in% c(6,7,8),   "Summer",
         if_else(month(data.pico2$FechaMuestra) %in% c(9,10,11), "Autmun", "NA"))))
)]

data.pico2 <- data.pico2[!(Taxa %in% c("...157","...158", "...159", "NA"))]

saveRDS(data.pico2, "03_FinalData/01_191216_data_before_taxon_clean_picos_Pepe.RDS")
data.pico2 = readRDS("03_FinalData/01_191216_data_before_taxon_clean_picos_Pepe.RDS")

# Taxonomic Cleaning ------------------------------------------------------

TU = sort(unique(append(data.cant2$Taxa, data.pico2$Taxa)))

classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191216_classification.object_Pepe.RDS")
classification.object = readRDS("03_FinalData/02_191216_classification.object_Pepe.RDS")

taxontable = data.table(
        taxon = TU,
        species = "NA",
        genus = "NA",
        family = "NA",
        order = "NA",
        clean = F
        )
#  Open the file up and run it manually. 
clean_diatoms_synonyms.R
        
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% length
        
response.vector = NULL
response = c()
        
# In this loop two errors pop up. Both are due to the taxon beeing on a class level. 
# Can be skipped and will be taken care of in a later step.
for (i in 1:(nrow(taxontable) + 1)) {
                
                # skip if clean
                if (taxontable$clean[i] == TRUE) {
                        next()
                }
                # fill tax object 
                tax = classification.object[taxontable$taxon[i]]
                
                # is a response vector loaded?
                if (!(is.null(response.vector))) {
                        response[i] = response.vector[i]
                        
                        # decline if tax is NA 
                } else if (is.na(tax)) {
                        response[i] = ""
                        
                        # look if the found species is found in the original name 
                } else if (str_detect( names(tax), tax[[1]][nrow(tax[[1]]),1])) {
                        
                        response[i] = "y"
                        
                } else {
                        cat(
                                "",
                                "\n",
                                "\ ",
                                names(tax),
                                "\n",
                                "\ ",
                                ifelse(
                                        "species" %in% tax[[1]][, 2],
                                        tax[[1]][which(tax[[1]][, 2] == "species"), 1],
                                        ifelse(
                                                "genus" %in% tax[[1]][, 2],
                                                tax[[1]][which(tax[[1]][, 2] == "genus"), 1],
                                                ifelse("family" %in% tax[[1]][, 2],
                                                       tax[[1]][which(tax[[1]][, 2] == "family"), 1],  tax[[1]][1])
                                        )
                                ),
                                fill = T
                        )
                        
                        response[i] = readline("good?")
                }
                
                if (response[i] == "y") {
                        taxontable$clean[i] = T
                        if ("species" %in% tax[[1]]$rank) {
                                spe.row = which(tax[[1]]$rank == "species")
                                taxontable[i, species := tax[[1]]$name[spe.row]]
                        }
                        if ("genus" %in% tax[[1]]$rank) {
                                spe.row = which(tax[[1]]$rank == "genus")
                                taxontable[i, genus := tax[[1]]$name[spe.row]]
                        }
                        if ("family" %in% tax[[1]]$rank) {
                                spe.row = which(tax[[1]]$rank == "family")
                                taxontable[i, family := tax[[1]]$name[spe.row]]
                        }
                        if ("order" %in% tax[[1]]$rank) {
                                spe.row = which(tax[[1]]$rank == "order")
                                taxontable[i, order := tax[[1]]$name[spe.row]]
                        }
                }
                if (response[i] == "break")
                        break()
}
        
# quick save and starting point for later sessions
saveRDS(taxontable, "03_FinalData/03_191216_initial_taxon_clean_Pepe.RDS")
taxontable = readRDS("03_FinalData/03_191216_initial_taxon_clean_Pepe.RDS")

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort
        
saveRDS(taxontable, "03_FinalData/04_191216_post_correction_taxontable_Pepe.RDS")
taxontable = readRDS("03_FinalData/04_191216_post_correction_taxontable_Pepe.RDS")

data.cant3 = left_join(data.cant2,
                       taxontable, 
                        by = c("Taxa" = "taxon")) %>% 
        setDT

data.pico3 = left_join(data.pico2,
                       taxontable, 
                       by = c("Taxa" = "taxon")) %>% 
        setDT


# Higher Taxa -------------------------------------------------------------

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

what_taxa_level <- list()

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

# none 
yes_order <- sort(data3[!is.na(order) & is.na(kingdom), unique(order)])
yes_order <- yes_order[-which(yes_order == "NA")]
data3[order == "NA" & is.na(kingdom)]

## -- clean up 
unique(data3$kingdom)
unique(data3$phylum) %>% sort
unique(data3$class) %>% sort
unique(data3$subclass) %>% sort

data3[kingdom == "NA"]
data3[is.na(kingdom)]

saveRDS(data3, "03_FinalData/05b_200512_final_taxon_join_mzb_Pepe_Cantabria_high_phyla.RDS")
data.cant4 <- data3


## -- switch to Picos 
data3 <- data.pico3
names(data3)[6] <- "taxon"

# I did not copy the code again (DRY!) just go up and rerun the code for the new data3 

saveRDS(data3, "03_FinalData/05b_200512_final_taxon_join_mzb_Pepe_Picos_high_phyla.RDS")
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

saveRDS(data.pico4, "03_FinalData/05b_200512_final_taxon_join_mzb_Pepe_Pico_high_phyla.RDS")
data.pico4 = readRDS("03_FinalData/05_191216_final_taxon_join_pico_Pepe.RDS")

saveRDS(data.cant4, "03_FinalData/05b_200512_final_taxon_join_mzb_Pepe_Cantabria_high_phyla.RDS")
data.cant4 = readRDS("03_FinalData/05_191216_final_taxon_join_cant_Pepe.RDS")

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
saveRDS(level_list, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/005_Pepe_Barquin/03_FinalData/08_2020-05-12_level_list_Pepe_Picos.RDS")

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
saveRDS(level_list, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/005_Pepe_Barquin/03_FinalData/08_2020-05-12_level_list_Pepe_Cant.RDS")


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

for (i in seq_along(unique(data.pico5$gr_sample_id))) {
        
        temp.reduced.data = data.pico5[gr_sample_id == unique(gr_sample_id)[i]]
        n.spec = nrow(temp.reduced.data)
        n.gen1 = unique(temp.reduced.data$gen)
        n.gen2 = length(n.gen1)
        n.gen3 = sum(is.na(temp.reduced.data$gen))
        n.gen = ifelse(n.gen3 > 0, sum(n.gen2, n.gen3, -1), n.gen2)
        n.fam1 = unique(temp.reduced.data$fam)
        n.fam2 = length(n.fam1)
        n.fam3 = sum(is.na(temp.reduced.data$fam))
        n.fam = ifelse(n.fam3 > 0, sum(n.fam2, n.fam3, -1), n.fam2)
        n.ord1 = unique(temp.reduced.data$ord)
        n.ord2 = length(n.ord1)
        n.ord3 = sum(is.na(temp.reduced.data$ord))
        n.ord = ifelse(n.ord3 > 0, sum(n.ord2, n.ord3, -1), n.ord2)
        
        data.pico5[gr_sample_id == unique(gr_sample_id)[i],
              c("n.species", "n.genus", "n.family", "n.order") := 
                      list(n.spec, n.gen, n.fam, n.ord)
              ]
        
        print(i)        
};beepr::beep()

for (i in seq_along(unique(data.cant5$gr_sample_id))) {
        
        temp.reduced.data = data.cant5[gr_sample_id == unique(gr_sample_id)[i]]
        n.spec = nrow(temp.reduced.data)
        n.gen1 = unique(temp.reduced.data$gen)
        n.gen2 = length(n.gen1)
        n.gen3 = sum(is.na(temp.reduced.data$gen))
        n.gen = ifelse(n.gen3 > 0, sum(n.gen2, n.gen3, -1), n.gen2)
        n.fam1 = unique(temp.reduced.data$fam)
        n.fam2 = length(n.fam1)
        n.fam3 = sum(is.na(temp.reduced.data$fam))
        n.fam = ifelse(n.fam3 > 0, sum(n.fam2, n.fam3, -1), n.fam2)
        n.ord1 = unique(temp.reduced.data$ord)
        n.ord2 = length(n.ord1)
        n.ord3 = sum(is.na(temp.reduced.data$ord))
        n.ord = ifelse(n.ord3 > 0, sum(n.ord2, n.ord3, -1), n.ord2)
        
        data.cant5[gr_sample_id == unique(gr_sample_id)[i],
                   c("n.species", "n.genus", "n.family", "n.order") := 
                           list(n.spec, n.gen, n.fam, n.ord)
                   ]
        
        print(i)        
};beepr::beep()



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
        n.species,
        n.genus,
        n.family,
        n.order,
        x.coord = X_Coord,
        y.coord = Y_Coord,
        EPSG = 23030,
        data.set = "Picos_Pepe",
        n.samples
        
)]

data.pico6 = data.pico6[!is.na(x.coord)]

final = st_as_sf(data.pico6, coords = c("x.coord", "y.coord"), crs = data.pico6$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, "03_FinalData/09_200512_MZB_Picos_Pepe_high_phyla.gpkg")          
saveRDS(final, "03_FinalData/09_200512_MZB_Picos_Pepe_high_phyla.RDS")          

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
        n.species,
        n.genus,
        n.family,
        n.order,
        x.coord = X_COORD,
        y.coord = Y_COORD,
        EPSG = 23030,
        data.set = "Cantabria_Pepe",
        n.samples
        
)]

data.cant6 = data.cant6[!is.na(x.coord)]
final = st_as_sf(data.cant6, coords = c("x.coord", "y.coord"), crs = data.cant6$EPSG[1])
final = st_transform(final, crs = 4326)


test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, "03_FinalData/09_200512_MZB_canta_Pepe_high_phyla.gpkg")          
saveRDS(final,  "03_FinalData/09_200512_MZB_canta_Pepe_high_phyla.RDS")     
