### ---------------------------------------------- ###
# --- Cleaning Invertebrate data from Peter Haase--- #
### ---------------------------------------------- ###

# 11.10.19 
# Cleaning/ Homogenization of invertebrate data from Peter Haase. 
# EPSG: 31467  DHDN / 3-degree Gauss-Kruger zone 3

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Cleaning Data 
# 03. Taxonomic Cleaning 
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------

pacman::p_load(dplyr, taxize, magrittr, sf, stringr, data.table, lubridate)
# also requireed: here 
setwd(here("020_Peter_Haase"))
setwd("~/01_Uni/03_GetReal/02_WP_02/Community Data/20_Peter_Haase/")
samples1 = readxl::read_excel("01_OriginalData/HE_MZB.xlsx") %>% 
  setDT
samples2 = readxl::read_excel("01_OriginalData/HE_MZB2.xlsx", sheet = 2) %>% 
  setDT
sites1 = readxl::read_excel("01_OriginalData/HE_STAMM.xlsx") %>% 
  setDT



# 02. Cleaning data -------------------------------------------------------
samples1_2 = samples1[, list(
  original_site_id = as.numeric(ID_RS),
  date = dmy(Datum),
  year = as.numeric(Year), 
  season = ifelse(Month %in% c("12", "01","02"), "winter", 
                  ifelse(Month %in% c("03", "04", "05"), "spring", 
                  ifelse(Month %in% c("06","07","08"), "summer", "autumn"))),
  taxon = Taxon,
  abundance = as.numeric(IZ),
  pristine = NA
  
)]

sites1_2 = sites1[,list(
  original_site_id = as.numeric(ID_RS),
  original_site_name = Messstelle,
  xcoord = as.numeric(RW_GK3),
  ycoord = as.numeric(HW_GK3),
  EPSG = 31467
)]        

data1 = samples1_2[sites1_2, on = "original_site_id"]

data1 = data1[!(is.na(taxon))]        


samples2_1 = samples2[,list(
  original_site_id = as.numeric(Site),
  date = dmy(samples2$Datum),
  taxon = Tax,
  original_site_name = "NA",
  xcoord = as.numeric(RW),
  ycoord = as.numeric(HW),
  EPSG = 31467,
  abundance = as.numeric(iz), 
  pristine = NA
  
)]

samples2_1[, c("year", "season") := list(
        year(date),
        ifelse(month(date) %in% c("12", "01","02"), "winter",
               ifelse(month(date) %in% c("03", "04", "05"), "spring",
                      ifelse(month(date) %in% c("06","07","08"), "summer", "autumn")))
)]

samples2_1 = samples2_1[,.SD, .SDcols = c(1,2,10,11,3,8,9,4,5,6,7)]

data = rbindlist(list(samples2_1, data1))
data2 = data


saveRDS(data2, "03_FinalData/01_191129_data_before_taxon_clean_Peter_Haase.RDS")
data2 = readRDS("03_FinalData/01_191129_data_before_taxon_clean_Peter_Haase.RDS")


# 03. Taxonomic Cleaning  ----------------------------------------------------------------


TU = sort(unique(data2$taxon))


classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191129_classification.object_Peter_Hasse.RDS")
classification.object = readRDS("03_FinalData/02_191129_classification.object_Peter_Hasse.RDS")

taxontable = data.table(
        taxon = TU,
        species = "NA",
        genus = "NA",
        family = "NA",
        order = "NA",
        clean = F
)

# sourcing does not work properly. Open the file up and run it manually. 
clean_invertebrate_synonyms.R


taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% length

response.vector = NULL
response = c()

for (i in 1173:(nrow(taxontable) + 1)) {
        
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
saveRDS(taxontable, "03_FinalData/03_191129_initial_taxon_clean_Peter_Haase.RDS")
taxontable = readRDS("03_FinalData/03_191129_initial_taxon_clean_Peter_Haase")


# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort

saveRDS(taxontable, "03_FinalData/04_191129_post_correction_taxontable_Peter_Haase.RDS")
taxontable = readRDS("03_FinalData/04_191129_post_correction_taxontable_Peter_Haase.RDS")


# add taxonomical information to data 
data2 = data[taxontable, on = "taxon"]

data3 = left_join(data2, 
                  taxontable) %>% 
        setDT

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
# character vector with all orders in data set 
(yes_order <- sort(data3[!is.na(order) & is.na(kingdom), unique(order)]))
yes_order <- yes_order[-which(yes_order == "NA")]
data3[order == "NA" & is.na(kingdom)]

# loop through order names and classify with taxize. Use the enzyclopedia of life  
for (j in 1:length(yes_order)) {
  loop_taxon <- yes_order[j]
  what_order[[j]] <- classification(loop_taxon, "eol")
}

# Amphimelaniidae did not work -- fixed in add higher taxa 

add_pre_order <- data.table(  order   = character(length(yes_order)), 
                              subclass = character(length(yes_order)), 
                              class    = character(length(yes_order)), 
                              phylum   = character(length(yes_order)), 
                              kingdom  = character(length(yes_order)) 
)

# Loop pulling subclass, class, phylum and kingdom entries out of respective taxize result table 
for (i in 1:length(yes_order)) {
  
  loop_taxon             <- yes_order[i]
  add_pre_order$order[i] <- loop_taxon
  loop_clasi             <- what_order[[i]][[1]]
  
  data3_rows <- which(data3$order == loop_taxon)
  
  if ("subclass" %in% loop_clasi[,2]) {
    data3$subclass[data3_rows] <- loop_clasi[which(loop_clasi[,2] == "subclass"), 1] 
    add_pre_order$subclass[i]  <- loop_clasi[which(loop_clasi[,2] == "subclass"), 1] 
  }
  if ("class" %in% loop_clasi[, 2]) {
    data3$class[data3_rows] <- loop_clasi[which(loop_clasi[, 2] == "class")  , 1]
    add_pre_order$class[i]  <- loop_clasi[which(loop_clasi[, 2] == "class")  , 1]
  }
  if ("phylum" %in% loop_clasi[, 2]) {
    data3$phylum[data3_rows] <- loop_clasi[which(loop_clasi[, 2] == "phylum") , 1]
    add_pre_order$phylum[i]  <- loop_clasi[which(loop_clasi[, 2] == "phylum") , 1]
  }
  if ("kingdom" %in% loop_clasi[, 2]) {
    data3$kingdom[data3_rows] <- loop_clasi[which(loop_clasi[, 2] == "kingdom"), 1]
    add_pre_order$kingdom[i]  <- loop_clasi[which(loop_clasi[, 2] == "kingdom"), 1]
    
  }
  
}

(yes_famil <- sort(data3[!is.na(family) & is.na(kingdom), unique(family)]))

for (j in 1:length(yes_famil)) {
  loop_taxon <- yes_famil[j]
  what_family[[j]] <- classification(loop_taxon, "eol")
}
add_pre_family <- data.table(family  = character(length(yes_famil)),
                             subclass = character(length(yes_famil)), 
                             class    = character(length(yes_famil)), 
                             phylum   = character(length(yes_famil)), 
                             kingdom  = character(length(yes_famil)) 
)
for (i in 1:length(yes_famil)) {
  
  loop_taxon               <- yes_famil[i]
  add_pre_family$family[i] <- loop_taxon
  loop_clasi               <- what_family[[i]][[1]]
  
  data3_rows <- which(data3$family == loop_taxon)
  
  if ("subclass" %in% loop_clasi[,2]) {
    data3$subclass[data3_rows]      <- loop_clasi[which(loop_clasi[,2] == "subclass"), 1] 
    add_pre_family$subclass[i] <- loop_clasi[which(loop_clasi[,2] == "subclass"), 1] 
  }
  if ("class" %in% loop_clasi[, 2]) {
    data3$class[data3_rows]      <- loop_clasi[which(loop_clasi[, 2] == "class")  , 1]
    add_pre_family$class[i] <- loop_clasi[which(loop_clasi[, 2] == "class")  , 1]
  }
  if ("phylum" %in% loop_clasi[, 2]) {
    data3$phylum[data3_rows]      <- loop_clasi[which(loop_clasi[, 2] == "phylum") , 1]
    add_pre_family$phylum[i] <- loop_clasi[which(loop_clasi[, 2] == "phylum") , 1]
  }
  if ("kingdom" %in% loop_clasi[, 2]) {
    data3$kingdom[data3_rows]      <- loop_clasi[which(loop_clasi[, 2] == "kingdom"), 1]
    add_pre_family$kingdom[i] <- loop_clasi[which(loop_clasi[, 2] == "kingdom"), 1]
    
  }
  
}


# clean new rows  
add_pre_family
add_pre_order
add_pre_family[,order := NULL]
pre_famil <- rbindlist(list(add_pre_family, pre_famil))
pre_order <- rbindlist(list(add_pre_order, pre_order))

saveRDS(pre_famil, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/data_fix_fam.RDS")
saveRDS(pre_order, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/data_fix_ord.RDS")

## -- clean up 
unique(data3$kingdom)
unique(data3$phylum) %>% sort
unique(data3$class) %>% sort
unique(data3$subclass) %>% sort

data3[kingdom == "NA"]; data3[is.na(kingdom)]

data3[species == "NA", taxon] %>% unique %>% sort
data3[is.na(species)]
data3[genus == "NA", taxon] %>% unique %>% sort
data3[is.na(genus)]
data3[family == "NA"] %>% unique(by = "taxon")
data3[is.na(family)]
data3[order == "NA"] %>% unique(by = "taxon")
data3[is.na(order)]

data3[species == "NA", species := NA]
data3[genus == "NA"  , genus  := NA]
data3[family == "NA" , family := NA]
data3[order == "NA"  , order := NA]

data4 = data3

saveRDS(data4, "03_FinalData/05b_200518_final_taxon_join_mzb_Peter_Haase_high_phyla.RDS")
data4 = readRDS("03_FinalData/05_200108_final_taxon_join_dia_RIVPACS.RDS")
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
                                data_set = "Peter Haase")

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
                               data_set = "Rivpacs")

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
  data_set = "Rivpacs"
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
  data_set = "Rivpacs"
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
  data_set = "Rivpacs"
  
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
saveRDS(level_list, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/020_Peter_Haase//03_FinalData/08_2020-05-18_level_list_mzb_peter_haase.RDS")

# 04. Final Touches -------------------------------------------------------

## now finish data set and make spatial 
# common structure: gr_id = abbreviation_ DIA or MZB _ Countrycode_ for each site 
# variables= gr_id, sites x.coord, y.coord, order, family, genus, species, year, season, epsg
# Give IDs

data3 = data4 

# assign gr_site_id
for (i in seq_along(unique(data3$original_site_id))) {
        
        site_ide_var = i 
        length_of_id = nchar(trunc(i))
        site_ide_var2 = case_when(length_of_id == 1 ~ paste0("0000",i),
                                  length_of_id == 2 ~ paste0("000",i),
                                  length_of_id == 3 ~ paste0("00",i),
                                  length_of_id == 4 ~ paste0("0",i),
                                  length_of_id == 5 ~ paste0(i))
        
        data3[original_site_id == unique(data3$original_site_id)[i], 
              site_id := site_ide_var2]
        
        dates = data3[site_id == site_ide_var2, "date"] %>% 
                pull() %>% 
                unique %>% 
                sort
        
        NumberOfSamplings = length(dates)
        
        for (k in seq_along(dates)) {
                
                data.filler = case_when(
                                        k < 10 ~ paste0("00",k),
                                        k > 10 & k < 100 ~ paste0("0", k), 
                                        k > 100 ~ paste0(k)
                                        )
                data3[site_id == site_ide_var2 & date == dates[k], date_id := data.filler]
        }
        data3[site_id == site_ide_var2, n.samples := NumberOfSamplings]
}
data3[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_MZB_peter_haase")]
beepr::beep()

data4 = data3[!is.na(original_site_id)]


for (i in seq_along(unique(data4$gr_sample_id))) {
        
        id = unique(data4$gr_sample_id)[i]
        temp.reduced.data = data4[gr_sample_id == id]
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
        
        data4[gr_sample_id == id, c("n.species", "n.genus", "n.family", "n.order") := 
                          list(n.spec, n.gen, n.fam, n.ord)]
        
};beepr::beep()


unique(data4, by = "gr_sample_id")
unique(data4$n.species) %>% sort
unique(data4$n.genus) %>% sort
unique(data4$n.family) %>% sort
unique(data4$n.order) %>% sort

cor(data4$n.species, data4$n.order)
cor(data4$n.species, data4$n.genus)
cor(data4$n.species, data4$n.order)

# there are no  NAs in the coordinates 
is.na(data4$y.coord) %>% sum
is.na(data4$x.coord) %>%  sum


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
        x.coord = xcoord,
        y.coord = ycoord,
        EPSG,
        data.set = "peter_haase",
        n.samples
        
)]

mzb_ger_ph.sf = st_as_sf(data5, coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

test1 = mzb_ger_ph.sf %>% setDT

test1 = unique(test1, by = "site_id")

test1 = st_as_sf(test1, crs = test1$EPSG[1])

library(tmap)
tmap_mode("view")
tm_shape(test1) + tm_dots()

# one site as wrong coordinates associated with it. I will drop it. 

mzb_ger_ph.sf = filter(mzb_ger_ph.sf, site_id != 546)

st_write(mzb_ger_ph.sf, "03_FinalData/09_200518_MZB_peter_haase_high_phyla.gpkg")        
saveRDS (mzb_ger_ph.sf, "03_FinalData/09_200518_MZB_peter_haase_high_phyla.RDS")        
