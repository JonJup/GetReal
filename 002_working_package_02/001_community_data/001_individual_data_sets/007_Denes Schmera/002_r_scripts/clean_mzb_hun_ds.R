### --------------------------- ### 
# --- Clean MZB Denes Schmera --- #
### --------------------------- ### 

## 23.10.19
## GR WP2 
## Here I clean and harmonize the biological data from Pepe Barquin from Cantabria and Picos

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Clean Data 
# 03. Taxonomic Cleaning 
# 04. Final Touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
#libraries
pacman::p_load(dplyr,stringr, here, readxl, sf, data.table,tidyr, taxize, magrittr, lubridate, purrr, tmap)
tmap_mode("view")
# also required: here 
setwd(here("007_Denes Schmera"))

data = read_excel("01_OriginalData/Hungary-data2.xlsx") %>%
        setDT


# 02. Clean Data ----------------------------------------------------------

# information on year and season is taken from  Schmera & Bauer (2011): Testing a
# typology system of running waters for conservation planning in Hungary

data = data[, 
            list(   site = SiteID,
                    year =  2005,
                    season = "spring",
                    x.coord = EOVY,
                    y.coord = EOVX,
                    taxon = TAXON,
                    EPSG = 23700
            )
            ]

saveRDS(data, "03_FinalData/01_191216_data_before_taxon_clean_Denes_Schmera.RDS")
data = readRDS("03_FinalData/01_191216_data_before_taxon_clean_Denes_Schmera.RDS")


# 03. Taxonomic Cleaning ------------------------------------------------------

TU = sort(unique(data$taxon))
classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191216_classification.object_Denes_Schmera.RDS")
classification.object = readRDS("03_FinalData/02_191216_classification.object_Denes_Schmera.RDS")

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
saveRDS(taxontable, "03_FinalData/03_191216_initial_taxon_clean_Denes_Schmera.RDS")
taxontable = readRDS("03_FinalData/03_191216_initial_taxon_clean_Denes_Schmera.RDS")

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort
        
saveRDS(taxontable, "03_FinalData/04_191216_post_correction_taxontable_Denes_Schmera.RDS")
taxontable = readRDS("03_FinalData/04_191216_post_correction_taxontable_Denes_Schmera.RDS")

data2 = left_join(data,
                  taxontable, 
                  by = "taxon") %>% 
        setDT

data3 <- data2

# Highlevel Taxonomic Groups  ---------------------------------------------
what_taxa_level <- list()
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
(yes_famil <- sort(data3[!is.na(family) & is.na(kingdom), unique(family)]))
yes_order <- yes_order[-which(yes_order == "NA")]
data3[order == "NA" & is.na(kingdom)]

# loop through order names and classify with taxize. Use the enzyclopedia of life  
for (j in 1:length(yes_famil)) {
        loop_taxon <- yes_famil[j]
        what_taxa_level[[j]] <- classification(loop_taxon, "eol")
}

# Amphimelaniidae did not work -- fixed in add higher taxa 

add_pre_family <- data.table(family  = character(length(yes_famil)),
                             order   = character(length(yes_famil)), 
                            subclass = character(length(yes_famil)), 
                            class    = character(length(yes_famil)), 
                            phylum   = character(length(yes_famil)), 
                            kingdom  = character(length(yes_famil)) 
)

# Loop pulling subclass, class, phylum and kingdom entries out of respective taxize result table 
for (i in 1:length(yes_famil)) {
        
        loop_taxon    <- yes_famil[i]
        add_pre_family$family[i] <- loop_taxon
        # loop_clasi <- classification(loop_taxon, "eol")
        # loop_clasi <- what_taxa_level[loop_taxon]
        loop_clasi <- what_taxa_level[[i]][[1]]
        
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
add_pre_family[,order := NULL]
pre_famil <- rbindlist(list(add_pre_family, pre_famil))

saveRDS(pre_famil, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/data_fix_fam.RDS")

## -- clean up 
unique(data3$kingdom)
unique(data3$phylum) %>% sort
unique(data3$class) %>% sort
unique(data3$subclass) %>% sort

data3[kingdom == "NA"]
data3[is.na(kingdom)]

data3[kingdom == "Metazoa", kingdom := "Animalia"]

data3[class == "Hexanauplia Oakley, Wolfe, Lindgren & Zaharof 2013", class := "Hexanauplia"]
data3[class == "Hydrozoa Owen 1843", class := "Hydrozoa"]
data3[class == "Gymnolaemata Allman 1856", class := "Gymnolaemata"]
data3[class == "Oligohymenophorea de Puytorac, Batisse, Bohatier, Corliss, Deroux, Didier, Dragesco, Fryd-Versavel, Grain, Grollere, Horasse, Mode, Laval, Roque, Savoie & Tuffrau 1974", 
      class := "Oligohymenophorea"]
data3[class == "Ostracoda Latreille 1802", class := "Ostracoda"]

data3[subclass == "Branchiura Thorell 1864", subclass := "Branchiura"]
data3[subclass == "Caenogastropoda Cox 1960", subclass := "Caenogastropoda"]
data3[subclass == "Cirripedia Burmeister 1834", subclass := "Cirripedia"]
data3[subclass == "Copepoda Milne Edwards 1840", subclass := "Copepoda"]
data3[subclass == "Digenea Carus 1863", subclass := "Digenea"]
data3[subclass == "Hoplonemertea Hubrecht 1879", subclass := "Hoplonemertea"]
data3[subclass == "Hydroidolina Collins 2000", subclass := "Hydroidolina"]
data3[subclass == "Neocopepoda Huys & Boxshall 1991", subclass := "Neocopepoda"]
data3[subclass == "Podocopa G. O. Sars 1866", subclass := "Podocopa"]
data3[subclass == "Thecostraca Gruvel 1905", subclass := "Thecostraca"]

data2[species == "NA", taxon] %>% unique %>% sort
data2[is.na(species)]
data2[genus == "NA", taxon] %>% unique %>% sort
data2[is.na(genus)]
data2[family == "NA"] %>% unique(by = "taxon")
data2[is.na(family)]
data2[order == "NA"] %>% unique(by = "taxon")
data2[is.na(order)]

data3[species == "NA", species := NA]
data3[genus == "NA", genus  := NA]
data3[family == "NA", family := NA]
data3[order == "NA", order := NA]


data4 = data3
data4[,Family := NULL]


saveRDS(data4, "03_FinalData/05b_200512_final_taxon_join_Denes_Schmera_high_phyla.RDS")
data4 = readRDS("03_FinalData/05_191216_final_taxon_join_Denes_Schmera.RDS")

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
                                data_set = "Schmera")

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
                               data_set = "Schmera")

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
        data_set = "Schmera"
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
        data_set = "Schmera"
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
        data_set = "Schmera"
        
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
saveRDS(level_list, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/007_Denes Schmera/03_FinalData/08_2020-05-12_level_list_Schmera.RDS")



# 04. Final touches -------------------------------------------------------

unique_sites = unique(data4$site)

data4[, c("site_id", "date_id") := list(
        map_int(data4$site, ~ which(unique_sites == .x ))
        )]


data4[, site_id2 := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", "XXX","_mzb_Schmera")]

data5 = data4

for (i in seq_along(unique(data5$gr_sample_id))) {
        
        temp.reduced.data = data5[gr_sample_id == unique(gr_sample_id)[i]]
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
        
        data5[gr_sample_id == unique(gr_sample_id)[i],
              c("n.species", "n.genus", "n.family", "n.order") := 
                      list(n.spec, n.gen, n.fam, n.ord)
              ]
        
        print(i)        
};beepr::beep()

for (i in seq_along(colnames(data4))) {
        x = pull(data4[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data4)[i])
        
}


data6 = data5[, list(
        gr_sample_id,
        original_site_name = site,
        date = NA,
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
        abundance = NA,
        pristine = NA,
        n.species,
        n.genus,
        n.family,
        n.order,
        x.coord,
        y.coord,
        EPSG,
        data.set = "Denes Schmera",
        n.samples = 1
        
)]

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
# test1 = final %>% setDT
# test1 = unique(test1, by = "site_id")
# test1 = st_as_sf(test1, crs = 4326)
# tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, "03_FinalData/09_200512_MZB_Denes_Schmera_high_phyl.gpkg")   
saveRDS(final, "03_FinalData/06_200110_MZB_Denes_Schmerahigh_phyl.RDS")   
