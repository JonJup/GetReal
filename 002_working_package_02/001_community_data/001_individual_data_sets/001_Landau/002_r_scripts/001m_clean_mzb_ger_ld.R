### --------------------------------------###
# --- Cleaning Invertebrate data from --- #
### --------------------------------------###

# 14.10.19 + 05.05.20 for inclusion of higher taxa 
# Cleaning MZB data from LD. 
# EPSG = 31463

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Cleaning Data 
# 03. Taxonomic Cleaning 
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
pacman::p_load(here, dplyr, taxize, magrittr, sf, stringr, data.table, lubridate, purrr, tmap, beepr)

tmap_mode("view")

setwd(here("001_Landau"))
# or
#setwd("~/01_Uni/03_GetReal/02_WP_02/Community Data/01_Landau/")

samples <- fread("01_OriginalData/mzb_samples.csv")
sites   <- fread("01_OriginalData/mzb_sites.csv") 
        

# 02. Cleaning data  ------------------------------------------------------

#remove rows with zero abundance 
samples = samples[ind_qm != 0]

## drop columns  
samples2 = samples[, c("date", "taxon", "site_id", "ind_qm") ]
rm(samples)

# In samples the sites with TH in their site_id seem to have mistakes. All TH ..
# sites in the samples data go like TH_TH_xxxx while those in the sites data go
# TH_xxxx. Hence I remove the first TH 
samples2$site_id = str_replace_all(samples2$site_id, "TH_TH_", "TH_")
sites2 = sites[, c("site_id", "stream", "site_name", "geom")]

#fix site coordinates 
#Geometry type PostgreSQL columns They can be converted with sf see
#https://github.com/r-dbi/RPostgres/issues/114 This converts the geom column
#which holds Postgres Geom codes to xy coordinates and also returns the
#projection.
coord <- st_as_sfc(
  structure(
    sites2$geom, 
    class = "WKB"
  ),
  EWKB = TRUE
)
coord2 = st_coordinates(coord) %>% data.frame()
sites3  <- bind_cols(
  sites2,
  coord2,
  EPSG = rep(31463, nrow(sites2))
) 
sites3 = sites3[,-c("geom")]
# join data sets 
data = left_join(samples2, sites3) %>% setDT
rm(coord,coord2,samples2,sites,sites2,sites3)

# fix date column
data[,c("date", "year", "month", "day") := list(ymd(date), year(date), month(date), day(date))]
data[,"season" := ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% c(3,4,5), "spring", ifelse(month %in% c(6,7,8), "summer", "autumn")))]
data2 = data
        
saveRDS(data,   "03_FinalData/01_191217_data_before_taxon_clean_mzb_Landau.RDS")
data2 = readRDS("03_FinalData/01_191217_data_before_taxon_clean_mzb_Landau.RDS")

# 03. Taxonomic Cleaning  ----------------------------------------------------------------

TU = sort(unique(data2$taxon))

classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191217_classification.object_mzb_Landau.RDS")
classification.object = readRDS("03_FinalData/02_191216_classification.object_mzb_Landau.RDS")

taxontable = data.table(
        taxon = TU,
        species = "NA",
        genus = "NA",
        family = "NA",
        order = "NA",
        clean = F
)

# sourcing does not work properly. Open the file up and run it manually. 
clean_mzb_synonyms.R


taxontable[order != "NA" | family != "NA" | genus != "NA" | species != "NA", clean := T]
taxontable[clean == F, taxon] %>% length

response.vector = NULL
response = c()
  
for (i in 3374:(nrow(taxontable) + 1)) {
        
        # skip if clean
        if (taxontable$clean[i] == TRUE) {
                next()
        }
        # fill tax object 
        select.taxon = taxontable$taxon[i]
        tax = classification.object[select.taxon]
        
        if (is.null(unlist(tax))) next() 
        
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
saveRDS(taxontable, "03_FinalData/03_191217_initial_taxon_clean_mzb_Landau.RDS")
taxontable = readRDS("03_FinalData/03_191217_initial_taxon_clean_mzb_Landau.RDS")

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA" | family != "NA" | genus != "NA" | species != "NA", 
           clean := T]
taxontable[clean == F, taxon] %>% sort


#saveRDS(taxontable,  "03_FinalData/04_191217_post_correction_taxontable_mzb_Landau.RDS")
taxontable = readRDS("03_FinalData/04_191217_post_correction_taxontable_mzb_Landau.RDS")

data3 <- left_join(data2, 
                   taxontable,
                   by = "taxon") %>% 
        setDT


# Highlevel Taxonomic Groups  ---------------------------------------------
what_family <- list()
what_order  <- list()

# load previous solutions 
pre_order <- readRDS("../999_quality_check/data_fix_ord.RDS")
pre_famil <- readRDS("../999_quality_check/data_fix_fam.RDS")


# list to hold each order in data sets and the respective taxize results.
# For some reason results change when this script is run directly (i.e. open it up and ight source in the upper right corner) compared to when I run it here.
# So open it up. 

source("../999_quality_check/add_higher_taxa.R")


# compare to previous solutions 
for (i in 1:nrow(pre_order)) {
  if (pre_order[i, 1] %in% c(data3$order)) {
    ids <- which(data3$order == as.character(pre_order[i, 1]))
    if (NA %in% data3[ids, unique(kingdom)]) {
      data3[ids, "subclass"] <- pull(pre_order[i, 2])
      data3[ids, "class"]    <- pull(pre_order[i, 3])
      data3[ids, "phylum"]   <- pull(pre_order[i, 4])
      data3[ids, "kingdom"]  <- pull(pre_order[i, 5])
    }
  }
}; beep()
for (i in 1:nrow(pre_famil)) {
  if (pre_famil[i, 1] %in% c(data3$family)) {
    if (NA %in% data3[ids, unique(kingdom)]) {
      ids <- which(data3$family == pull(pre_famil[i, 1]))
      data3[ids, "subclass"] <- pull(pre_order[i, 2])
      data3[ids, "class"]    <- pull(pre_order[i, 3])
      data3[ids, "phylum"]   <- pull(pre_order[i, 4])
      data3[ids, "kingdom"]  <- pull(pre_order[i, 5])
    }
  }
}; beep()

# character vector with all orders in data set 
(yes_order <- sort(data3[!is.na(order) & is.na(kingdom), unique(order)]))
yes_order <- yes_order[-which(yes_order == "NA")]
data3[order == "NA" & is.na(kingdom), unique(taxon)]

# loop through order names and classify with taxize. Use the enzyclopedia of life  
for (j in 1:length(yes_order)) {
  loop_taxon <- yes_order[j]
  what_order[[j]] <- classification(loop_taxon, "eol")
}

(yes_famil <- sort(data3[!is.na(family) & is.na(kingdom), unique(family)]))


## -- clean up 
unique(data3$kingdom)
unique(data3$phylum)   %>% sort
unique(data3$class)    %>% sort
unique(data3$subclass) %>% sort


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
data3[subclass == "Hydroidolina Collins 2000", subclass := "Hydroidolina"]
data3[subclass == "Neocopepoda Huys & Boxshall 1991", subclass := "Neocopepoda"]
data3[subclass == "Podocopa G. O. Sars 1866", subclass := "Podocopa"]
data3[subclass == "Thecostraca Gruvel 1905", subclass := "Thecostraca"]

## -- extract fix table for future 
data_fix_ord <- data3[!is.na(order), c("order", "subclass", "class", "phylum", "kingdom")]
data_fix_ord <- unique(data_fix_ord, by = "order")

data_fix_fam <- data3[is.na(order) & !is.na(family), c("family", "subclass", "class", "phylum", "kingdom")]
data_fix_fam <- unique(data_fix_fam, by = "family")

saveRDS(data_fix_ord, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/data_fix_ord.RDS")
saveRDS(data_fix_fam, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/data_fix_fam.RDS")


## -- fixes before final touches begin 

data4 <- data3

# insert true NAs 
data4[species == "NA", species := NA]
data4[genus   == "NA", genus   := NA]
data4[family  == "NA", family  := NA]
data4[order   == "NA", order   := NA]

# save to file 
#saveRDS(data4, "03_FinalData/05b_200527_final_taxon_join_mzb_Landau_high_phly.RDS")
data4 = readRDS("03_FinalData/05b_200527_final_taxon_join_mzb_Landau_high_phly.RDS")

# 04. Final touches -------------------------------------------------------

unique_sites = unique(data4$site_id)
unique_dates = unique(data4$date) %>% sort

data4[, c("site_id", "date_id") := list(
        map_int(data4$site_id, ~ which(unique_sites == .x )),
        map_int(data4$date, ~ which(unique_dates == .x ))
)];beep()


data4[, site_id2 := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))];beep()

data4[, date_id2 := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(date_id))];beep()

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_mzb_Landau")]

n.sample.data = data4[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data5 = data4[n.sample.data, on = "site_id2"]

# for (i in seq_along(unique(data5$gr_sample_id))) {
#         
#         temp.reduced.data = data5[gr_sample_id == unique(gr_sample_id)[i]]
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
#         data5[gr_sample_id == unique(gr_sample_id)[i],
#               c("n.species", "n.genus", "n.family", "n.order") := 
#                       list(n.spec, n.gen, n.fam, n.ord)
#               ]
#         
#         print(i)        
# };beep()

# Where are still NAs?
for (i in seq_along(colnames(data5))) {
        x = pull(data5[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data5)[i])
        
}

# -> only taxonomic variables 

data6 = data5[, list(
        gr_sample_id,
        original_site_name = site_id,
        date,
        year,
        season,
        site_id = site_id2,
        date_id,
        species,
        genus,
        family,
        order,
        subclass,
        class,
        phylum,
        kingdom,
        abundance = ind_qm,
        pristine = NA,
        #n.species,
        #n.genus,
        #n.family,
        #n.order,
        x.coord = X,
        y.coord = Y,
        EPSG,
        data.set = "MZB_LD",
        n.samples
        
)]

# Determine Levels for Taxa -----------------------------------------------

## -- Phylum 
n_phyl <- length(data6[,unique(phylum)])

level_data_phylum <- data.table(phylum_name = character(n_phyl), 
                                species  = numeric(n_phyl),
                                genus    = numeric(n_phyl),
                                family   = numeric(n_phyl),
                                order    = numeric(n_phyl),
                                subclass = numeric(n_phyl),
                                class    = numeric(n_phyl),
                                phylum   = numeric(n_phyl),
                                data_set = "LD")

for (i in 1:n_phyl) {
  
  level_data_phylum[i, phylum_name := data6[,unique(phylum)][i]]
  
  loop_sub <- data6[phylum == data6[,unique(phylum)][i]]
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
n_class <- length(data6[!is.na(class),unique(class)])

level_data_class <- data.table(
  phylum_name = character(n_class),
  class_name  = character(n_class),
  species    = numeric  (n_class),
  genus      = numeric  (n_class),
  family     = numeric  (n_class),
  order      = numeric  (n_class),
  subclass   = numeric  (n_class),
  class      = numeric  (n_class),
  data_set   = "LD"
)

for (i in 1:n_class) {
  
  loop_class <- data6[!is.na(class),unique(class)][i]
  
  level_data_class[i, class_name :=  loop_class]
  level_data_class[i, phylum_name := data6[class == class_name, unique(phylum)]]
  
  loop_sub <- data6[class == loop_class]
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
n_subclass <- length(data6[!is.na(subclass), unique(subclass)])

level_data_subclass <- data.table(
  phylum_name    = character(n_subclass),
  class_name     = character(n_subclass),
  subclass_name  = character(n_subclass),
  species        = numeric   (n_subclass),
  genus          = numeric   (n_subclass),
  family         = numeric   (n_subclass),
  order          = numeric   (n_subclass),
  subclass       = numeric   (n_subclass),
  data_set       = "LD"
)

for (i in 1:n_subclass) {
  
  loop_subclass <- data6[!is.na(subclass),unique(subclass)][i]
  
  level_data_subclass[i, subclass_name := loop_subclass]
  level_data_subclass[i, class_name    := data6[subclass == loop_subclass, unique(class)]]
  level_data_subclass[i, phylum_name   := data6[subclass == subclass_name, unique(phylum)]]
  
  loop_sub <- data6[subclass == loop_subclass]
  loop_obs <- nrow(loop_sub)
  
  level_data_subclass[i, species   := round(loop_sub[!is.na(species),                                                                                    .N]/ loop_obs * 100,2)]
  level_data_subclass[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                                                                    .N]/ loop_obs * 100,2)]
  level_data_subclass[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),                                                   .N]/ loop_obs * 100,2)]
  level_data_subclass[i, order     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),                                   .N]/ loop_obs * 100,2)]
  level_data_subclass[i, subclass  := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) &  is.na(order) & !is.na(subclass),                .N]/ loop_obs * 100,2)]
}
setorderv(level_data_subclass, cols = c("phylum_name", "class_name", "subclass_name"))

## -- order 
n_order <- length(data6[!is.na(order), unique(order)])

level_data_order <- data.table(
  phylum_name    = character (n_order),
  class_name     = character (n_order),
  subclass_name  = character (n_order),
  order_name     = character (n_order),
  species        = numeric   (n_order),
  genus          = numeric   (n_order),
  family         = numeric   (n_order),
  order          = numeric   (n_order),
  data_set       = "LD"
)

for (i in 1:n_order) {
  
  loop_order <- data6[!is.na(order),unique(order)][i]
  
  level_data_order[i, order_name    := loop_order]
  level_data_order[i, subclass_name := data6[order == loop_order, unique(subclass) ]]
  level_data_order[i, class_name    := data6[order == loop_order, unique(class)]]
  level_data_order[i, phylum_name   := data6[order == loop_order, unique(phylum)]]
  
  loop_sub <- data6[order == loop_order]
  loop_obs <- nrow(loop_sub)
  
  level_data_order[i, species   := round(loop_sub[!is.na(species),                                                                                    .N]/ loop_obs * 100,2)]
  level_data_order[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                                                                    .N]/ loop_obs * 100,2)]
  level_data_order[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),                                                   .N]/ loop_obs * 100,2)]
  level_data_order[i, order     := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order),                                   .N]/ loop_obs * 100,2)]
}
setorderv(level_data_order, cols = c("phylum_name", "class_name", "subclass_name", "order_name"))

## -- family 
n_family <- length(data6[!is.na(family), unique(family)])

level_data_family <- data.table(
  phylum_name    = character (n_family),
  class_name     = character (n_family),
  subclass_name  = character (n_family),
  order_name     = character (n_family),
  family_name    = character (n_family),
  species        = numeric   (n_family),
  genus          = numeric   (n_family),
  family         = numeric   (n_family),
  data_set       = "LD"
  
)

for (i in 1:n_family) {
  
  loop_family <- data6[!is.na(family),unique(family)][i]
  
  level_data_family[i, family_name   := loop_family]
  level_data_family[i, order_name    := data6[family == loop_family, unique(order)]]
  level_data_family[i, subclass_name := data6[family == loop_family, unique(subclass) ]]
  level_data_family[i, class_name    := data6[family == loop_family, unique(class)]]
  level_data_family[i, phylum_name   := data6[family == loop_family, unique(phylum)]]
  
  loop_sub <- data6[family == loop_family]
  loop_obs <- nrow(loop_sub)
  
  level_data_family[i, species   := round(loop_sub[!is.na(species),                                 .N]/ loop_obs * 100,2)]
  level_data_family[i, genus     := round(loop_sub[ is.na(species) & !is.na(genus),                 .N]/ loop_obs * 100,2)]
  level_data_family[i, family    := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),.N]/ loop_obs * 100,2)]

}
setorderv(level_data_family, cols = c("phylum_name", "class_name", "subclass_name", "order_name", "family_name"))


level_list <- list(level_data_phylum, level_data_class, level_data_subclass,level_data_order, level_data_family)

saveRDS(level_list, paste0("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/001_Landau/03_FinalData/08_",Sys.Date(),"_level_list.RDS"))

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
# test1 = final %>% setDT
# test1 = unique(test1, by = "site_id")
# test1 = st_as_sf(test1, crs = 4326)
# tm_shape(test1) + tm_dots(col = "n.order")

st_write(final, paste0("03_FinalData/09_",Sys.Date(),"_MZB_Landau_higher_taxa.gpkg"));beep()
saveRDS(final,  paste0("03_FinalData/09_",Sys.Date(),"_MZB_Landau_higher_taxa.RDS" ))
