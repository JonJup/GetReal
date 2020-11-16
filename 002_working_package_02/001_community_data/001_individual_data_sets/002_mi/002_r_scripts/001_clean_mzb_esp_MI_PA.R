### -------------------------------------------------- ###
# --- Cleaning Invertebrate data from Miguel Iglesias--- #
### -------------------------------------------------- ###

# 18.12.19
# Cleaning MZB PA data from Miguel Iglesias. 
# EPSG = 32630 UTM 30N


### --- OVERVIEW --- ### 
# 01. Setup
# 02. Cleaning Data 
# 03. Taxonomic Cleaning 
# 04. Final touches
### ---------------- ###



# 01. Setup -------------------------------------------------------------------
pacman::p_load(here, dplyr, taxize, magrittr, sf, stringr, data.table, lubridate, purrr, tmap)
# tmap_mode("view")
setwd(here("002_working_package_02/001_community_data/001_individual_data_sets/002_mi//"))


samples = readxl::read_excel("001_raw_data//PA data.xls") %>% 
  setDT()

unique(samples$Hydraenida)        

# 02. Cleaning data  ------------------------------------------------------

# melt table 
data = melt(samples, id.vars = c("Site", "Code", "XUTM30N", "YUTM30N"), 
     variable.name = "taxon",
     value.name = "abundance")

data = data[abundance != 0]

data = data[,
               list(
                 site = Site,
                taxon =  as.character(taxon),
                 "x.coord" = XUTM30N,
                 "y.coord" = YUTM30N,
                 "EPSG" = 32630
               )]



saveRDS(data, "03_FinalData/01_191218_data_before_taxon_clean_mzb_Miguel_Iglesias_PA.RDS")
data = readRDS("03_FinalData/01_191218_data_before_taxon_clean_mzb_Miguel_Iglesias_PA.RDS")

# 03. Taxonomic Cleaning  ----------------------------------------------------------------

TU = sort(unique(data$taxon))

classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191218_classification.object_mzb_Miguel_Iglesias_PA.RDS")
classification.object = readRDS("03_FinalData/02_191218_classification.object_mzb_Miguel_Iglesias_PA.RDS")

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

for (i in 1:(nrow(taxontable) + 1)) {
        
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
                                               tax[[1]][which(tax[[1]][, 2] == "family"), 1],  tax[[1]][1][nrow(tax[[1]]),])
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
saveRDS(taxontable, "03_FinalData/03_191218_initial_taxon_clean_mzb_Miguel_Iglesias_PA.RDS")
taxontable = readRDS("03_FinalData/03_191218_initial_taxon_clean_mzb_Miguel_Iglesias_PA.RDS")

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA" | family != "NA" | genus != "NA" | species != "NA", 
           clean := T]
taxontable[clean == F, taxon] %>% sort

saveRDS(taxontable, "03_FinalData/04_191218_post_correction_taxontable_mzb_Miguel_Iglesias_PA.RDS")
taxontable = readRDS("03_FinalData/04_191218_post_correction_taxontable_mzb_Miguel_Iglesias_PA.RDS")


# add taxonomical information to data 
data2 = left_join(data, taxontable,  by = "taxon") %>%  
  setDT
data3 <- data2

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

## -- old -- ## 
what_taxa_level <- list()
# list to hold each order in data sets and the respective taxize results. 
source("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/add_higher_taxa.R")

# load previous solutions 
pre_order <- readRDS("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/data_fix_ord.RDS")
pre_famil <- readRDS("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/data_fix_fam.RDS")

for (i in 1:nrow(pre_order)) {
  if (pre_order[i,1] %in% c(data3$order)) {
    ids <- which(data3$order == as.character(pre_order[i,1]))
    data3[ids, "subclass"] <- pull(pre_order[i,2])
    data3[ids, "class"]    <- pull(pre_order[i,3])
    data3[ids, "phylum"]   <- pull(pre_order[i,4])
    data3[ids, "kingdom"]  <- pull(pre_order[i,5])
  }
}
for (i in 1:nrow(pre_famil)) {
  if (pre_famil[i,1] %in% c(data3$family)) {
    ids <- which(data3$family == pull(pre_famil[i,1]))
    data3[ids, "subclass"] <- pull(pre_order[i,2])
    data3[ids, "class"]    <- pull(pre_order[i,3])
    data3[ids, "phylum"]   <- pull(pre_order[i,4])
    data3[ids, "kingdom"]  <- pull(pre_order[i,5])
  }
}
# character vector with all orders in data set 
yes_order <- sort(data3[!is.na(order) & is.na(kingdom), unique(order)])
yes_order <- yes_order[-which(yes_order == "NA")]

data3[order == "NA" & is.na(kingdom)]

# loop through order names and classify with taxize. Use the enzyclopedia of life  
for (j in 1:length(yes_order)) {
  loop_taxon <- yes_order[j]
  what_taxa_level[[j]] <- classification(loop_taxon, "eol")
}

## -- check results 
unique(data3$kingdom)
unique(data3$phylum) %>% sort
unique(data3$class) %>% sort
unique(data3$subclass) %>% sort

add_pre_order <- data.table(order = character(length(yes_order)), 
                            subclass = character(length(yes_order)), 
                            class = character(length(yes_order)), 
                            phylum = character(length(yes_order)), 
                            kingdom = character(length(yes_order)) 
)

# Loop pulling subclass, class, phylum and kingdom entries out of respective taxize result table 
for (i in 1:length(yes_order)) {
  
  loop_taxon    <- yes_order[i]
  add_pre_order$order[i] <- loop_taxon
  # loop_clasi <- classification(loop_taxon, "eol")
  # loop_clasi <- what_taxa_level[loop_taxon]
  loop_clasi <- what_taxa_level[[i]][[1]]
  
  data3_rows <- which(data3$order == loop_taxon)
  
  if ("subclass" %in% loop_clasi[,2]) {
    data3$subclass[data3_rows]      <- loop_clasi[which(loop_clasi[,2] == "subclass"), 1] 
    add_pre_order$subclass[i] <- loop_clasi[which(loop_clasi[,2] == "subclass"), 1] 
  }
  if ("class" %in% loop_clasi[, 2]) {
    data3$class[data3_rows]      <- loop_clasi[which(loop_clasi[, 2] == "class")  , 1]
    add_pre_order$class[i] <- loop_clasi[which(loop_clasi[, 2] == "class")  , 1]
  }
  if ("phylum" %in% loop_clasi[, 2]) {
    data3$phylum[data3_rows]      <- loop_clasi[which(loop_clasi[, 2] == "phylum") , 1]
    add_pre_order$phylum[i] <- loop_clasi[which(loop_clasi[, 2] == "phylum") , 1]
  }
  if ("kingdom" %in% loop_clasi[, 2]) {
    data3$kingdom[data3_rows]      <- loop_clasi[which(loop_clasi[, 2] == "kingdom"), 1]
    add_pre_order$kingdom[i] <- loop_clasi[which(loop_clasi[, 2] == "kingdom"), 1]
    
  }
  
}

# clean new rows -- example 
add_pre_order[order == "Branchiobdellida", 
              c("subclass", "class", "kingdom") := .(NA, NA, "Animalia")]
add_pre_order[order == "Onychura", c("subclass", "kingdom") := .(NA, "Animalia")]

pre_order <- rbindlist(list(add_pre_order, pre_order))

saveRDS(pre_order, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/data_fix_ord.RDS")

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
data3[subclass == "Hydroidolina Collins 2000", subclass := "Hydroidolina"]
data3[subclass == "Neocopepoda Huys & Boxshall 1991", subclass := "Neocopepoda"]
data3[subclass == "Podocopa G. O. Sars 1866", subclass := "Podocopa"]
data3[subclass == "Thecostraca Gruvel 1905", subclass := "Thecostraca"]
data3[subclass == "Hirudinea", class := "Clitellata"]

## -- end old -- ## 

data3[is.na(taxon)]
data3[species == "NA", taxon] %>% unique %>% sort
data3[is.na(species)]
data3[genus == "NA", taxon] %>% unique %>% sort
data3[is.na(genus)]
data3[family == "NA"] %>% unique(by = "taxon")
data3[is.na(family)]
data3[order == "NA", list(taxon,genus, family, order)] %>% unique(by = "taxon")
data3[is.na(order)]

data3[species == "NA", species := NA]
data3[genus   == "NA", genus   := NA]
data3[family  == "NA", family  := NA]
data3[order   == "NA", order   := NA]

data4 = data3

saveRDS(data4, paste0("03_FinalData/05b_",Sys.Date(),"_final_taxon_join_mzb_Miguel_Iglesias_PA_high_phly.RDS"))
data4 <- readRDS("03_FinalData/mzb/05b_2020-05-27_final_taxon_join_mzb_Miguel_Iglesias_PA_high_phly.RDS")
# 04. Final touches -------------------------------------------------------

data4[species == "Notonectidae", c("species", "genus") := NA]
data4[family == "Notonectidae"]

unique_sites = unique(data4$site)


data4[, c("site_id") := list(
        map_int(data4$site, ~ which(unique_sites == .x ))
      
)]


data4[, site_id2 := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", "XXX","_mzb_Miguel_Iglesias_PA")]

data5 = data4

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
# };beepr::beep()

for (i in seq_along(colnames(data5))) {
        x = pull(data5[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data5)[i])
        
}


data6 = data5[, list(
        gr_sample_id,
        original_site_name = site,
        date = NA,
        year = NA,
        season = NA,
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
        abundance = NA,
        pristine = NA,
        # n.species,
        # n.genus,
        # n.family,
        # n.order,
        x.coord = x.coord,
        y.coord = y.coord,
        EPSG = 32630,
        data.set = "mzb_Miguel_Iglesias_PA",
        n.samples = 1
        
)]

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
# test1 = final %>% setDT
# test1 = unique(test1, by = "site_id")
# test1 = st_as_sf(test1, crs = 4326)
# tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, paste0("03_FinalData/mzb/09_",Sys.Date(),"_MZB_Miguel_Iglesias_PA_high_phyla.gpkg"))   
saveRDS(final,  paste0("03_FinalData/mzb/09_",Sys.Date(),"_MZB_Miguel_Iglesias_PA_high_phyla.RDS" ))   


