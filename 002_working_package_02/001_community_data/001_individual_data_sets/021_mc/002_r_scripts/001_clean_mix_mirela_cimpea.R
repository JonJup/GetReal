### ----------------------------------------- ### 
# --- Clean Diatom data from Mirela Cimpean --- #
### ----------------------------------------- ###

# date: 27.11.19 

# In this script I clean the diatom data from Mirela Cîmpean

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###


# 01. Setup -------------------------------------------------------------------
pacman::p_load(here, dplyr, taxize, magrittr, sf, stringr, data.table, lubridate, readxl, tmap, measurements)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/021_Mirela Cimpea/")
data1 <- read_xlsx("01_OriginalData/Aries_River_Romania_GetReal.xlsx", 
                        col_types = c("text", "text", "date", "text", "text", "text", "numeric"))

data2 <- read_xlsx("01_OriginalData/Capra_River_Romania_GetReal -.xlsx") %>% 
        setDT

data3 <- xlsx::read.xlsx2("01_OriginalData/Caras_River_Romania.xls", 
                          sheetIndex = 1, 
                          colIndex = 1:8, 
                          colClasses = c("character", "character", "Date", "character", "character", "character", "numeric")
                          ) %>% 
        setDT

data4 <- read_xlsx("01_OriginalData/Dejani_River_Romania_GetReal.xlsx") %>% 
        setDT

(data5 <- read_xlsx("01_OriginalData/Sebes_River_Romania_GetReal.xlsx") %>% 
        setDT)

(data6 <- xlsx::read.xlsx2("01_OriginalData/Tur_River_Romana.xls",
                           sheetIndex = 1, 
                           colClasses = c("character", "character", "Date", "character", "character", "character", "numeric")) %>%
                setDT)

# 02. Data Cleaning ----------------------------------------------------------


## -- data set 1 Aries river 
data1 = data1[-1,]
setDT(data1)

x = data1[,c("X coordinates", "Y coordinates")]

names(x) <- c("lat", "long")        
chd = substr(x$lat, 5,5)[1]
chm = substr(x$lat, 8,8)[1]
chs = substr(x$lat, 11, 11)[1]

x$lat %<>% str_remove("N") %>% str_trim    
x$long %<>% str_remove("E") %>% str_trim    

cd.lat = sp::char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.long = sp::char2dms(x$long, chd = chd, chm = chm, chs = chs)

data12 = data1[, list(
        site = Site_name,
        date = ymd(`sampling date`),
        taxon = `taxon name`,
        x.coord = as.numeric(cd.long),
        y.coord = as.numeric(cd.lat),
        EPSG = `Coordinate reference system`
)]
data12[, c("year", "season") := list(
        year = year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]

## -- data set 2 
data21 = data2[, list(
        site = Code,
        date = data2$`sampling date` %>% dmy(),
        taxon = data2$`taxon name`,
        x.coord = data2$Longitudine,
        y.coord = data2$Latitudine,
        EPSG = data2$`Coordinate reference system`
)]

x = data21[,c("x.coord", "y.coord")]

names(x) <- c("long", "lat")        
chd = "?"
chm = "'"
chs = "\""

x$lat %<>% str_remove_all("N") %>%
        str_trim
x$long %<>% str_remove_all("E") %>%
        str_trim

cd.lat = sp::char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.long = sp::char2dms(x$long, chd = chd, chm = chm, chs = chs)

data21 = data21[, c("x.coord", "y.coord", "year", "season") := list(
        as.numeric(cd.long),
        as.numeric(cd.lat),
        year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
        )]
# data212 = unique(data21, by = "site")
# data212 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[[1]])
# tm_shape(data212) + tm_dots()


## -- data set 3

data3 = data3[-1,]
x = data3[,c("X.coordinates.", "Y.coordinates.")]

names(x) <- c("lat", "long")        
chd = substr(x$lat, 5,5)[1]
chm = substr(x$lat, 8,8)[1]
chs = "\""

x$lat %<>% str_remove("N") %>% str_trim    
x$long %<>% str_remove("E") %>% str_trim    

cd.lat = sp::char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.long = sp::char2dms(x$long, chd = chd, chm = chm, chs = chs)

data32 = data3[, list(
        site = Site_name.,
        date = ymd(`sampling.date.`),
        taxon = `taxon.name.`,
        x.coord = as.numeric(cd.long),
        y.coord = as.numeric(cd.lat),
        EPSG = `Coordinate.reference.system.`
)]

data32[, c("year", "season") := list(
        year = year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]
# data312 = unique(data32, by = "site")
# data312 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[[1]])
# tm_shape(data312) + tm_dots()

## -- data set 4
data41 = data4[, list(
        site = Code,
        date = data4$`sampling date` %>% dmy(),
        taxon = data4$`taxon name`,
        x.coord = data4$Longitudine,
        y.coord = data4$Latitudine,
        EPSG = data4$`Coordinate reference system`
)]

x = data41[,c("x.coord", "y.coord")]

names(x) <- c("long", "lat")        
chd = "?"
chm = "'"
chs = "\""

x$lat %<>% str_remove_all("N") %>%
        str_trim
x$long %<>% str_remove_all("N") %>%
        str_trim

cd.lat = sp::char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.long = sp::char2dms(x$long, chd = chd, chm = chm, chs = chs)

data41 = data41[, c("x.coord", "y.coord", "year", "season") := list(
        as.numeric(cd.long),
        as.numeric(cd.lat),
        year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]
# data412 = unique(data41, by = "site")
# data412 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[[1]])
# tm_shape(data412) + tm_dots()

## -- data set 5
data51 = data5[, list(
        site = Code,
        date = data5$`sampling date` %>% dmy(),
        taxon = data5$`taxon name`,
        x.coord = data5$Longitudine,
        y.coord = data5$Latitudine,
        EPSG = data5$`Coordinate reference system`
)]

x = data51[,c("x.coord", "y.coord")]

names(x) <- c("long", "lat")        
chd = "?"
chm = "'"
chs = "\""

x$lat %<>% str_remove_all("N") %>%
        str_trim
x$long %<>% str_remove_all("N") %>%
        str_trim

cd.lat = sp::char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.long = sp::char2dms(x$long, chd = chd, chm = chm, chs = chs)

data51 = data51[, c("x.coord", "y.coord", "year", "season") := list(
        as.numeric(cd.long),
        as.numeric(cd.lat),
        year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]
# data512 = unique(data51, by = "site")
# data512 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[[1]])
# tm_shape(data512) + tm_dots()

## -- data set 6

data6 = data6[-1,]
x = data6[,c("X.coordinates.", "Y.coordinates.")]

names(x) <- c("lat", "long")        
chd = substr(x$lat, 5,5)[1]
chm = substr(x$lat, 8,8)[1]
chs = "\""

x$lat %<>% str_remove("N") %>% str_trim    
x$long %<>% str_remove("E") %>% str_trim    

cd.lat = sp::char2dms(x$lat, chd = chd, chm = chm, chs = chs)
cd.long = sp::char2dms(x$long, chd = chd, chm = chm, chs = chs)

data62 = data6[, list(
        site = Site_name.,
        date = ymd(`sampling.date.`),
        taxon = `taxon.name.`,
        x.coord = as.numeric(cd.long),
        y.coord = as.numeric(cd.lat),
        EPSG = `Coordinate.reference.system.`
)]

data62[, c("year", "season") := list(
        year = year(date),
        case_when(
                month(date) %in% c(12,1,2) ~ "winter",
                month(date) %in% c(3,4,5) ~ "spring",
                month(date) %in% c(6,7,8) ~ "summer",
                month(date) %in% c(9,10,11) ~ "autumn")
)]
# data612 = unique(data62, by = "site")
# data612 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[[1]])
# tm_shape(data612) + tm_dots()


## -- combine into one dataset 

data = rbindlist(list(data62, data51, data41, data32, data21, data12))
data2 = data

saveRDS(data2, "03_FinalData/01_191128_data_before_taxon_clean_mirela_cimpean.RDS")
data2 = readRDS("03_FinalData/01_191128_data_before_taxon_clean_mirela_cimpean.RDS")

rm(list = setdiff(ls(), "data2"))
# 03. Taxonomic cleaning ----------------------------------------------------------------

data2$taxon <- as.character(data2$taxon)
data2$taxon <- str_trim(data2$taxon)
TU = unique(data2$taxon) %>% sort

dia = data2[
        str_detect(taxon, "Achnanthidium") |
        str_detect(taxon, "Adlafia") |
        str_detect(taxon, "Amphora") |
        str_detect(taxon, "Aneumastus") |
        str_detect(taxon, "Aulacoseira") |
        str_detect(taxon, "Bacillaria") |
        str_detect(taxon, "Brachysira") |
        str_detect(taxon, "Brebissonia") |
        str_detect(taxon, "Caloneis") |
        str_detect(taxon, "Cocconeis") |
        str_detect(taxon, "Craticula") |
        str_detect(taxon, "Cyclotella") |
        str_detect(taxon, "Cymatopleura") |
        str_detect(taxon, "Cymbella") |
        str_detect(taxon, "Cymbopleura") |
        str_detect(taxon, "Diatoma") |
        str_detect(taxon, "Didymosphenia") |
        str_detect(taxon, "Diploneis") |
        str_detect(taxon, "Encyonema") |
        str_detect(taxon, "Eunotia") |
        str_detect(taxon, "Fragilaria") |
        str_detect(taxon, "Frustulia") |
        str_detect(taxon, "Gomphonella") |
        str_detect(taxon, "Gomphonema") |
        str_detect(taxon, "Gyrosigma") |
        str_detect(taxon, "Halamphora") |
        str_detect(taxon, "Hannaea") |
        str_detect(taxon, "Hantzschia") |
        str_detect(taxon, "Hippodonta") |
        str_detect(taxon, "Iconella") |
        str_detect(taxon, "Lemnicola") |
        str_detect(taxon, "Luticola") |
        str_detect(taxon, "Melosira") |
        str_detect(taxon, "Meridion") |
        str_detect(taxon, "Navicula") |
        str_detect(taxon, "Navigeia") |
        str_detect(taxon, "Neidium") |
        str_detect(taxon, "Nitzscha") |
        str_detect(taxon, "Odontidium") |
        str_detect(taxon, "Orthoseira") |
        str_detect(taxon, "Paraplaconeis") |
        str_detect(taxon, "Pinnularia") |
        str_detect(taxon, "Placoneis") |
        str_detect(taxon, "Psammothidium") |
        str_detect(taxon, "Reimeria") |
        str_detect(taxon, "Stauroneis") |
        str_detect(taxon, "Staurosirella") |
        str_detect(taxon, "Surirella") |
        str_detect(taxon, "Tabellaria") |
        str_detect(taxon, "Tetracyclus") |
        str_detect(taxon, "Tryblionella") |
        str_detect(taxon, "Ulnaria")
                    ]

mzb = data2[!taxon %in% dia$taxon]

saveRDS(dia, "03_FinalData/01_200316_dia_before_taxon_clean_mirela_cimpean.RDS")
saveRDS(mzb, "03_FinalData/01_200316_mzb_before_taxon_clean_mirela_cimpean.RDS")


# -- old 


classification.object = classification(TU, db = "gbif")
saveRDS(classification.object, "03_FinalData/02_191128_classification.object_mirela_cimpean.RDS")
classification.object = readRDS("03_FinalData/02_191128_classification.object_mirela_cimpean.RDS")

taxontable = data.table(
        taxon = TU,
        species = "NA",
        genus = "NA",
        family = "NA",
        order = "NA",
        clean = F
)
# sourcing does not work properly. Open the file up and run it manually. 
clean_diatoms_synonyms.R

taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% length

response.vector = NULL
response = c()

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
                                               tax[[1]][which(tax[[1]][, 2] == "family"), 1],
                                               paste(tax[[1]][nrow(tax[[1]]), 1],"is a ", tax[[1]][nrow(tax[[1]]), 2]))
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
saveRDS(taxontable, "03_FinalData/03_191128_initial_taxon_clean_mirela_cimpean.RDS")
taxontable = readRDS("03_FinalData/03_191128_initial_taxon_clean_mirela_cimpean.RDS")      


# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort

# remove special entries 

saveRDS(taxontable, "03_FinalData/04_191128_post_correction_taxontable_mirela_cimpean.RDS")
taxontable = readRDS("03_FinalData/04_191128_post_correction_taxontable_mirela_cimpean.RDS")
# add taxonomical information to data 
data3 = data2[taxontable, on = "taxon"]


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
(yes_famil <- sort(data3[!is.na(family) & is.na(kingdom), unique(family)]))
yes_order <- yes_order[-which(yes_order == "NA")]
data3[order == "NA" & is.na(kingdom)]

# loop through order names and classify with taxize. Use the enzyclopedia of life  
for (j in 1:length(yes_order)) {
        loop_taxon <- yes_order[j]
        what_order[[j]] <- classification(loop_taxon, "eol")
}
# remove Bacillariophyta ordo incertae sedis
what_order <- what_order[-4]

for (j in 1:length(yes_famil)) {
        loop_taxon <- yes_famil[j]
        what_family[[j]] <- classification(loop_taxon, "eol")
}

# Amphimelaniidae did not work -- fixed in add higher taxa 

add_pre_family <- data.table(family  = character(length(yes_famil)),
                             subclass = character(length(yes_famil)), 
                             class    = character(length(yes_famil)), 
                             phylum   = character(length(yes_famil)), 
                             kingdom  = character(length(yes_famil)) 
)
add_pre_order <- data.table(  order   = character(length(yes_order)), 
                              subclass = character(length(yes_order)), 
                              class    = character(length(yes_order)), 
                              phylum   = character(length(yes_order)), 
                              kingdom  = character(length(yes_order)) 
)

# Loop pulling subclass, class, phylum and kingdom entries out of respective taxize result table 
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
for (i in 1:length(yes_order)) {
        if (i == 4) next()
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

## mirella cimpea extra -- 
## remove diatoms 

data3 <- data3[!class %in% c("Diatomea", "Diatomeae", "Bacillariophyceae", "Mediophyceae")]


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

saveRDS(data4, "03_FinalData/05b_200518_final_taxon_join_mzb_MC_high_phyla.RDS")
data4 = readRDS("03_FinalData/05_200108_final_taxon_join_dia_RIVPACS.RDS")

## seperate diatoms and mzb 
data4[is.na(order)]
order.ver = unique(data4$order)

for (i in seq_along(order.ver)) {
        
        if (is.na(order.ver[[i]])) {
        
                data4[is.na(order), taxon_group :=  "MZB"]
                next()        
                
        }
        
        temp_class = classification(order.ver[[i]], db = "itis")
        
        if (is.na(temp_class[[1]])) next()
        
        data4[order == order.ver[[i]], taxon_group := ifelse("Animalia" %in% temp_class[[1]][1,], "MZB", "Diatom")]
        rm(temp_class)
}

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
                                data_set = "MC")

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
                               data_set = "MC")

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
        data_set = "MC"
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
        data_set = "MC"
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
        data_set = "MC"
        
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
saveRDS(level_list, "~/01_Uni/03_GetReal/002_WP_02/001_Community Data/021_Mirela Cimpea/03_FinalData/08_2020-05-18_level_list_mzb_MC.RDS")

# 04. Final Touches -------------------------------------------------------
data4.mzb <- data4
# assign gr_site_id
for (i in seq_along(unique(data4.mzb$site))) {
        
        site_ide_var = i 
        length_of_id = nchar(trunc(i))
        site_ide_var2 = case_when(length_of_id == 1 ~ paste0("0000",i),
                                  length_of_id == 2 ~ paste0("000",i),
                                  length_of_id == 3 ~ paste0("00",i),
                                  length_of_id == 4 ~ paste0("0",i),
                                  length_of_id == 5 ~ paste0(i))
        
        data4.mzb[site == unique(data4.mzb$site)[i], site_id := site_ide_var2]
        
        dates = data4.mzb[site_id == site_ide_var2, "date"] %>% 
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
                
                data4.mzb[site_id == site_ide_var2 & date == dates[k], date_id := data.filler]
        }
        
        data4.mzb[site_id == site_ide_var2, n.samples := NumberOfSamplings]
        
}
data4.mzb[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_MZB_mirella_cimpean")]

for (i in seq_along(unique(data4.diatom$site))) {

        site_ide_var = i 
        length_of_id = nchar(trunc(i))
        site_ide_var2 = case_when(length_of_id == 1 ~ paste0("0000",i),
                                  length_of_id == 2 ~ paste0("000",i),
                                  length_of_id == 3 ~ paste0("00",i),
                                  length_of_id == 4 ~ paste0("0",i),
                                  length_of_id == 5 ~ paste0(i))
        
        data4.diatom[site == unique(data4.diatom$site)[i], site_id := site_ide_var2]
        
        dates = data4.diatom[site_id == site_ide_var2, "date"] %>% 
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
                
                data4.diatom[site_id == site_ide_var2 & date == dates[k], date_id := k]
        }
        
        data4.diatom[site_id == site_ide_var2, n.samples := NumberOfSamplings]
        
}
data4.diatom[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_DIA_mirella_cimpean")]


# there is a NA site with one observation in the mzb data. I will drop it. 
data4.mzb = data4.mzb[gr_sample_id != "site_NA_date_NA"]
beepr::beep()


# number of species 

for (i in seq_along(unique(data4.mzb$gr_sample_id))) {
        
        id = unique(data4.mzb$gr_sample_id)[i]
        temp.reduced.data = data4.mzb[gr_sample_id == id]
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

        data4.mzb[gr_sample_id == id, c("n.species", "n.genus", "n.family", "n.order") := 
                          list(n.spec, n.gen, n.fam, n.ord)]

}

for (i in seq_along(unique(data4.diatom$gr_sample_id))) {
        
        id = unique(data4.diatom$gr_sample_id)[i]
        temp.reduced.data = data4.diatom[gr_sample_id == id]
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
        
        data4.diatom[gr_sample_id == id, c("n.species", "n.genus", "n.family", "n.order") := 
                          list(n.spec, n.gen, n.fam, n.ord)]
        
}


# last checks: 
## i) nas 
for (i in seq_along(colnames(data4.mzb))) {
        x = pull(data4.mzb[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data4.mzb)[i])

}
for (i in seq_along(colnames(data4.diatom))) {
        x = pull(data4.mzb[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data4.diatom)[i])
        
}

unique(data4.mzb, by = c("site_id", "date_id")) %>% arrange(site_id, date_id)
unique(data4.mzb[n.species < 5], by = c("site_id", "date_id"))

unique(data4.diatom, by = c("site_id", "date_id")) %>% arrange(site_id, date_id)
unique(data4.diatom[n.species < 5], by = c("site_id", "date_id"))

data4.mzb[, data.set := "mirella_cimpea"]
data4.diatom[, data.set := "mirella_cimpea"]

data5.mzb = data4.mzb[, list(
        gr_sample_id,
        original_site_name = site,
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
        abundance = NA,
        pristine = NA,
        n.species,
        n.genus,
        n.family,
        n.order,
        data.set,
        n.samples,
        x.coord,
        y.coord,
        EPSG

)]
data5.diatom = data4.diatom[, list(
        gr_sample_id,
        original_site_name = site,
        date,
        year,
        season,
        site_id,
        date_id,
        species,
        genus,
        family,
        order,
        abundance = NA,
        pristine = NA,
        n.species,
        n.genus,
        n.family,
        n.order,
        data.set = "MC",
        n.samples,
        x.coord,
        y.coord,
        EPSG

)]

# there are NAs in the coordinates 
is.na(data5.mzb$y.coord) %>% sum
is.na(data5.mzb$x.coord) %>%  sum
is.na(data5.diatom$y.coord) %>% sum
is.na(data5.diatom$x.coord) %>%  sum

data5.mzb = data5.mzb[!is.na(y.coord)]

data_mzb_spatial = st_as_sf(data5.mzb, coords = c("x.coord", "y.coord"), crs = data5.mzb$EPSG[1])
data_diatom_spatial = st_as_sf(data5.diatom, coords = c("x.coord", "y.coord"), crs = data5.diatom$EPSG[1])

test1 = data_mzb_spatial %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = test1$EPSG[1])
tm_shape(data_mzb_spatial) + tm_dots()
test2 = data_diatom_spatial %>% setDT
test2 = unique(test1, by = "site_id")
test2 = st_as_sf(test2, crs = test2$EPSG[1])
tm_shape(test2) + tm_dots()


## save files to disk 
saveRDS( data_mzb_spatial, "03_FinalData/09_200518_MZB_mirella_cimpean_high_phyla.RDS")
st_write(data_mzb_spatial, "03_FinalData/09_200518_MZB_mirella_cimpean_high_phyla.gpkg")
st_write(data_diatom_spatial, "03_FinalData/06_191128_DIA_mirella_cimpean.gpkg")

# forgot n_samples .. 




