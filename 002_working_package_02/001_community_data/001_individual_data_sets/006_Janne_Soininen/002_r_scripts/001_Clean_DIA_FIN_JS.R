### ----------------------------------------- ### 
# --- Clean Diatom data from Janne Soininen --- #
### ----------------------------------------- ###

## 02.08.19

# In this script I clean the diatom data from Janne Soininen

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
# libraries 
pacman::p_load(dplyr,
               stringr, 
               readxl, 
               sf, 
               tidyr, 
               data.table, 
               taxize, 
               magrittr, 
               lubridate, 
               purrr,
               tmap)
tmap_mode("view")
# wd 
setwd(here::here("06_Janne_Soininen/"))


# IO
Dia.Finland.Soininen <- read_excel("01_OriginalData/Species_diatoms_Finland.xlsx") %>% 
  setDT
Env.Finland.Soininen <- read_excel("01_OriginalData/Streams_environment_Finland.xlsx") %>% 
  setDT

non_pristine_sites = c("Tikkuri", "Ruutin", "Konigst", "Shellin", "Klaukka", "Kuhak", "Myllyp", "Mylly", "Nukari", 
                       "Koivum", "Matkun", "Myllyk", "VanhMyl", "Karaja", "Jokelan")

# 02. Data Cleaning ----------------------------------------------------------------

# drop columns 
Dia.Finland.Soininen2 = Dia.Finland.Soininen[,
                                             c("abbr. name", "Species", "genus") := list(NULL, NULL, NULL) 
                                             ]
# remove unobserved species 
Dia.Finland.Soininen2 %>% 
        gather("Site","Abundance",-'full name') %>% 
        filter(Abundance != 0) ->
        Dia.Finland.Soininen2

# join lattitude and longitude to biological data based on site names 
Dia.Finland.Soininen3 <- 
        left_join(Dia.Finland.Soininen2,
                  Env.Finland.Soininen[,c("Site","longitude","latitude")],
                  by = "Site")

data = Dia.Finland.Soininen3 %>%
  setDT
names(data) <- c("taxon", "Site", "abundance", "longitude", "latitude") 

data[,pristine := ifelse(Site %in% non_pristine_sites, 0, 1)]

saveRDS(data, "03_FinalData/01_191216_data_before_taxon_clean_Janne_Soininen.RDS")
data = readRDS("03_FinalData/01_191216_data_before_taxon_clean_Janne_Soininen.RDS")

# 03. Taxonomic cleaning --------------------------------------------------------

# vector with all taxa names 
TU = unique(data$taxon)

# classify with taxize package 
classification.object = classification(TU, db = "gbif")

# save classification.object as backup
saveRDS(classification.object, "03_FinalData/02_191216_classification.object_Janne_Soininen.RDS")
classification.object = readRDS("03_FinalData/02_191216_classification.object_Janne_Soininen.RDS")


# this table will hold all the taxonomic information. Once this is completly
# filled it will be joined with  "data".
taxontable = data.table(
        taxon = TU,
        species = "NA",
        genus = "NA",
        family = "NA",
        order = "NA",
        clean = F
)

# Both scirpts contain cleaning of synonyms or erros from gbif. Sourcing does
# not work properly. Open the file up and run it manually.
clean_diatoms_synonyms.R
clean_gbif_errors.R

# validate rows filled by scripts 
taxontable[order != "NA", clean := T]

# how many are still missing? 
taxontable[clean == F, taxon] %>% length

# create empty variables for loop 
response.vector = NULL
response = c()

# loop that asks wether the results of the classfication look sensible.
for (i in 1:nrow(taxontable)) {
        
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
saveRDS(taxontable, "03_FinalData/03_191216_initial_taxon_clean_Janne_Soininen.RDS")
taxontable = readRDS("03_FinalData/03_191216_initial_taxon_clean_Janne_Soininen.RDS")

# run cleaning scripts ...synonyms for synonyms and gbif_errors for taxa that
# gbif does not know or erroneously rates as Synonyms.
clean_diatoms_synonyms.R
clean_gbif_errors.R

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon]

# save taxontable 
saveRDS(taxontable, "03_FinalData/04_191216_post_correction_taxontable_Janne_Soininen.RDS")
taxontable = readRDS("03_FinalData/04_191216_post_correction_taxontable_Janne_Soininen.RDS")

# add taxonomical information to data 
data2 = left_join(data,
                  taxontable,
                  by = "taxon") %>% 
        setDT

# data2[species == "NA"] %>% unique(by = "taxon")
# data2[is.na(species)]
# data2[genus == "NA"] %>% unique(by = "taxon")
# data2[is.na(genus)]
# data2[family == "NA"] %>% unique(by = "taxon")
# data2[is.na(family)]
# data2[order == "NA"] %>% unique(by = "taxon")
# data2[is.na(order)]

data2[species == "NA", species := NA]
data2[genus == "NA", genus  := NA]
data2[family == "NA", family := NA]
data2[order == "NA", order := NA]

source("~/01_Uni/03_GetReal/02_WP_02/Community Data/quality_check/lists_of_accepted_orders.R")
mzb.orders = unique(data2[, order])
index = mzb.orders %in% accepted.diatoms
mzb.orders[!index] %>% sort

data4 = data2

saveRDS(data4, "03_FinalData/05_191216_final_taxon_join_Janne_Soininen.RDS")
data4 = readRDS("03_FinalData/05_191216_final_taxon_join_Janne_Soininen.RDS.RDS")

# 04. Final touches -------------------------------------------------------

unique_sites = unique(data4$Site)

data4[, c("site_id") := list(
        map_int(data4$Site, ~ which(unique_sites == .x ))
)]


data4[, site_id2 := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", "XXX","_Janne_Soininen")]

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

# all sampling was conducted in August 2001 or 2004. See Soininen (2008): The
# Ecological Characteristics of Idiosyncratic and Nested Diatoms
data6 = data5[, list(
        gr_sample_id,
        original_site_name = Site,
        date = NA,
        year = NA ,
        season = "summer",
        site_id,
        date_id = NA,
        species,
        genus,
        family,
        order,
        abundance,
        pristine,
        n.species,
        n.genus,
        n.family,
        n.order,
        x.coord = longitude,
        y.coord = latitude,
        EPSG = 2393,
        data.set = "Janne_Soininen",
        n.samples = 1
        
)]

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, "03_FinalData/06_191216_DIA_Janne_Soininen.gpkg")   
