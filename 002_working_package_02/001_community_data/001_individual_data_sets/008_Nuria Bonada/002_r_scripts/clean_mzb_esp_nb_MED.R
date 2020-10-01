### ------------------------------------ ### 
# --- Clean MZB Nuria Bonada MEDRivers --- #
### ------------------------------------ ### 

## 31.10.19
## GR WP2 
## Here I clean and harmonize the biological data from Nuria Bonada from the MEDRivers  Project.

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Clean Data 
# 03. Taxonomic Cleaning 
# 04. Final Touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
#libraries
pacman::p_load(dplyr,stringr, readxl, sf, data.table,tidyr, taxize, magrittr, lubridate, purrr, tmap)
tmap_mode("view")
# also required: here 
setwd(here::here("08_Nuria Bonada/"))

samples = read_excel("01_OriginalData/Trichoptera_MEDRivers.xls", sheet = 5) %>%
 setDT

sites = read_excel("01_OriginalData/Trichoptera_MEDRivers.xls", sheet = 2) %>% 
  setDT

env = read_excel("01_OriginalData/Trichoptera_MEDRivers.xls", sheet = 3) %>% 
        setDT

# 02. Clean Data ----------------------------------------------------------

samples2 = samples[, list(
  site = Code,
  year = 1999,
  season = ifelse(Season == "a", "autumn", ifelse(
    Season == "p", "spring", ifelse(
      Season == "s", "summer", "winter"))),
  taxon = Taxon,
  abundance = Abundance
  )
]

sites2 = sites[, list(
  site = Code,
  x.coord = X_UTM,
  y.coord = Y_UTM
)]

data = samples2[sites2, 
                on = "site"]

env[, c("NO_three_qual", "NH_qual", "NO_two_qual") :=
              list(
                      case_when(`N-NO3` < 10 ~ 0,
                                `N-NO3` > 10 ~ 1),
                      case_when(`N-NH4` < 0.3 ~ 0,
                                `N-NH4` >= 0.3 ~ 1),
                      case_when(`N-NO2` < 0.1 ~ 0,
                                `N-NO2` >= 0.1 ~ 1)
              )]

env[, pre_pristine := NO_three_qual + NH_qual + NO_two_qual]
env[,pristine := ifelse(pre_pristine == 0, 1, 0)]

env2 = env[,.(Code, pristine)]
data2 = left_join(data, 
                  env2,
                  by = c("site" = "Code")) %>% 
        setDT

saveRDS(data2, "03_FinalData/01_191216_data_before_taxon_clean_MED_Nuria_Bonada.RDS")
data2 = readRDS("03_FinalData/01_191216_data_before_taxon_clean_MED_Nuria_Bonada.RDS")
# 03. Taxonomic Cleaning ------------------------------------------------------

TU = sort(unique(data$taxon))
classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191216_classification.object_MED_Nuria_Bonada.RDS")
classification.object = readRDS("03_FinalData/02_191216_classification.object_MED_Nuria_Bonada.RDS")

taxontable = data.table(
        taxon = TU,
        species = "NA",
        genus = "NA",
        family = "NA",
        order = "NA",
        clean = F
        )
#  Open the file up and run it manually. 
clean_mzb_synonyms.R
        
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% length
        
response.vector = NULL
response = c()
        
# In this loop two errors pop up. Both are due to the taxon beeing on a class level. 
# Can be skipped and will be taken care of in a later step.
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
saveRDS(taxontable, "03_FinalData/03_191216_initial_taxon_clean_MED_Nuria_Bonada.RDS")
taxontable = readRDS("03_FinalData/03_191216_initial_taxon_clean_MED_Nuria_Bonada.RDS")

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort
        
saveRDS(taxontable, "03_FinalData/04_191216_post_correction_taxontable_MED_Nuria_Bonada.RDS")
taxontable = readRDS("03_FinalData/04_191216_post_correction_taxontable_MED_Nuria_Bonada.RDS")

data3 = left_join(data2,
                  taxontable, 
                  by = "taxon") %>% 
        setDT


# data3[species == "NA", taxon] %>% unique %>% sort
# data3[is.na(species)] %>% unique(by = "taxon")
# data3[genus == "NA"] %>% unique(by = "taxon")
# data3[is.na(genus)]
# data3[family == "NA"] %>% unique(by = "taxon")
# data3[is.na(family)]
# data3[order == "NA"] %>% unique(by = "taxon")
# data3[is.na(order)]

data3[species == "NA", species := NA]
data3[genus == "NA", genus  := NA]
data3[family == "NA", family := NA]
data3[order == "NA", order := NA]

source("~/01_Uni/03_GetReal/02_WP_02/Community Data/quality_check/lists_of_accepted_orders.R")
mzb.orders = unique(data3[, order])
index = mzb.orders %in% accepted.mzb
mzb.orders[!index] %>% sort

data4 = data3

saveRDS(data4, "03_FinalData/05_191216_final_taxon_join_MED_Nuria_Bonada.RDS")
data4 = readRDS("03_FinalData/05_191216_final_taxon_join_MED_Nuria_Bonada.RDS") 

# 04. Final touches -------------------------------------------------------

unique_sites = unique(data4$site)
unique_dates = unique(data4$season)[c(2,3,1,4)]


data4[, c("site_id", "date_id") := list(
        map_int(data4$site, ~ which(unique_sites == .x )),
        map_int(data4$season, ~ which(unique_dates == .x ))
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

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_MED_Nuria_Bonada")]

n.sample.data = data4[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data5 = data4[n.sample.data, on = "site_id2"]

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
        date,
        year,
        season,
        site_id,
        date_id,
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
        x.coord = y.coord,
        y.coord = x.coord,
        EPSG = 32630,
        data.set = "MED_Nuria_Bonada",
        n.samples
        
)]

final = st_as_sf(data6, coords = c("y.coord", "x.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, "03_FinalData/06_191216_MZB_MED_Nuria_Bonada.gpkg")   


        
        
      
