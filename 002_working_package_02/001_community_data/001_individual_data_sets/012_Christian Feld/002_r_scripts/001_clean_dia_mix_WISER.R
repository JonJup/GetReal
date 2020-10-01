#########################################
### --- Clean DIA data from WISER --- ### 
#########################################

# 09.10.19

# GR WP 02 
# EPSG = 4326

# Setup -------------------------------------------------------------------
pacman::p_load(dplyr, readxl, stringr, readr, data.table, taxize, sf, tmap, purrr)
tmap_mode("view")
setwd(here::here("12_Christian Feld"))

# biological data 
bio = read_excel("01_OriginalData/WISER_Diatom_taxa.xlsx") %>% 
  setDT
sites = read_excel("01_OriginalData/WISER_Metadata_Abiotics.xls") %>% 
  setDT

# Initial data cleaning ---------------------------------------------------

bio = bio[,list("site_id" = StationCode, "Taxon" = TaxonName)] 
  
sites = sites[,list(StationCode, "X.Coord" = Longitude, "Y.Coord" = Latitude, 
                    "site_name" = StationName, "stream" = RiverName)]

data = bio[sites, on = c("site_id" = "StationCode")]
data = data[!(is.na(Taxon))]

saveRDS(data, "03_FinalData/01_191212_data_before_taxon_clean_dia_WISER.RDS")
data = readRDS("03_FinalData/01_191221_data_before_taxon_clean_dia_WISER.RDS")

# taxonomical data --------------------------------------------------------

TU = sort(unique(data$Taxon))
classification.object = classification(TU, db = "gbif")


saveRDS(classification.object, "03_FinalData/02_191212_classification.object_dia_WISER.RDS")
classification.object = readRDS("03_FinalData/02_191212_classification.object_dia_WISER.RDS")

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
saveRDS(taxontable, "03_FinalData/03_191212_initial_taxon_clean_dia_WISER.RDS")
taxontable = readRDS("03_FinalData/03_191212_initial_taxon_clean_dia_WISER.RDS")

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort

saveRDS(taxontable, "03_FinalData/04_191212_post_correction_taxontable_dia_WISER.RDS")
taxontable = readRDS("03_FinalData/04_191212_post_correction_taxontable_dia_WISER.RDS")

# add taxonomical information to data 
data2 = left_join(data, 
                  taxontable,
                  by = c("Taxon" = "taxon")) %>% 
  rename(taxon = Taxon) %>% 
  setDT


# data2[species == "NA", taxon] %>% unique %>% sort
# data2[genus == "NA", taxon] %>% unique %>% sort
# data2[family == "NA"] %>% unique(by = "taxon")
# data2[order == "NA"] %>% unique(by = "taxon")

data2[species == "NA", species := NA]
data2[genus == "NA", genus  := NA]
data2[family == "NA", family := NA]
data2[order == "NA", order := NA]

source("~/01_Uni/03_GetReal/02_WP_02/Community Data/quality_check/lists_of_accepted_orders.R")
mzb.orders = unique(data2[, order])
index = mzb.orders %in% accepted.diatoms
mzb.orders[!index] %>% sort

data4 = data2

saveRDS(data4, "03_FinalData/05_191212_final_taxon_join_dia_dia_WISER.RDS")
data4 = readRDS("03_FinalData/05_191212_final_taxon_join_dia_dia_WISER.RDS")



# 04. Final Touches --------------------------------------------------------



unique_sites = unique(data4$site_id)

data4[, c("site_id") := list(
  map_int(data4$site_id, ~ which(unique_sites == .x ))
)]


data4[, site_id2 := case_when(
  nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
  nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
  nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
  nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
  nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", "XXX","_dia_WISER")]

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

for (i in seq_along(colnames(data5))) {
  x = pull(data5[,.SD,.SDcols = i])
  y = sum(is.na(x))        
  if (y > 0) print(names(data5)[i])
  
}


data6 = data5[, list(
  gr_sample_id,
  original_site_name = site_name,
  date = NA,
  year = NA,
  season = NA,
  site_id,
  date_id = NA,
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
  x.coord = X.Coord,
  y.coord = Y.Coord,
  EPSG = 4258,
  data.set = "dia_Wiser",
  n.samples = 1
  
)]

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "n.order")

st_write(final, "03_FinalData/06_191212_dia_WISER.gpkg")   

