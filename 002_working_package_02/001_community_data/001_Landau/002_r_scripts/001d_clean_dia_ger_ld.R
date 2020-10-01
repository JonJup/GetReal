### --------------------------------- ###
# --- Clean Diatom data from Landau --- # 
### --------------------------------- ###

# 08.10.19

# In this script I clean the diatom data from landau.

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------

pacman::p_load(here, dplyr, taxize, magrittr, sf, stringr, data.table, lubridate, purrr, tmap)
tmap_mode("view")
# also required: here 
setwd(here::here("01_Landau/"))

samples = fread("01_OriginalData/diatom_samples.csv")
sites = fread("01_OriginalData/diatom_sites.csv") 

# 02. Data Cleaning -----------------------------------------------------

#remove rows with zero abundance 
samples = samples[iz_n != 0]

## drop columns  
samples2 = samples[, c("date", "taxon", "site_id", "iz_n") ]

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

# fix date column
data[,c("date","year", "month") := list(ymd(date), year(date), month(date))]
data[,"season" := ifelse(month %in% c(12,1,2), "winter", ifelse(month %in% c(3,4,5), "spring", ifelse(month %in% c(6,7,8), "summer", "autumn")))]
data2 = data

saveRDS(data, "03_FinalData/01_191217_data_before_taxon_clean_dia_Landau.RDS")
data = readRDS("03_FinalData/01_191217_data_before_taxon_clean_dia_Landau.RDS")

# 03. Taxonomic cleaning ----------------------------------------------------------

TU = sort(unique(data2$taxon))
classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191217_classification.object_dia_Landau.RDS")
classification.object = readRDS("03_FinalData/02_191216_classification.object_dia_Landau.RDS")

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
saveRDS(taxontable, "03_FinalData/03_191217_initial_taxon_clean_dia_Landau.RDS")
taxontable = readRDS("03_FinalData/03_191217_initial_taxon_clean_dia_Landau.RDS")


# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort


saveRDS(taxontable, "03_FinalData/04_191217_post_correction_taxontable_dia_Landau.RDS")
taxontable = readRDS("03_FinalData/04_191217_post_correction_taxontable_dia_Landau.RDS")

# add taxonomical information to data 
data3 <- left_join(data2, 
                   taxontable,
                   by = "taxon") %>%  
        setDT

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
data3[genus == "NA", genus  := NA]
data3[family == "NA", family := NA]
data3[order == "NA", order := NA]

source("~/01_Uni/03_GetReal/02_WP_02/Community Data/quality_check/lists_of_accepted_orders.R")
mzb.orders = unique(data3[, order])
index = mzb.orders %in% accepted.diatoms
mzb.orders[!index] %>% sort

data4 = data3

saveRDS(data4, "03_FinalData/05_191217_final_taxon_join_dia_Landau.RDS")
data4 = readRDS("03_FinalData/05_191217_final_taxon_join_dia_Landau.RDS")

# 04. Final touches -------------------------------------------------------

data4[, original_site_name := site_id]

unique_sites = unique(data4$site_id)
unique_dates = unique(data4$date) %>% sort

data4[, c("site_id", "date_id") := list(
        map_int(data4$site_id, ~ which(unique_sites == .x )),
        map_int(data4$date, ~ which(unique_dates == .x ))
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

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_Landau")]

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

for (i in seq_along(colnames(data5))) {
        x = pull(data5[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data5)[i])
        
}


data6 = data5[, list(
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
        abundance = iz_n,
        pristine = NA,
        n.species,
        n.genus,
        n.family,
        n.order,
        x.coord = X,
        y.coord = Y,
        EPSG,
        data.set = "Dia_LD",
        n.samples
        
)]

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "n.order")

st_write(final, "03_FinalData/06_200305_DIA_Landau.gpkg")   



# subset to close to river sites ------------------------------------------




# Land use  ---------------------------------------------------------------
final <- st_read("03_FinalData/06_200305_DIA_Landau.gpkg")



landcover <- readRDS("../../../001_WP_01/003_corine_land_cover/03_data_processed/reduced_clc.RDS")
