# -------------------------------------- #
# --- Clean Edwin Peters Diatom data --- # 
# -------------------------------------- #

# Jonathan Jupke 
# date: 26.09.19
# European Map of Diatoms 

# In this script I create a harmonized spatial data set from the raw data
# provided by Edwin Peters.

# EPSG based on projfinder: 28992

## -- OVERVIEW -- ## 
# 01. Setup
# 02. Clean data
# 03. Taxonomy
## -------------- ##

# 01. Setup -------------------------------------------------------------------

pacman::p_load(
  taxize,
  data.table,
  sf,
  dplyr,
  stringr,
  lubridate,
  readr,
  purrr,
  lubridate
)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/019_Edwin Peters/")
data = fread("01_OriginalData/Diatoms/Diatoms_count.csv")

# 02. Clean data ----------------------------------------------------------

# drop columns 
data[, c("data", "location,waterboard", "watertype") := NULL]

# fix date and add season 
data[,date := dmy_hms(Location)]
data[,c("year", "season") := list(year(date),
                                  case_when(month(date) %in% c(12,1,2) ~ "winter",
                                            month(date) %in% c(3,4,5) ~ "spring",
                                            month(date) %in% c(6,7,8) ~ "summer",
                                            month(date) %in% c(9,10,11) ~ "autumn")
                                  )]

# year 1972 to 2012 
unique(data$year) %>% sort

saveRDS(data, 
        "03_FinalData/01_200313_data_before_taxon_clean_dia_Edwin_Peters.RDS")
data2 = readRDS("03_FinalData/01_200313_data_before_taxon_clean_dia_Edwin_Peters.RDS")

# 03. Taxonomy ----------------------------------------------------------------
data2 = data
TU = unique(data2$species_name)
#classification.object = classification(TU, db = "gbif")
#saveRDS(classification.object, "19_Edwin Peters/03_FinalData/classification.object_NL_EP_DIA.RDS")
classification.object = readRDS("19_Edwin Peters/03_FinalData/classification.object_NL_EP_DIA.RDS")

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
saveRDS(taxontable, "03_FinalData/191008_GER_LD_DIA_QS.RDS")



# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort

# remove special entries 
taxontable = taxontable[
        !(taxon %in% c("Bacillariophycidae",
                       "Coscinodiscophyceae",
                       "Fragilariophyceae",
                       "Khakista"))
        ]
saveRDS(taxontable, "03_FinalData/03_191209_initial_taxon_clean_dia_Edwin Peters.RDS")
taxontable = readRDS("03_FinalData/03_FinalData/03_191209_initial_taxon_clean_dia_Edwin Peters.RDS")


# add taxonomical information to data 

data2 = left_join(data,
                  taxontable,
                  by = c("species_name" = "taxon")) %>% setDT

data2[, taxon := species_name]


# cannot find: Anomoeoneis neoexillis; Opephora gedanensis
# I assume Fragilaria pseudoconstructa to be Fragilaria pseudoconstruens which is a synonym of Staurosira pseudoconstruens
data2[species == "NA", taxon] %>% unique %>% sort
data2[genus == "NA", taxon] %>% unique %>% sort
data2[family == "NA"] %>% unique(by = "taxon")
data2[order == "NA", list(taxon,genus, family, order)] %>% unique(by = "taxon")  

data2[species == "NA", species := NA]
data2[genus == "NA", genus := NA]
data2[family == "NA", family := NA]
data2[order == "NA", order := NA]

data3 = data2[!(taxon %in% c("Bacillariophyceae"))]

# check against and update accepted names 
dia.orders = unique(data3[, order])
index = dia.orders %in% accepted.diatoms
dia.orders[!index] %>% sort

saveRDS(data3, "03_FinalData/05_191209_final_taxon_join_dia_edwin_peters.RDS")
data3 = readRDS("03_FinalData/05_191129_final_taxon_join_mzb_edwin_peters.RDS")


# 04. Final Touches --------------------------------------------------------

# assign gr_site_id

# I need to go beyond the loooop 

unique_sites = unique(data3$V1)
unique_dates = unique(data3$date)

data3[, c("site_id", "date_id") := list(
  map_int(data3$V1, ~ which(unique_sites == .x )),
  map_int(data3$date, ~ which(unique_dates == .x ))
)]


data3[, site_id2 := case_when(
  nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
  nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
  nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
  nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
  nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data3[, date_id2 := case_when(
  nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
  nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
  nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
  nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
  nchar(trunc(date_id)) == 5 ~ paste0(date_id))]

data3[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_DIA_edwin_peters")]

test = data3[1:1000,]
n.sample.data = data3[, .(n.samples = length(unique(date_id2))), .(site_id2)]
data4 = data3[n.sample.data, on = "site_id2"]

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
  
  print(i)        
};beepr::beep()


# check columns with NA 
for (i in seq_along(colnames(data4))) {
  x = pull(data4[,.SD,.SDcols = i])
  y = sum(is.na(x))        
  if (y > 0) print(names(data4)[i])
  
}

data5 = data4[, list(
  gr_sample_id,
  original_site_name = V1,
  date,
  year,
  season,
  site_id,
  date_id,
  species,
  genus,
  family,
  order,
  abundance = counts,
  pristine = NA,
  n.species,
  n.genus,
  n.family,
  n.order,
  x.coord = xcoord,
  y.coord = ycoord,
  EPSG = 28992,
  data.set = "ediwn_peters_dia",
  n.samples
  
)]

is.na(data5$y.coord) %>% sum
is.na(data5$x.coord) %>%  sum

data5[is.na(x.coord), unique(gr_sample_id)] == data5[is.na(y.coord), unique(gr_sample_id)] 
na.coord = data5[is.na(y.coord), unique(gr_sample_id)] 

data5 = data5[!(gr_sample_id %in% na.coord)]


final = st_as_sf(data5, coords = c("x.coord", "y.coord"), crs = data5$EPSG[1])

test1 = final %>% setDT

test1 = unique(test1, by = "site_id")

test1 = st_as_sf(test1, crs = test1$EPSG[1])

library(tmap)
tmap_mode("view")
tm_shape(test1) + tm_dots()

st_write(final, "03_FinalData/06_191209_DIA_ediwn_peters.gpkg")   
