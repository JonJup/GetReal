### ---------------------------------- ### 
# --- Clean Diatoms Sandin & Kahlert --- #
### ---------------------------------- ### 

## 21.08.19
## GR WP2 
## Here I clean and harmonize the diatom data provided by Maria Kahlert and Leonard Sandin

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Clean Data 
# 03. Taxonomic Cleaning 
# 04. Final Touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
pacman::p_load(here, dplyr, taxize, magrittr, sf, stringr, data.table, lubridate)
setwd(here::here("14_Sandin_Kahlert/"))
samples = readxl::read_excel("01_OriginalData/AQEMBilaga%205 Excel.xls") %>% setDT
sites = readxl::read_excel("01_OriginalData/AQEM_diatoms_coordinates.xlsx") %>% setDT


# 02. Clean Data -----------------------------------------------------
# gather data to long format 
samples2 = samples %>% tidyr::gather(key = "Site", value = "Abundance", -c(1:3)) %>% setDT
# remove zero abundance observations
samples3 = samples2[!(is.na(Abundance))]                                     
data = samples3[, list(
        taxon = ...1,
        taxon_work = ...1,
        Site,
        Abundance
        
)]

data2 = data

saveRDS(data2, "03_FinalData/01_191211_data_before_taxon_clean_dia_Leonard_Sandin.RDS")
data2 = readRDS("03_FinalData/01_191211_data_before_taxon_clean_dia_Leonard_Sandin.RDS")

# 03. Taxonomic cleaning  ---------------------------------------------------------

TU = unique(data2$taxon_work) %>% 
        sort

classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191211_classification.object_dia_Leonard_Sandin.RDS")
classification.object = readRDS("03_FinalData/02_191211_classification.object_dia_Leonard_Sandin.RDS")

taxontable = data.table(
  taxon = TU,
  species = "NA",
  genus = "NA",
  family = "NA",
  order = "NA",
  clean = F
)


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

saveRDS(taxontable, "03_FinalData/03_191211_initial_taxon_clean_dia_Leonard_Sandin.RDS")
taxontable = readRDS("03_FinalData/03_191211_initial_taxon_clean_dia_Leonard_Sandin.RDS")


# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort

saveRDS(taxontable, "03_FinalData/04_191211_post_correction_taxontable_dia_Leonard_Sandin.RDS")
taxontable = readRDS("03_FinalData/04_191211_post_correction_taxontable_dia_Leonard_Sandin.RDS")

## -- join tables -- ## 
data3 = left_join(data2,
          taxontable) %>% setDT


data3 = data3[!(taxon %in% c("SUMMA RÄKNADE SKAL", 
                             "SUMMA EUNOTIA", 
                             "SUMMA EUNOTIA %", 
                             "SUMMA EUNOTIA-E. FORMICA %")
                )]

# data3[species == "NA", taxon] %>% unique %>% sort
# data3[genus == "NA", taxon] %>% unique %>% sort
# data3[family == "NA"] %>% unique(by = "taxon")
# data3[order == "NA"] %>% unique(by = "taxon")

data3[species == "NA", species := NA]
data3[genus == "NA", genus  := NA]
data3[family == "NA", family := NA]
data3[order == "NA", order := NA]

source("~/01_Uni/03_GetReal/02_WP_02/Community Data/quality_check/lists_of_accepted_orders.R")
dia.orders = unique(data3[, order])
index = dia.orders %in% accepted.diatoms
dia.orders[!index] %>% sort

data4 = data3

saveRDS(data4, "03_FinalData/05_191211_final_taxon_join_dia_dia_Leonard_Sandin.RDS")
data4 = readRDS("03_FinalData/05_191211_final_taxon_join_dia_dia_Leonard_Sandin.RDS")

# 04. Final Touches --------------------------------------------------------



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


data4[,gr_sample_id := paste0("site_", site_id2, "_date_", "XXX","_dia_Leonard_Sandin")]

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
        x = pull(data4[,.SD,.SDcols = i])
        y = sum(is.na(x))        
        if (y > 0) print(names(data5)[i])
        
}

data5 = data5[sites, on = "Site"]
names(data5)[18:19] <- c("y.coord", "x.coord")
data6 = data5[, list(
        gr_sample_id,
        original_site_name = Site,
        date = NA,
        year = 2000,
        season = NA,
        site_id,
        date_id = NA,
        species,
        genus,
        family,
        order,
        abundance = Abundance,
        pristine = NA,
        n.species,
        n.genus,
        n.family,
        n.order,
        x.coord,
        y.coord,
        EPSG = 2400,
        data.set = "dia_Leonard_Sandin",
        n.samples = 1
        
)]

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "n.order")

st_write(final, "03_FinalData/06_191211_dia_Leonard_Sandin.gpkg")   












data4 = data3[, list(site = Site, 
                     order,
                     family,
                     genus,
                     species)]

# Add Site data and make spatial ------------------------------------------

data5 = data4[sites, on = c("site" = "Site")]
names(data5) <- c("site_code", "order", "family", "genus", "species", 
                  "site", "y.coord", "x.coord", "a", "b", "c", "d", "e")
data6 = data5[,list(site_code, site, x.coord = as.numeric(x.coord), 
                    y.coord = as.numeric(y.coord), order, family, genus, species)]
data7  = unique(data6, by = "site" )
data.sp = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = 2400)

st_write(data.sp, "dia_sk.gpkg")


## --  Filter down to sites within 1km of CCM2 Streams -- ## 
# read in stream geofile 
river = st_read("../../../01_WP_01/01_Stream_Network/01_CCM2/02_GPKG/Riversegments/2019-05-29_allGRcountires_rivers.gpkg")
# transform spatial data to crs of rivers layer 
data.sp2 = st_transform(data.sp, crs = st_crs(river))
# reduce to unique sites and transform 
data.u.sp = st_as_sf(data7, coords = c("x.coord", "y.coord"), crs = 2400)
data.u.sp2 = st_transform(data.u.sp, crs = st_crs(river))
# which streams (rows of river) are within 1km of the sampling point 
distance = st_is_within_distance(data.u.sp2, river, dist = 1000)
# this function finds the nearest element of rivers to each element of
# data.u.sp2 (sampling points). This is used to decide for points where more
# than one stream are close to the samling point.
nn = st_nearest_feature(data.u.sp2,river)


data.u.sp2$WSO1_ID = NA
for (i in 1:length(distance)) {
  
  if (length(distance[[i]]) == 1) {
    data.u.sp2$WSO1_ID[i] = distance[[i]]
  } 
  if (length(distance[[i]]) == 0) {
    data.u.sp2$WSO1_ID[i] = 0
  }
  if (length(distance[[i]]) > 1) {
    data.u.sp2$WSO1_ID[i] = river$WSO1_ID[nn[i]]
  }
  
}
# join WSO1ID to sites 
data.join = left_join(data.sp2,
          data.u.sp2 %>% select("site_code", "WSO1_ID") %>% st_drop_geometry
          )
# remove sites that are to far from streams 
data.join %<>% filter(WSO1_ID != 0)
# safe file 
st_write(data.join, "test2.gpkg")
