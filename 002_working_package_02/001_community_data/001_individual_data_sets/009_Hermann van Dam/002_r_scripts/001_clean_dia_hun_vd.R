### ---------------------------------- ### 
# --- Clean Diatom data from Ecosurv --- #
### ---------------------------------- ###

# date: 02.08.19

# In this script I clean the diatom data from Ecosurv

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
pacman::p_load(readxl,
               magrittr,
               sf,
               taxize,
               data.table,
               stringr,
               tidyr,
               dplyr,
               purrr,
               tmap,
               lubridate)

tmap_mode("view")

setwd(here::here("09_Hermann van Dam"))

dia = read_excel("01_OriginalData/2415PhybeRiversExport2.xls") %>% setDT
chem = fread("04_old/Database/01_Individual_Tables/tb_Data_Chemistry.csv")
chem2 = fread("04_old/Database/01_Individual_Tables/tb_Chemistry_parameters.csv")
chem_samp = fread("04_old/Database/01_Individual_Tables/tb_Sampling_Event_Chemistry.csv")
sites = fread("01_OriginalData/tb_Sampling_locations_all.csv")
date = fread("04_old/Database/01_Individual_Tables/tb_Sampling_Event_PhytoBenton.csv")
# 02. Data Cleaning -------------------------------------------------------

# shape chemical data for identifying pristine sites
chem1_2 = chem[, c(4:7) := NULL]
chem2_2 = chem2[, list(CHEM.param.azon = ChemParam.ID,
                       ChemParam.name)]

chem3 = left_join(chem1_2,
                  chem2_2,
                  by = "CHEM.param.azon") %>%
  setDT

chem4 = dcast.data.table(chem3, CHEMvent.azon ~ ChemParam.name, value.var = "CHEM.value")

chem_samp2 = chem_samp[, list(
  CHEMvent.azon = CHEM.Sampling.Event..ID,
  date = mdy_hms(CHEM.Sampling.date),
  site_id = CHEM.Sampling.site.azon,
  x.coord = CHEM.eovx,
  y.coord = CHEM.eovy,
  WB.code = CHEM.nO
)]

sites2 = sites[, list(site_id = Sampling.location.ID,
                      WB.code,
                      x.coord = EOVX,
                      y.coord = EOVY)]

site_w_chem = left_join(sites2,
                        chem_samp2,
                        by = c("WB.code"))
site_w_chem2 = left_join(site_w_chem,
                         chem4,
                         by = "CHEMvent.azon") %>%
  setDT

site_w_chem2[, c("x.coord.y", "y.coord.y") := NULL]


# gather table into long format
dia = dia[, PhyBENT_Taxoncode := NULL]
dia2 <-
  gather(dia, key = "Site", value = "Abundance",-PhyBENT_TaxonName) %>% setDT
data = dia2[Abundance != 0, list(taxon = PhyBENT_TaxonName, Site, Abundance)]


# add sampling date to site

date$PHYBENT.sampling.date <- mdy_hms(date$PHYBENT.sampling.date)
site_w_date = left_join(site_w_chem2,
                        date,
                        by = c("WB.code" = "PHYBENT.sampling.Number"))

site_w_date$WB.code = str_remove(site_w_date$WB.code, "_")


data2 = left_join(data, site_w_date, by = c("Site" = "WB.code")) %>% setDT

data2[, c("NO_three_qual", "P_qual", "NH_qual", "NO_two_qual") :=
        list(
          case_when(`NO3` < 10 ~ 0,
                    `NO3` > 10 ~ 1),
          case_when(`Total P` < 0.2 ~ 0,
                    `Total P` >= 0.2 ~ 1),
          case_when(`NH4` < 0.3 ~ 0,
                    `NH4` >= 0.3 ~ 1),
          case_when(`NO2` < 0.1 ~ 0,
                    `NO2` >= 0.1 ~ 1)
        )]

data2[, pre_pristine := NO_three_qual + P_qual + NH_qual + NO_two_qual]
data2[,pristine := ifelse(pre_pristine == 0, 1, 0)]


data2 = data2[,
              list(
                      taxon,
                      Site,
                      abundance = Abundance,
                      x.coord = x.coord.x,
                      y.coord = y.coord.x,
                      date = PHYBENT.sampling.date,
                      year = year(date),
                      season = case_when(month(date) %in% c(12,1,2) ~ "winter",
                                         month(date) %in% c(3,4,5) ~ "spring",
                                         month(date) %in% c(6,7,8) ~ "summer",
                                         month(date) %in% c(9,10,11) ~ "autumn"),
                      EPSG = 23700,
                      pristine
              )]

saveRDS(data2, "03_FinalData/01_191216_data_before_taxon_clean_dia_Ecosurv.RDS")
data2 = readRDS("03_FinalData/01_191216_data_before_taxon_clean_dia_Ecosurv.RDS")

# 03. Taxonomic cleaning  ----------------------------------------------------------

TU = sort(unique(data2$taxon))

classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191216_classification.object_dia_Ecosurv.RDS")
classification.object = readRDS("03_FinalData/02_191216_classification.object_dia_Ecosurv.RDS")

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
saveRDS(taxontable, "03_FinalData/03_191216_initial_taxon_clean_dia_Ecosurv.RDS")
taxontable = readRDS("03_FinalData/03_191216_initial_taxon_clean_dia_Ecosurv.RDS")

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort

saveRDS(taxontable, "03_FinalData/04_191216_post_correction_taxontable_dia_Ecosurv.RDS")
taxontable = readRDS("03_FinalData/04_191216_post_correction_taxontable_dia_Ecosurv.RDS")
# add taxonomical information to data 


data3 = left_join(data2, 
                  taxontable,
                  by = "taxon") %>% 
        setDT

data3[species == "NA", taxon] %>% unique %>% sort
data3[is.na(species)]
data3[genus == "NA", taxon] %>% unique %>% sort
data3[is.na(genus)]
data3[family == "NA"] %>% unique(by = "taxon")
data3[is.na(family)]
data3[order == "NA"] %>% unique(by = "taxon")
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

saveRDS(data4, "03_FinalData/05_191216_final_taxon_join_dia_dia_Ecosurv.RDS")
data4 = readRDS("03_FinalData/05_191216_final_taxon_join_dia_dia_Ecosurv.RDS")


# 04. Final touches -------------------------------------------------------

unique_sites = unique(data4$Site)
unique_dates = unique(data4$date) %>% sort

data4[, c("site_id", "date_id") := list(
        map_int(data4$Site, ~ which(unique_sites == .x )),
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

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_Ecosurv")]

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
        original_site_name = Site,
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
        EPSG,
        data.set = "dia_Ecosurv",
        n.samples
        
)]

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, "03_FinalData/06_191216_DIA_Ecosurv.gpkg")   
