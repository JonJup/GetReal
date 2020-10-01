### ------------------------------------------ ### 
# --- Clean Diatom data from Miguel Iglesias --- #
### ------------------------------------------ ###

# date: 16.09.19 

# In this script I clean the diatom data from Miguel Iglesias
# EPSG: 32630 -- WGS 84 / UTM zone 30N 

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
pacman::p_load(here, 
               dplyr, 
               taxize, 
               magrittr, 
               sf, 
               stringr, 
               data.table, 
               lubridate, 
               tidyr,
               readxl, 
               tmap,
               purrr)
tmap_mode("view")

setwd(here::here("02_Miguel_Iglesias/"))

samples = read_excel("01_OriginalData/Diatomeas EBRO.xls") %>% 
        setDT

# 02. Data Cleaning -------------------------------------------------------

CEE = samples[Parámetro == "Indice CEE"]
IBD = samples[Parámetro == "Indice IBD"]
IPS = samples[Parámetro == "Indice IPS"]

CEE = CEE[, .SD, .SDcols = c(2,9,13)]
names(CEE) <- c("site", "date", "CEE")
CEE$date <- dmy(CEE$date)
CEE$CEE <- as.numeric(CEE$CEE)
CEE = CEE[, .(CEE = mean(CEE, na.rm = T)),  site]
IBD = IBD[, .SD, .SDcols = c(2,9,13)]
names(IBD) <- c("site", "date", "IBD")
IBD$date <- dmy(IBD$date)
IBD$IBD <- as.numeric(IBD$IBD)
IBD = IBD[, .(IBD = mean(IBD, na.rm = T)),  site]
IPS = IPS[, .SD, .SDcols = c(2,9,13)]
names(IPS) <- c("site", "date", "IPS")
IPS$date <- dmy(IPS$date)
IPS$IPS <- as.numeric(IPS$IPS)
IPS = IPS[, .(IPS = mean(IPS, na.rm = T)),  site]

pristine_data = left_join(IPS,
                          IBD,
                          by = "site") %>% 
        left_join(CEE,
                  by = "site") %>% 
        setDT

pristine_data[,c("CEE_q", "IBD_q", "IPS_q") := 
                      .(
                              ifelse(CEE >= 14.5, 1, 0),
                              ifelse(IBD >= 14.5, 1, 0),
                              ifelse(IPS >= 14.5, 1, 0)
                      )]

pristine_data[,qualiyscore := CEE_q + IBD_q + IPS_q]
pristine_data[,prisitne := ifelse(qualiyscore >= 2, 1,0) ]

pd = pristine_data[,.(site, prisitne)]
rm(pristine_data)

data = samples[,
               .(
                       date = dmy(Fecha),
                       year = year(dmy(Fecha)),
                       season = ifelse(month(dmy(Fecha)) %in% c(12,1,2), "winter", 
                                       ifelse(
                                               month(dmy(Fecha)) %in% c(3,4,5), "spring",
                                               ifelse(
                                                       month(dmy(Fecha)) %in% c(6,7,8), "summer", "autumn"
                                               )
                                       )
                       ) ,
                       site = .SD,
                       "taxon" = Taxa,
                       "x.coord" = XUTM30,
                       "y.yoord" = YUTM30,
                       "EPSG" = 32630
               ),
               .SDcols = 2]   
data = data[!(taxon == "--")]
names(data)[4] <- "site"


data2 = left_join(data, 
                  pd,
                  by = "site")


saveRDS(data2, "03_FinalData/01_191217_data_before_taxon_clean_dia_Miguel_Iglesias.RDS")
data2 = readRDS("03_FinalData/01_191217_data_before_taxon_clean_dia_Miguel_Iglesias.RDS")

# 03. Taxonomic cleaning ----------------------------------------------------------------

data2 = data

TU = unique(data2$taxon) %>% sort

classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191217_classification.object_dia_Miguel_Iglesias.RDS")
classification.object = readRDS("03_FinalData/02_191216_classification.object_dia_Miguel_Iglesias.RDS")

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
saveRDS(taxontable, "03_FinalData/03_191217_initial_taxon_clean_dia_Miguel_Iglesias.RDS")
taxontable = readRDS("03_FinalData/03_191217_initial_taxon_clean_dia_Miguel_Iglesias.RDS")

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon]

# save taxontable 
saveRDS(taxontable, "03_FinalData/04_191217_post_correction_taxontable_dia_Miguel_Iglesias.RDS")
taxontable = readRDS("03_FinalData/04_191217_post_correction_taxontable_dia_Miguel_Iglesias.RDS")


# add taxonomical information to data 
data3 = left_join(data2, 
                  taxontable,
                  by = "taxon") %>% 
        setDT



# data3[is.na(taxon)]
# data3[species == "NA", taxon] %>% unique %>% sort
# data3[is.na(species)]
# data3[genus == "NA", taxon] %>% unique %>% sort
# data3[is.na(genus)]
# data3[family == "NA"] %>% unique(by = "taxon")
# data3[is.na(family)]
# data3[order == "NA", list(taxon,genus, family, order)] %>% unique(by = "taxon")  
# data3[is.na(order)]

data3[species == "NA", species := NA]
data3[genus == "NA", genus  := NA]
data3[family == "NA", family := NA]
data3[order == "NA", order := NA]

source("~/01_Uni/03_GetReal/02_WP_02/Community Data/quality_check/lists_of_accepted_orders.R")
mzb.orders = unique(data3[, order])
index = mzb.orders %in% accepted.diatoms
mzb.orders[!index] %>% sort

data4 = data3

saveRDS(data4, "03_FinalData/05_191217_final_taxon_join_dia_Miguel_Iglesias.RDS")
data4 = readRDS("03_FinalData/05_191217_final_taxon_join_dia_Miguel_Iglesias.RDS")

# 04. Final touches -------------------------------------------------------

unique_sites = unique(data4$site)
unique_dates = unique(data4$date) %>% sort

data4[, c("site_id", "date_id") := list(
        map_int(data4$site, ~ which(unique_sites == .x )),
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

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_MiguelIglesias")]

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
        pristine = prisitne,
        n.species,
        n.genus,
        n.family,
        n.order,
        x.coord,
        y.coord = y.yoord,
        EPSG,
        data.set = "dia_Miguel_Iglesias",
        n.samples
        
)]

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, "03_FinalData/06_191217_DIA_Miguel_Iglesias.gpkg")   


