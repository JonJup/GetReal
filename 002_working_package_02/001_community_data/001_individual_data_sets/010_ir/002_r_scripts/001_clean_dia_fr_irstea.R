### -------------------------------------------------------------------- ###
# --- Clean Juliette Tison-Rosebery and Aurelien Jamoneau Diatom data  --- # 
### -------------------------------------------------------------------- ###

# Jonathan Jupke 
# date: 20.11.19

# In this script I create a harmonized spatial data set from the raw data
# provided by Juliette Tison-Rosebery and Aurelien Jamoneau Diatom @ irstea.

# EPSG based on email: Lambert-93 transformation projected system (EPSG2154)

## -- OVERVIEW -- ## 
# 01. Setup
# 02. Clean data
# 03. Taxonomic Cleaning
# 04. Final Touches
## -------------- ##

# 01. Setup -------------------------------------------------------------------

pacman::p_load(
        taxize,
        data.table,
        sf,
        dplyr,
        stringr,
        tmap,
        purrr
)
# other required packages: lubridate
tmap_mode("view")
setwd(here::here("10_Irstea"))

load("01_OriginalData/French_data_for_Jupke.RData")

setDT(comm)
setDT(env)
setDT(taxa)
taxa$taxon = as.character(taxa$taxon)

# 02. Clean data --------------------------------------------------------

# Ok I will start from the env matrix. Comm and env both do not have site names.
# Instead each row is one site.
comm[, site := .I]
env[,site := .I]

comm2 = data.table::melt(comm, id.var = "site")
comm2 = comm2[value != 0]

com.env = left_join(comm2, env, by = "site") %>% setDT
com.env$variable = as.character(com.env$variable)

# identify pristine variables 
# load data from PUP 
source("../16_PhillipeUsseglioPolterra/02_Rscripts/derive_thresholds.R")

com.env$NO3 %>% summary
com.env[,NO_qual := case_when(NO3 < 10 ~ 0,
                              NO3 > 10 ~ 1)]
com.env$Orthophosp %>% summary
com.env[,P_qual := case_when(Orthophosp < 0.2 ~ 0,
                              Orthophosp >= 0.2 ~ 1)]


com.env[,pre_pristine := P_qual + NO_qual]
com.env[, pristine := ifelse(pre_pristine == 0, 1, 0)]


data = left_join(com.env, taxa, by = c("variable" = "taxon")) %>% setDT

data[, season := ifelse(month(date_opecont) %in% c(12,1,2), "winter",
                        ifelse(month(date_opecont) %in% c(3,4,5), "spring",
                        ifelse(month(date_opecont) %in% c(6,7,8), "summer", "autumn")))]

data2 = data[,list(
        site = site,
        year = year(date_opecont),
        season,
        taxon = str_trim(as.character(Nom_complet)),
        x.coord = X,
        y.coord = Y,
        EPSG = 2154,
        abundance = value,
        pristine
)]


saveRDS(data2, "03_FinalData/01_191212_data_before_taxon_clean_irstea.RDS")
data2 = readRDS("03_FinalData/01_191211_data_before_taxon_clean_irstea.RDS")
# 03. Taxonomic Cleaning --------------------------------------------------

TU = sort(unique(data2$taxon))
classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_191211_classification.object_irstea.RDS")
classification.object = readRDS("03_FinalData/02_191211_classification.object_irstea.RDS")

taxontable = data.table(
        taxon = TU,
        species = "NA",
        genus = "NA",
        family = "NA",
        order = "NA",
        clean = F
)
#  Open the file up and run it manually. 
clean_dia_synonyms.R

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
                                               tax[[1]][which(tax[[1]][, 2] == "family"), 1],  
                                               paste(tail(tax[[1]][1][[1]], n = 1), 
                                                     "is a",
                                                     tail(tax[[1]][2][[1]], n = 1)))
                                               
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

# quick save and WISERting point for later sessions
saveRDS(taxontable, "03_FinalData/03_191211_initial_taxon_clean_irstea.RDS")
taxontable = readRDS("03_FinalData/03_191211_initial_taxon_clean_irstea.RDS")

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort
taxontable$taxon = str_trim(taxontable$taxon)


saveRDS(taxontable, "03_FinalData/04_191211_post_correction_taxontable_irstea.RDS")
taxontable = readRDS("03_FinalData/04_191211_post_correction_taxontable_irstea.RDS")

data3 = left_join(data2,
                  taxontable, 
                  by = "taxon") %>% 
        setDT
data3 = data3[!(taxon %in% c("Centric Diatoms Diatomées centriques indifférenciées",
                             "Centriques indifférenciées in TDI3 Kelly", 
                             "Cyclotella wuethrichiana Druart & Straub", 
                             "Diatomée anormale Abnormal diatom valve (unidentified) or sum of deformities abundance",
                             "Diatomées non identifiées vue connectives"))]

# data3[species == "NA", taxon] %>% unique %>% sort
# data3[is.na(species)] %>% unique %>% sort
# data3[genus == "NA", taxon] %>% unique %>% sort
# data3[is.na(genus)] %>% unique %>% sort
# data3[family == "NA"] %>% unique(by = "taxon")
# data[is.na(family)] %>% unique(by = "taxon")
# data3[order == "NA", list(taxon,genus, family, order)] %>% unique(by = "taxon")  
# data3[is.na(order), list(taxon,genus, family, order)] %>% unique(by = "taxon")  

data3[species == "NA", species := NA]
data3[genus == "NA", genus  := NA]
data3[family == "NA", family := NA]
data3[order == "NA", order := NA]

source("~/01_Uni/03_GetReal/02_WP_02/Community Data/quality_check/lists_of_accepted_orders.R")
mzb.orders = unique(data3[, order])
index = mzb.orders %in% accepted.diatoms
mzb.orders[!index] %>% sort

data4 = data3

saveRDS(data4, "03_FinalData/05_191212_final_taxon_join_dia_irstea.RDS")
data4 = readRDS("03_FinalData/05_191212_final_taxon_join_dia_irstea.RDS")

# 04. Final Touches --------------------------------------------------------

unique_sites = unique(data4$site)
unique_dates = unique(data4$season) #%>% sort

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

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_IRSTEA")]

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
        date = NA,
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
        x.coord ,
        y.coord,
        EPSG = 2154,
        data.set = "IRSTEA",
        n.samples
        
)]

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
test1 = final %>% setDT
test1 = unique(test1, by = "site_id")
test1 = st_as_sf(test1, crs = 4326)
tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, "03_FinalData/06_191212_DIA_IRSTEA.gpkg")   

