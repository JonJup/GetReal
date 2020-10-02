### ---------------------------------- ### 
# --- Clean Diatom data from Naides --- #
### --------------------------------- ###

# date: 16.09.19 

# In this script I clean the diatom data from Naiades

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###

# 01. Setup -------------------------------------------------------------------
pacman::p_load(here, dplyr, taxize, magrittr, sf, stringr, data.table, lubridate, ggplot2, purrr, tmap)
tmap_mode("view")
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/004_Naiades")

fauna_table <- fread("01_OriginalData/ListesFauneFlore.CSV")
geo_table = fread("01_OriginalData/Stations.CSV")
resbio = fread("01_OriginalData/ResultatsBiologiques.CSV")

# 02. Data Cleaning ----------------------------------------------------------

# subset fauna table to diatoms.
fauna_table = fauna_table[LbSupport == unique(fauna_table$LbSupport)[1]]

join1 = left_join(fauna_table,
                  geo_table,
                  by = "CdStationMesureEauxSurface") %>% 
setDT

# the resultats biologiques table contains the biological diatomic index. Which
# I use here to sperate between reference and non-reference sites. It goes from
# 0 to 20, the higher the index, the better the water quality. Everything form
# 14,5 is considered good quality. For more see here: 
#http://www.driee.ile-de-france.developpement-durable.gouv.fr/l-indice-biologique-diatomees-ibd-r1092.html
names(resbio)
index = resbio[[10]] == "IndiceBioDiat"
# subset to IBD rows 
resbio = resbio[index]
# turn into numeric
resbio$ResIndiceResultatBiologique %<>% 
  str_replace_all(pattern = ",",
                  replacement = ".") %>% 
        as.numeric()
# subset an rename
resbio = resbio[, list( 
        IBD = ResIndiceResultatBiologique,
        CdStationMesureEauxSurface)]

# use mean for stations with multiple measurements 
resbio[, mean_IDB := mean(IBD), by = list(CdStationMesureEauxSurface)]

join2 = left_join(join1, 
                  resbio) %>% 
  setDT

unique(join2$mean_IDB) %>%
  is.na %>%
  sum

join2[is.na(mean_IDB)]
join2[,pristine := ifelse(mean_IDB >= 14.5, 1, ifelse(is.na(mean_IDB), NA, 0))]

data = join2[,
                  list(
                          "ID" = CdStationMesureEauxSurface,
                          pristine,
                          "date" = ymd(DateDebutOperationPrelBio),
                          "site" = LbStationMesureEauxSurface.x,
                          "taxon" =  NomLatinAppelTaxon,
                          "x.coord" =  CoordXStationMesureEauxSurface,
                          "y.coord" =  CoordYStationMesureEauxSurface,
                          "EPSG" = 2154
                  )
             ]

data[,c("year", "season") := .(year(date), case_when(month(date) %in% c(12,1,2) ~ "winter",
                                                     month(date) %in% c(3,4,5) ~ "spring",
                                                     month(date) %in% c(6,7,8) ~ "summer",
                                                     month(date) %in% c(9,10,11) ~ "autumn"))]

data2 = data
data2 = unique(data2, by = c("site", "date", "taxon"))
saveRDS(data2, 
        "03_FinalData/01_200310_data_before_taxon_clean_dia_Naiades.RDS")
data2 = readRDS("03_FinalData/01_200310_data_before_taxon_clean_dia_Naiades.RDS")

# 03. Taxonomic cleaning ----------------------------------------------------------------

data2 = data2[!taxon %in% c(
        "Appellation de Taxon inconnue",
        "Abnormal diatom",
        "Abnormal diatom valve",
        "Centric Diatoms",
        "Pennate"
)]

TU = unique(data2$taxon) %>% sort

classification.object = classification(TU, db = "gbif")

saveRDS(classification.object, "03_FinalData/02_200312_classification.object_dia_Naiades.RDS")
classification.object = readRDS("03_FinalData/02_200312_classification.object_dia_Naiades.RDS")

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
response = rep("0", nrow(taxontable))

for (i in 1:(nrow(taxontable) + 1)) {
        
        # skip if clean
        if (taxontable$clean[i] == TRUE) {
                print(paste(i))
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
        } else if ("species" %in% tax[[1]]$rank) {
                if (str_detect(names(tax), tax[[1]][which(tax[[1]]$rank == "species"),1])) {
                        response[i] = "y"
                        print("checking species")
                }
        } else if ("genus"  %in% tax[[1]]$rank) {
                if (str_detect(names(tax), tax[[1]][which(tax[[1]]$rank == "genus"),1])) {
                        response[i] = "y"
                        print("checking genus")
                }
        } else if ("family" %in% tax[[1]]$rank) {
                if (str_detect(names(tax), tax[[1]][which(tax[[1]]$rank == "family"),1])) {
                        response[i] = "y"
                        print("checking family")
                }
        } 
        if (response[i] == "0") {
                print("Checked nothing")
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
        print(paste(i))
}

group_id <- str_which(taxontable$taxon, "Group")

taxontable <- taxontable[taxon != taxontable$taxon[group_id]]
taxontable <- taxontable[taxon != "Pennate"]

# quick save and starting point for later sessions
saveRDS(taxontable, "03_FinalData/03_200312_initial_taxon_clean_dia_Naiades.RDS")
taxontable = readRDS("03_FinalData/03_200312_initial_taxon_clean_dia_Naiades.RDS")

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon] %>% sort

# remove special entries 
saveRDS(taxontable, "03_FinalData/04_200312_post_correction_taxontable_dia_Naiades.RDS")
taxontable = readRDS("03_FinalData/04_200312_post_correction_taxontable_dia_Naiades.RDS")

# add taxonomical information to data 
data3 = left_join(data2, 
                  taxontable,
                  by = "taxon") %>% 
  setDT

data3 = taxontable[data2, 
                   on = "taxon"]

data3[is.na(species), unique(taxon)]


data3 = data3[
  !(taxon %in% c("Groupe Diatomées centriques indéterminées"))
  ]


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
  
  source("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/lists_of_accepted_orders.R")
  mzb.orders = unique(data3[, order])
  index = mzb.orders %in% accepted.diatoms
  mzb.orders[!index] %>% sort
  
  data4 = data3
  data4 = data4[!taxon %in% "Pennate"]
  data4 = data4[!taxon %in% " Pennate"]
  saveRDS(data4, "03_FinalData/05_200312_final_taxon_join_dia_dia_Naiades.RDS")
  data4 = readRDS("03_FinalData/05_200312_final_taxon_join_dia_dia_Naiades.RDS")

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
  nchar(trunc(site_id)) == 5 ~ paste0(site_id))];beepr::beep()

data4[, date_id2 := case_when(
  nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
  nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
  nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
  nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
  nchar(trunc(date_id)) == 5 ~ paste0(date_id))];beepr::beep()

data4[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_Naiades")]

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

data5 = data5[!is.na(x.coord)]

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
  pristine,
  n.species,
  n.genus,
  n.family,
  n.order,
  x.coord,
  y.coord,
  EPSG,
  data.set = "dia_Naiades",
  n.samples
  
)]

final = st_as_sf(data6, coords = c("x.coord", "y.coord"), crs = data6$EPSG[1])
final = st_transform(final, crs = 4326)
# test1 = final %>% setDT
# test1 = unique(test1, by = "site_id")
# test1 = st_as_sf(test1, crs = 4326)
# tm_shape(test1) + tm_dots(col = "pristine")

st_write(final, "03_FinalData/06_200312_DIA_Naiades.gpkg")   
saveRDS(final, "03_FinalData/06_200312_DIA_Naiades.RDS")
