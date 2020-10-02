### ------------------------------------------------ ###
# ----- Clean Diatoms Jenny Jyrkänkallio-Mikkola ----- #
### ------------------------------------------------ ###

# 10.12.19
# Cleaning/ Homogenization of diatom data from Jenny Jyrkänkallio-Mikkola. 
# EPSG: 3067 -- ETRS89 / TM35FIN(E,N) -- Finland

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Cleaning Data 
# 03. Taxonomic Cleaning 
# 04. Final touches
### ---------------- ###


# 01. Setup -------------------------------------------------------------------
# libraries 
pacman::p_load(dplyr, 
               taxize, 
               magrittr, 
               sf, 
               stringr, 
               data.table, 
               lubridate,
               purrr,
               tmap,
               here)

# wd 
setwd(here("018_Jenny Jyrkaenkallio-Mikkola/"))


# some exploratory analysis 
# cor(sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`, sites$tot_N_µg_l)
# cor(sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`, sites$tot_N_µg_l)
# cor(sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`, sites$tot_N_µg_l)
# cor(sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`, sites$tot_N_µg_l)
# car::spm(sites[,c(7,8,10,14:20)])

# unique(sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`) %>% sort
# hist(sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`)
# plot(sites$tot_N_µg_l ~ sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`)
# plot(sites$`tot P µg/l` ~ sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`)
# plot(sites$conductivity ~ sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`)
# plot(sites$`Colour_ Pt_mg_l` ~ sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`)
# plot(sites$pH ~ sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`)
# plot(sites$temp ~ sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`)
# plot(sites$temp ~ sites$`Agricultural_ areas_ incl_artificical_ non_agricultural_ vegetated_ areas`)

# Cleaning ----------------------------------------------------------------

# gather data to long format 
samples2 = samples %>% tidyr::gather(key = "Taxa", value = "Abundance", -1) %>% setDT

# new taxon column withouth "_" 
samples2[,taxon := str_replace_all(Taxa, pattern = "_", replacement = "\\ ")] 
# remove zero abundance observations
samples3 = samples2[Abundance != 0]

# join with site data 
data = samples3[sites, on = c("Sampling_site" = "site")]
names(data)[10] = "tot_N"
names(data)[11] = "tot_P"
names(data)[30] = "agriculture"
data2 = data[,list(
        site = Sampling_site,
        taxon,
        date = ymd(date),
        year = ymd(date) %>% year(),
        season = ifelse(month(ymd(date)) %in% c(12,1,2), "winter", 
                        ifelse(month(ymd(date)) %in% c(3,4,5), "spring", 
                               ifelse(month(ymd(date)) %in% c(6,7,8), "summer", "autumn"))),
        "y.coord" = coor_n,
        "x.coord" = coor_e,
        conductivity,
        tot_N,
        tot_P,
        pH,
        abundance = Abundance,
        artificial = agriculture + Artificial_areas
)
]

#hist(data2$artificial)
saveRDS(data2, "03_FinalData/01_200313_data_before_taxon_clean_JJM.RDS")
data2 = readRDS("03_FinalData/01_200313_data_before_taxon_clean_JJM.RDS")

# Taxonomical data --------------------------------------------------------


# vector with all taxa names 
TU = unique(data2$taxon) %>% sort

# classify with taxize package 
classification.object = classification(TU, db = "gbif")

# save/ load classification.object as backup
saveRDS(classification.object, "03_FinalData/02_200313_classification.object_JJM.RDS")
classification.object = readRDS("03_FinalData/02_200313_classification.object_JJM.RDS")

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

# validate rows filled by scripts
taxontable[order != "NA", clean := T]

# how many are still missing? 
taxontable[clean == F, taxon] %>% length

# create empty variables for loop 
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

# quick save and starting point for later sessions
saveRDS(taxontable, "03_FinalData/03_200313_initial_taxon_clean_JJM.RDS")
taxontable = readRDS("03_FinalData/03_200313_initial_taxon_clean_JJM.RDS")


# run cleaning scripts ...synonyms for synonyms and gbif_errors for taxa that
# gbif does not know or erroneously rates as Synonyms.
clean_diatoms_synonyms.R

View(taxontable)

# remove 
# Myriactis pulvinata - Braunalge

# now enter new rows to clean_diatoms and clean_gbif_errors
taxontable[order != "NA", clean := T]
taxontable[clean == F, taxon]

taxontable <- taxontable[!taxon %in% c("Myriactis pulvinata")]


# save taxontable 
saveRDS(taxontable, "03_FinalData/04_200313_post_correction_taxontable_JJM.RDS")
taxontable = readRDS("03_FinalData/04_200313_post_correction_taxontable_JJM.RDS")
# add taxonomical information to data 

data3 = data2[taxontable, on = "taxon"]


# not in algaebase: Naviculadicta fennica
data3[species == "NA", taxon] %>% unique %>% sort
data3[genus == "NA", taxon] %>% unique %>% sort
data3[family == "NA"] %>% unique(by = "taxon")
data3[order == "NA", list(taxon,genus, family, order)] %>% unique(by = "taxon")  

data3[species == "NA", species := NA]
data3[genus == "NA", genus := NA]
data3[family == "NA", family := NA]
data3[order == "NA", order := NA]


source("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/999_quality_check/lists_of_accepted_orders.R")
dia.orders = unique(data3[, order])
index = dia.orders %in% accepted.diatoms
dia.orders[!index] %>% sort

saveRDS(data3, "03_FinalData/05_200313_final_taxon_join_dia_JJM.RDS")
data3 = readRDS("03_FinalData/05_200313_final_taxon_join_dia_JJM.RDS")



# 03. Final touches -------------------------------------------------------

unique_sites = unique(data3$site)
unique_dates = unique(data3$date)

data3[, c("site_id", "date_id") := list(
  map_int(data3$site, ~ which(unique_sites == .x )),
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

data3[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_JJM")]

n.sample.data = data3[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data4 = data3[n.sample.data, on = "site_id2"]



## prisitne 

data4[, pristine := ifelse(artificial >= 10, 0,1)]

data5 = data4[, list(
  gr_sample_id,
  original_site_name = site,
  date = as.Date(date),
  year,
  season,
  site_id,
  date_id,
  original_name = taxon,
  species,
  genus,
  family,
  order,
  abundance,
  pristine,
  x.coord,
  y.coord,
  EPSG = 3067,
  data.set = "JJM"
  
)]

data5  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data5, paste0("03_FinalData/08_", Sys.Date(), "_diatoms_jjm_w_org_names.RDS"))   
