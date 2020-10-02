# -------------------------------------------------------- #
### --- Extract original names form low_impact sites --- ### 
### ---------------------- Diatoms --------------------- ###
### ------------------- Edwin Peters ------------------- ###
# -------------------------------------------------------- #

#date: 03.04.20
#Jonathan Jupke 

# 01. Setup -------------------------------------------------------------------

pacman::p_load(dplyr, magrittr, sf, stringr, data.table)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/019_Edwin Peters//")

# read data ---------------------------------------------------------------

data       <- readRDS("03_FinalData/01_200313_data_before_taxon_clean_dia_Edwin_Peters.RDS")
low_impact <- readRDS("../100_Combined/001_Diatoms/003_processed_data/004_2020-04-09_dia_data_low_ls_all.RDS")


# clean data --------------------------------------------------------------

setDT(data)
setDT(low_impact)

# extract names  ----------------------------------------------------------

low_impact <- low_impact[data.set == "ediwn_peters_dia"]
data_new <- data[V1 %in% low_impact$original_site_name]
data_new %<>% select(species_name, V1, date)
TU       <- sort(unique(data_new$species_name))

# save to file  -----------------------------------------------------------

setwd("../100_Combined/001_Diatoms/orignal_names_low_impact_sites/")

saveRDS(data_new,
        paste0("013_",
               Sys.Date(),
               "_data_from_low_impact_sites_EP.RDS"
        ))
