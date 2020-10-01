# -------------------------------------------------------- #
### --- Extract original names form low_impact sites --- ### 
### ---------------------- Diatoms --------------------- ###
### ----------------------- WISER ---------------------- ###
# -------------------------------------------------------- #

#date: 03.04.20
#Jonathan Jupke 

# 01. Setup -------------------------------------------------------------------

pacman::p_load(dplyr, magrittr, sf, stringr, data.table)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/012_Christian Feld/")


# read data ---------------------------------------------------------------

data     <- readRDS("03_FinalData/01_191212_data_before_taxon_clean_dia_WISER.RDS")
low_impact <- readRDS("../100_Combined/001_Diatoms/003_processed_data/004_2020-04-09_dia_data_low_ls_all.RDS")

# clean data --------------------------------------------------------------

setDT(data)
setDT(low_impact)

low_impact <- low_impact[data.set == "dia_Wiser"]
data_new <- data[site_name %in% low_impact$original_site_name]
data_new %<>% select(Taxon, site_name)
TU <- sort(unique(data_new$Taxon))

# save to file  -----------------------------------------------------------

setwd("../100_Combined/001_Diatoms/orignal_names_low_impact_sites/")

saveRDS(data_new,
        paste0("009_",
               Sys.Date(),
               "_data_from_low_impact_sites_WS.RDS"
        ))
