### ----------------------------------------- ### 
# --- Clean Diatom data from Mirela Cimpean --- #
### ----------------------------------------- ###

# date: 27.11.19 

# In this script I clean the diatom data from Mirela Cîmpean

### --- OVERVIEW --- ###
# 01. Setup
# 02. Data Cleaning
# 03. Taxonomic cleaning
# 04. Final touches
### ---------------- ###


# 01. Setup -------------------------------------------------------------------
pacman::p_load(here, dplyr, taxize, magrittr, sf, stringr, data.table, lubridate, readxl, tmap, measurements, here)
setwd(here("021_Mirela Cimpea/"))
data4 = readRDS("03_FinalData/05_191128_final_taxon_join_diatom_mirela_cimpean.RDS")


# 04. Final Touches -------------------------------------------------------
for (i in seq_along(unique(data4$site))) {

        site_ide_var = i 
        length_of_id = nchar(trunc(i))
        site_ide_var2 = case_when(length_of_id == 1 ~ paste0("0000",i),
                                  length_of_id == 2 ~ paste0("000",i),
                                  length_of_id == 3 ~ paste0("00",i),
                                  length_of_id == 4 ~ paste0("0",i),
                                  length_of_id == 5 ~ paste0(i))
        
        data4[site == unique(data4$site)[i], site_id := site_ide_var2]
        
        dates = data4[site_id == site_ide_var2, "date"] %>% 
                pull() %>% 
                unique %>% 
                sort
        
        NumberOfSamplings = length(dates)
        
        for (k in seq_along(dates)) {
                
                data.filler = case_when(
                        k < 10 ~ paste0("00",k),
                        k > 10 & k < 100 ~ paste0("0", k), 
                        k > 100 ~ paste0(k)
                )
                
                data4[site_id == site_ide_var2 & date == dates[k], date_id := k]
        }
        
        data4[site_id == site_ide_var2, n.samples := NumberOfSamplings]
        
}
data4[,gr_sample_id := paste0("site_", site_id, "_date_", date_id,"_DIA_mirella_cimpean")]


unique(data4, by = c("site_id", "date_id")) %>% arrange(site_id, date_id)

data4[, data.set := "mirella_cimpea"]

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
        abundance = NA,
        pristine = NA,
        data.set = "MC",
        x.coord,
        y.coord,
        EPSG

)]

data5  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data5, paste0("03_FinalData/diatoms/08_", Sys.Date(), "_diatoms_mirella_cimpean_w_org_names.RDS"))   
        



