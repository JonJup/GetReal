# ---------------------------------------- #
### --- Clean Diatom data Joan Gomà --- ### 
# ---------------------------------------- #

# GR WP2 
# Cleaning/ Homogenization of diatom data from Joan Gomà
# EPSG: 23031 -- ED50 / UTM zone 31N

### --- OVERVIEW --- ### 
# 01. Setup
# 02. Cleaning Data 
# 03. Taxonomic Cleaning 
# 04. Final touches
### ---------------- ###


# 01. Setup ---------------------------------------------------------------
pacman::p_load(dplyr, 
               data.table, 
               lubridate,
               magrittr, 
               sf, 
               stringr,
               taxize,
               tmap,
               here)

setwd(here("017_Joan_Goma/"))
site.data <- readxl::read_excel("01_OriginalData/Diatoms CAT JGomà.xlsx", sheet = 3, skip = 1) %>%
        setDT
names(site.data) = c("id","Region","stream", "site","x.coord","y.coord", "s", "a", "d")
site.data2 = site.data[,
                       list(
                               site_id = str_remove_all(id, "\ "),
                               site,
                               stream,
                               x.coord,
                               y.coord
                       )
                       ]

data4 = readRDS("03_FinalData/05_191210_final_taxon_join_dia_Joan_Goma.RDS")
data4[, site_id := str_remove(site_id, "-[0-9]$")]
data5 = merge(x = data4, y = site.data2, all.x = T, by = "site_id")
names(data5)[1] <- "site_org_id"
unique_sites = unique(data5$site_org_id)
unique_dates = unique(data5$date)

data5[, c("site_id", "date_id") := list(
        map_int(data5$site_org_id, ~ which(unique_sites == .x )),
        map_int(data5$date, ~ which(unique_dates == .x ))
)]


data5[, site_id2 := case_when(
        nchar(trunc(site_id)) == 1 ~ paste0("0000", site_id),
        nchar(trunc(site_id)) == 2 ~ paste0("000", site_id),
        nchar(trunc(site_id)) == 3 ~ paste0("00", site_id),
        nchar(trunc(site_id)) == 4 ~ paste0("0", site_id),
        nchar(trunc(site_id)) == 5 ~ paste0(site_id))]

data5[, date_id2 := case_when(
        nchar(trunc(date_id)) == 1 ~ paste0("0000", date_id),
        nchar(trunc(date_id)) == 2 ~ paste0("000", date_id),
        nchar(trunc(date_id)) == 3 ~ paste0("00", date_id),
        nchar(trunc(date_id)) == 4 ~ paste0("0", date_id),
        nchar(trunc(date_id)) == 5 ~ paste0(date_id))]

data5[,gr_sample_id := paste0("site_", site_id2, "_date_", date_id2,"_dia_joan_goma")]

n.sample.data = data5[, .(n.samples = length(unique(date_id2))), .(site_id2)]

data6 = data5[n.sample.data, on = "site_id2"]

data7 = data6[, list(
        gr_sample_id,
        original_site_name = site_org_id,
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
        abundance = Abundanz,
        pristine = NA,
        x.coord,
        y.coord,
        EPSG = 23031,
        data.set = "joan_goma"
        
)]
data7  %<>% unique(by = c("gr_sample_id", "species", "genus", "family", "order", "original_name"))
saveRDS(data7, paste0("03_FinalData/08_", Sys.Date(), "_diatoms_joan_goma_w_org_names.RDS"))   


