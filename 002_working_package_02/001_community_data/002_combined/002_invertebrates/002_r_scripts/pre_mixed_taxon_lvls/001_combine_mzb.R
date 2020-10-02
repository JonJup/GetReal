### --------------------------- ###
# --- Combine all mzb data --- #
### --------------------------- ###

# 08.01.20

# Combine single mzb data sets to one big data set

### --- OVERVIEW --- ### 
#01. Setup
#02. Data input
### ---------------- ###


# 01.Setup -------------------------------------------------------------------

pacman::p_load(sf, magrittr, dplyr, data.table, stringr)

#setwd(here::here())
#or
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/")

# 02.Data input --------------------------------------------------------------

set01 <- st_read("001_Landau/03_FinalData/06_200108_MZB_Landau.gpkg", quiet = TRUE, stringsAsFactors = FALSE)
set02 <- st_read("001_Landau/03_FinalData/06_191218_MZB_Landau_Hungary.gpkg", quiet = TRUE, stringsAsFactors = FALSE)
set03 <- st_read("002_Miguel_Iglesias/03_FinalData/06_200108_MZB_Miguel_Iglesias.gpkg", quiet = TRUE, stringsAsFactors = FALSE)
set04 <- st_read("002_Miguel_Iglesias/03_FinalData/06_191217_MZB_Miguel_Iglesias_PA.gpkg", quiet = TRUE, stringsAsFactors = FALSE)
set05 <- st_read("004_Naiades/03_FinalData/06_200108_MZB_Naiades.gpkg", quiet = TRUE, stringsAsFactors = FALSE)
set06 <- st_read("005_Pepe_Barquin/03_FinalData/06_191216_MZB_Cantabria_Pepe.gpkg", quiet = TRUE, stringsAsFactors = FALSE)
set07 <- st_read("005_Pepe_Barquin/03_FinalData/06_191216_MZB_Picos_Pepe.gpkg", quiet = TRUE, stringsAsFactors = FALSE)
set08 <- st_read("007_Denes Schmera/03_FinalData/06_200110_MZB_Denes_Schmera.gpkg", quiet = TRUE, stringsAsFactors = FALSE)
set09 <- st_read("008_Nuria Bonada/03_FinalData/06_191216_MZB_MED_Nuria_Bonada.gpkg",quiet = TRUE, stringsAsFactors = FALSE)
set10 <- st_read("008_Nuria Bonada/03_FinalData/06_200110_MZB_GUA_Nuria_Bonada.gpkg", quiet = T, stringsAsFactors = FALSE)
set11 <- st_read("009_Hermann van Dam/03_FinalData/06_191216_MZB_Ecosurv.gpkg", quiet = T, stringsAsFactors = FALSE)
set12 <- st_read("011_RivPacs/03_FinalData/06_200108_MZB_Rivpacs.gpkg", quiet = T, stringsAsFactors = FALSE)
set13 <- st_read("012_Christian Feld/03_FinalData/06_191212_MZB_mzb_WISER.gpkg", quiet = T, stringsAsFactors = FALSE)
set14 <- st_read("012_Christian Feld/03_FinalData/06_191212_MZB_STARS.gpkg", quiet = T, stringsAsFactors = FALSE)
set15 <- st_read("013_Oscar Belmar/03_FinalData/06_191211_MZB_Oscar_Belmar.gpkg", quiet = T, stringsAsFactors = FALSE)
set16 <- st_read("014_Sandin_Kahlert/03_FinalData/06_191211_MZB_Leonard_Sandin.gpkg", quiet = T, stringsAsFactors = FALSE)
set17 <- st_read("015_KaisaLenaHuttunen/03_FinalData/06_191211_MZB_Kaisa-Leena_Huttunen.gpkg", quiet = T, stringsAsFactors = FALSE)
set18 <- st_read("016_PhillipeUsseglioPolterra/03_FinalData/06_191211_MZB_Philippe_Usseglio_Polatera.gpkg", quiet = T, stringsAsFactors = FALSE)
set19 <- st_read("019_Edwin Peters/03_FinalData/06_191205_MZB_ediwn_peters.gpkg", quiet = T, stringsAsFactors = FALSE)
set20 <- st_read("020_Peter_Haase/03_FinalData/06_191129_MZB_peter_haase.gpkg", quiet = T, stringsAsFactors = FALSE)
set21 <- readRDS("021_Mirela Cimpea/03_FinalData/06_200110_MZB_mirella_cimpean.RDS")
beepr::beep()
data = ls()

# fix naiades 
set05_02 <- select(set05, original_site_name)
set05_02 <- unique(set05_02, by = "original_site_name")


set05 %>% 
         st_drop_geometry()%>% 
        distinct() -> 
        set05 
set05 <- left_join(set05, 
                     set05_02, 
                     by = c("original_site_name"))

beepr::beep()
# check crs 
# crs missing for 19 and 20 
for (i in seq_along(data)) {

       print(
               paste(
                       i,
                       st_crs(
                               get(
                                   data[i]
                                   )
                               )[[1]]
               )
       )

}


assign(data[19],
       st_transform(
                get(
                        data[19]
                ),
                crs = 4326
        )
)
assign(data[20],
       st_transform(
               get(
                       data[20]
               ),
               crs = 4326
       )
)


#check names 
# for (i in seq_along(data)) {
#         
#         print(
#                 paste(
#                         i,
#                         names(
#                                 get(
#                                         data[i]
#                                 )
#                         )
#                 )
#         )
#         
# }
# 
# names(set1)
# names(set13)
# harmonize ---------------------------------------------------------------

setdiff(names(set01), 
        names(set21))

set09$date = NA
set17$year = NA
names(set21)[21] = "geom"

names(set01)[3]
names(set04)[3]

set04 %<>% mutate(date = as.Date(date))
 
set06 %<>% mutate(date = as.Date(date)) 
set07 %<>% mutate(date = as.Date(date)) 
set08 %<>% mutate(date = as.Date(date)) 
set09 %<>% mutate(date = as.Date(date)) 

set12 %<>% mutate(date = as.Date(date))
set14 %<>% mutate(date = as.Date(date))
set17 %<>% mutate(date = as.Date(date))
set19 %<>% mutate(date = as.Date(date))
 

all_mzb = rbindlist(
        list(
                set01, 
                set02,
                set03,
                set04,
                set05,
                set06,
                set07,
                set08,
                #set09,
                #set10,
                set11,
                set12,
                set13,
                set14,
                set15,
                set16,
                set17,
                set18,
                set19,
                set20,
                set21
        ),
        use.names = T
)


mzb_sites = unique(all_mzb, by = "gr_sample_id")

mzb_sites_sf = st_as_sf(mzb_sites)
all_mzb_sf = st_as_sf(all_mzb)

saveRDS(all_mzb_sf, 
        paste0("100_Combined/002_invertebrates/003_processed_data/001_",
        Sys.Date(),
        "_all_mzb_data.RDS"))
saveRDS(mzb_sites_sf, 
        paste0("100_Combined/002_invertebrates/003_processed_data/001_",
               Sys.Date(),
               "_all_mzb_sites.RDS"))


# st_write(mzb_sites_sf, "100_Combined/MZB/combined_data/200110_all_mzb_sites.gpkg")
# st_write(all_mzb_sf, "100_Combined/MZB/combined_data/200110_all_mzb_data.gpkg")

