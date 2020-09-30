## -- What orders are in all data sets -- ## 

# date: 17.03.20


# Setup -------------------------------------------------------------------
pacman::p_load(sf, data.table, dplyr, magrittr)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/")

# load data ---------------------------------------------------------------
ld    <- st_read("001_Landau/03_FinalData/06_200108_MZB_Landau.gpkg") 
ld_h  <- st_read("001_Landau/03_FinalData/06_191218_MZB_Landau_Hungary.gpkg")
MI    <- st_read("002_Miguel_Iglesias/03_FinalData/06_200108_MZB_Miguel_Iglesias.gpkg")
MI_pa <- st_read("002_Miguel_Iglesias/03_FinalData/06_191217_MZB_Miguel_Iglesias_PA.gpkg")
na    <- st_read("004_Naiades/03_FinalData/06_200108_MZB_Naiades.gpkg")
pb_c  <- st_read("005_Pepe_Barquin/03_FinalData/06_191216_MZB_Cantabria_Pepe.gpkg")
pb_p  <- st_read("005_Pepe_Barquin/03_FinalData/06_191216_MZB_Picos_Pepe.gpkg")
ds    <- st_read("007_Denes Schmera/03_FinalData/06_200110_MZB_Denes_Schmera.gpkg")
nb_g  <- st_read("008_Nuria Bonada/03_FinalData/06_200110_MZB_GUA_Nuria_Bonada.gpkg")
nb_m  <- st_read("008_Nuria Bonada/03_FinalData/06_191216_MZB_MED_Nuria_Bonada.gpkg")
hvd   <- st_read("009_Hermann van Dam/03_FinalData/06_191216_MZB_Ecosurv.gpkg")
rp    <- st_read("011_RivPacs/03_FinalData/06_200108_MZB_Rivpacs.gpkg")
stars <- st_read("012_Christian Feld/03_FinalData/06_191212_MZB_STARS.gpkg")
wiser <- st_read("012_Christian Feld/03_FinalData/06_191212_MZB_mzb_WISER.gpkg")
ob    <- st_read("013_Oscar Belmar/03_FinalData/06_191211_MZB_Oscar_Belmar.gpkg")
ls    <- st_read("014_Sandin_Kahlert/03_FinalData/06_191211_MZB_Leonard_Sandin.gpkg")
klh   <- st_read("015_KaisaLenaHuttunen/03_FinalData/06_191211_MZB_Kaisa-Leena_Huttunen.gpkg")
pup   <- st_read("016_PhillipeUsseglioPolterra/03_FinalData/06_191211_MZB_Philippe_Usseglio_Polatera.gpkg")
ep    <- st_read("019_Edwin Peters/03_FinalData/06_191205_MZB_ediwn_peters.gpkg")
ph    <- st_read("020_Peter_Haase/03_FinalData/06_191129_MZB_peter_haase.gpkg")
mc    <- st_read("021_Mirela Cimpea/03_FinalData/06_200110_MZB_mirella_cimpean.gpkg")

# extract unique orders ---------------------------------------------------
for (i in c("ld", "ld_h", "MI", "MI_pa", "na", "pb_c", "pb_p", "ds", "nb_g",
            "nb_m", "hvd", "rp", "stars", "wiser", "ob", "ls", "klh", "pup",
            "ep", "ph", "mc")) {
        
        data <- get(i)
        orders <- unique(data$order)
        if ("factor" %in% class(orders)) orders <- as.character(orders)
        assign(
                x = paste0(i,"_orders"),
                value = orders
        )
}

# find universal orders  --------------------------------------------------
test <- Reduce(intersect, list(ld_orders, 
                               ld_h_orders,
                               MI_orders,   
                               MI_pa_orders,
                               na_orders,
                               pb_c_orders,
                               pb_p_orders,
                               ds_orders,
                               #nb_g_orders,
                               #nb_m_orders,
                               hvd_orders,
                               rp_orders,
                               stars_orders,
                               wiser_orders,
                               ob_orders,
                               ls_orders,
                               klh_orders,
                               pup_orders,
                               ep_orders,
                               ph_orders,
                               mc_orders))

# save to file  -----------------------------------------------------------

saveRDS(file = paste0("100_Combined/002_invertebrates/003_processed_data/999_",
                      Sys.Date(),
                      "_common_orders.RDS"),
        object = test)
