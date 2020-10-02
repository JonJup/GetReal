## -- Subset MZB to common orders -- ## 

# date: 18.03.20


# setup  ------------------------------------------------------------------

pacman::p_load(sf, dplyr, data.table)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/")


# read data  --------------------------------------------------------------

data          <- readRDS("003_processed_data/001_2020-03-17_all_mzb_data.RDS")
common_orders <- readRDS("003_processed_data/999_2020-03-17_common_orders.RDS")


data2 <- filter(data, order %in% common_orders)

sites <- data2
setDT(sites)
sites <- unique(sites, by = "gr_sample_id")

saveRDS(
        object = data2,
        file = paste0(
                "003_processed_data/002",
                Sys.Date(),
                "_mzb_data_common_orders.RDS"
        )
)
saveRDS(
        object = sites,
        file = paste0(
                "003_processed_data/002",
                Sys.Date(),
                "_mzb_sites_common_orders.RDS"
        )
)
