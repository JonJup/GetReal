# --------------------------------------- #
### --- Add cluster ids to mzb data --- ###
# --------------------------------------- #

#date: 26.03.20
#Jonathan Jupke

# Setup -------------------------------------------------------------------

setwd("~/01_Uni/03_GetReal/002_WP_02/")

pacman::p_load(sf, dplyr, data.table, magrittr)

# load data -------------------------------------------------------------------

# all mzb data 
all_mzb <- readRDS("001_Community Data/100_Combined/002_invertebrates/003_processed_data/001_speciesXsites_tables/2020-03-24bio2.s.all.RDS")
# or ls data 
all_mzb <- readRDS("001_Community Data/100_Combined/002_invertebrates/003_processed_data/001_speciesXsites_tables/2020-03-26bio2.s.all_ls.RDS")

# cluster ids with sampling id
cluster_ids <- readRDS("002_comparing_typologies/003_processed_data/002_2020-03-26_mzb_combined_cluster_ids.RDS")
# or for ls 
cluster_ids <- readRDS("002_comparing_typologies/003_processed_data/001_2020-03-26_clean_ls_mzb_with_ls_broad_type.RDS")

# clean data -------------------------------------------------------------------

setDT(all_mzb)

# for ls data 
cluster_ids %<>%
        st_drop_geometry() %>%  
        setDT

all_mzb[, gr_sample_id := as.character(gr_sample_id)]

length(unique(all_mzb$gr_sample_id))
length(unique(cluster_ids$gr_sample_id))

sum()

# join data -------------------------------------------------------------------

join1 <- cluster_ids[all_mzb,
                     on = "gr_sample_id"]

# save to file  -------------------------------------------------------------------

saveRDS(object = join1, 
        file = paste0("002_comparing_typologies/003_processed_data/003_", Sys.Date(), "_all_ls_mzb_with_cluster.RDS"))
