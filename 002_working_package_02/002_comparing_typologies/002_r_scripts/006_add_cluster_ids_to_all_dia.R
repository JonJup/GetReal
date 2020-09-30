### --- Add cluster ids to diatom data --- ###



# Setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, data.table, magrittr)

# load data -------------------------------------------------------------------
# all diatom data 
all_dia <- readRDS("../001_Community Data/100_Combined/001_Diatoms/003_processed_data/001_speciesXsites_tables/bio2.s.all.RDS")

# cluster ids with sampling id
cluster_ids <- readRDS("003_processed_data/2020-03-03combined_cluster_ids.RDS")

# clean data -------------------------------------------------------------------
setDT(all_dia)


# join data -------------------------------------------------------------------

join1 <- cluster_ids[all_dia,
                     on = "gr_sample_id"]

# save to file  -------------------------------------------------------------------

saveRDS(object = join1, 
        file = paste0("003_processed_data/", Sys.Date(), "_all_dia_with_cluster.RDS"))
