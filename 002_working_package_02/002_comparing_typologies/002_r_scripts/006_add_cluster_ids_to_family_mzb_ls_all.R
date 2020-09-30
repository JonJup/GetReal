# --------------------------------------- #
### --- Add cluster ids to mzb data --- ###
# --------------------------------------- #

#date: 26.03.20
#Jonathan Jupke

# Setup -------------------------------------------------------------------

setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/003_processed_data/")

pacman::p_load(sf, dplyr, data.table, magrittr)

# load data -------------------------------------------------------------------

all_mzb <- readRDS("001b_speciesXsites_tables_ls_all/2020-04-01bio2.f.all_ls_all.RDS")

sites <- readRDS("003_2020-03-31_mzb_sites_close_to_ls_large.RDS")

# clean data -------------------------------------------------------------------

setDT(all_mzb)

# for ls data 
sites %<>%
        st_drop_geometry() %>%  
        setDT

all_mzb[, gr_sample_id := as.character(gr_sample_id)]

length(unique(all_mzb$gr_sample_id))
length(unique(sites$gr_sample_id))

# join data -------------------------------------------------------------------

join1 <- sites[all_mzb,
                     on = "gr_sample_id"]

# quality check 
sum(is.na(join1$ls_bd_20))

# save to file  -------------------------------------------------------------------
setwd("~/01_Uni/03_GetReal/002_WP_02/002_comparing_typologies/003_processed_data/")
saveRDS(object = join1, 
        file = paste0("003_", Sys.Date(), "_all_ls_all_family_mzb_with_cluster.RDS"))
