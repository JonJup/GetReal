# ------------------------------------------- #
### --- Add cluster ids to mzb data1585 --- ###
# ------------------------------------------- #

#date: 19.06.20
#date used: 01.07.20, 04.11
#Jonathan Jupke

# Setup -------------------------------------------------------------------

pacman::p_load(data.table,
               dplyr, 
               here, 
               magrittr,
               sf)

dir_pd = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/")

# load data -------------------------------------------------------------------
all_mzb_spe = readRDS(file.path(dir_pd, "001_speciesXsites_tables/2020-11-04_bio2_all_spe.RDS"))
all_mzb_gen = readRDS(file.path(dir_pd, "001_speciesXsites_tables/2020-11-04_bio2_all_gen.RDS"))
all_mzb_foh = readRDS(file.path(dir_pd, "001_speciesXsites_tables/2020-11-04_bio2_all_foh.RDS"))
sites       = readRDS(file.path(dir_pd, "003b_2020-11-03_mzb_sites_1585_close_to_ls_withWSO.RDS"))

# clean data -------------------------------------------------------------------
setDT(all_mzb_spe)
setDT(all_mzb_gen)
setDT(all_mzb_foh)

# for ls data 
sites %<>%
        st_drop_geometry() %>%
        setDT

all_mzb_spe[, gr_sample_id := as.character(gr_sample_id)]
all_mzb_gen[, gr_sample_id := as.character(gr_sample_id)]
all_mzb_foh[, gr_sample_id := as.character(gr_sample_id)]

length(unique(all_mzb_spe$gr_sample_id))
length(unique(all_mzb_gen$gr_sample_id))
length(unique(all_mzb_foh$gr_sample_id))
length(unique(sites$gr_sample_id))

# join data -------------------------------------------------------------------
join_spe <- sites[all_mzb_spe, on = "gr_sample_id"]
join_gen <- sites[all_mzb_gen, on = "gr_sample_id"]
join_foh <- sites[all_mzb_foh, on = "gr_sample_id"]

# quality check, ok if TRUE  
sum(is.na(join_spe$ls_bd_20)) == 0 & 
sum(is.na(join_gen$ls_bd_20)) == 0 & 
sum(is.na(join_foh$ls_bd_20)) == 0 

# save to file  -------------------------------------------------------------------
saveRDS(object = join_spe,  file = file.path(dir_pd, paste0("005_", Sys.Date(), "obs_low_impact_w_LS20_spe.RDS")))
saveRDS(object = join_gen,  file = file.path(dir_pd, paste0("005_", Sys.Date(), "obs_low_impact_w_LS20_gen.RDS")))
saveRDS(object = join_foh,  file = file.path(dir_pd, paste0("005_", Sys.Date(), "obs_low_impact_w_LS20_foh.RDS")))
