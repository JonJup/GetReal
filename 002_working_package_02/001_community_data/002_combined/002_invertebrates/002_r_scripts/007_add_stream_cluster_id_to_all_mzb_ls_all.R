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

DIR = list(pd = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"))
OPT = list(flow = TRUE)
# load data -------------------------------------------------------------------
ls_mzb   = readRDS(file.path(DIR$pd, "05_sxs_list.RDS"))
dt_sites = readRDS(file.path(DIR$pd, "003b_2020-11-03_mzb_sites_1585_close_to_ls_withWSO.RDS"))

# clean data -------------------------------------------------------------------
# for ls data 
dt_sites %<>%
        st_drop_geometry() %>%
        setDT

if (!OPT$flow){
        print(uniqueN(ls_mzb$spe$gr_sample_id))
        print(uniqueN(ls_mzb$gen$gr_sample_id))
        print(uniqueN(ls_mzb$foh$gr_sample_id))
        print(uniqueN(dt_sites$gr_sample_id))
}

# join data -------------------------------------------------------------------
ls_joind = list()
ls_joind$spe <- dt_sites[ls_mzb$spe, on = "gr_sample_id"]
ls_joind$gen <- dt_sites[ls_mzb$gen, on = "gr_sample_id"]
ls_joind$foh <- dt_sites[ls_mzb$foh, on = "gr_sample_id"]



# quality check, ok if TRUE  
sum(is.na(ls_joind$spe$ls_bd_20)) == 0 & 
sum(is.na(ls_joind$gen$ls_bd_20)) == 0 & 
sum(is.na(ls_joind$foh$ls_bd_20)) == 0 

# save to file  -------------------------------------------------------------------
saveRDS(object = ls_joind,  file = file.path(DIR$pd, "06_sxs_w_LS20.RDS"))

