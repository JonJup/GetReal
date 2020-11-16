### ------------------------------------- ###
### --- Create species X sites tables --- ### 
### --- Diatoms                       --- ### 
### ------------------------------------- ###

# date written/modified: 03.08.20 + 14.09
# date used: 03.08.20, 04.08., 05.08, 20.08, 14.09, 04.11
# GetReal 
# WP2
# Diatoms 
# Jonathan Jupke 

# 01. Setup  --------------------------------------------------------------

pacman::p_load(data.table,
               dplyr,
               fuzzySim,
               here,
               magrittr,
               stringr,
               taxadb
               )


dir_pd = here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/")

# read in data ------------------------------------------------
data  = readRDS(file.path(dir_pd, "007_2020-11-04all_dia_optimal_taxon_75.RDS"))

# Prepare data -------------------------------------------------------------------------
data[, c("final_taxon", "gr_sample_id") := .(
        str_trim(final_taxon, side = "both"),
        as.character(gr_sample_id)
        )]

# 02. Subset  --------------------------------------------------------------
general_subset <- data[(is.na(year) | year >= 2000)]

## -- Quality Check -- ## 
unique(general_subset$season)
## --               -- ##

rm(data);gc()

# 03. Drop columns --------------------------------------------------------

dat_all <-  general_subset[,.(gr_sample_id, final_taxon, final_taxon_level)]
# in the last run (18.08.20) I did not consider seasonal stuff 
# bio2_spr <-      sub_spring[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_sum <-      sub_summer[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_aut <-      sub_autumn[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_win <-      sub_winter[,.(gr_sample_id, final_taxon, final_taxon_level)]

rm(general_subset); gc()

## -- different levels 


dat_all_s <- dat_all[final_taxon_level == "species"]
dat_all_g <- dat_all[final_taxon_level == "genus"]

# dat_all <- dat_all[final_taxon_level %in% c("species", "genus")]

## -- Quality Check -- ## 
if ((nrow(dat_all_s) + nrow(dat_all_g)) == nrow(dat_all)) print("Quality check passed") else print("Quality check failed")
## --               -- ##

dat_all_s[, final_taxon_level := NULL] 
dat_all_g[, final_taxon_level := NULL] 

# 04. Turn to site X species matrix --------------------------------------------------------
dat_all_s %<>% splist2presabs(sites.col = 1, sp.col = 2)
dat_all_g %<>% splist2presabs(sites.col = 1, sp.col = 2)


# 05. remove rare species/ sites --------------------------------------------------------
# i.e. < 5 taxa per site or < 5 occurrences of a taxon in the whole dataset 
# all  
# -- rare taxa -- #

# 1% cutoff
nu_rate_cutoff = round(length(unique(dat_all$gr_sample_id)) * 0.01)

# -- rare taxa -- #
dat_s_id   = which(colSums(x = dat_all_s[,-1]) < nu_rate_cutoff) + 1
dat_g_id   = which(colSums(x = dat_all_g[,-1]) < nu_rate_cutoff) + 1
ch_rare_species = names(dat_s_id) %>% str_replace("\\.", " ")
ch_rare_genera = names(dat_g_id) %>% str_replace("\\.", " ")

ls_rare = list(ch_rare_species, ch_rare_genera)
saveRDS(ls_rare, file.path(dir_pd, "008_a_rare_names.RDS"))

dat_all_s = dat_all_s[, -dat_s_id]
dat_all_g = dat_all_g[, -dat_g_id]

empty_site_id_s        <- which(rowSums(x = dat_all_s[,-1]) == 0)
empty_site_id_g        <- which(rowSums(x = dat_all_g[,-1]) == 0)

if (length(empty_site_id_s) != 0)  dat_all_s = dat_all_s[- empty_site_id_s,]
if (length(empty_site_id_g) != 0)  dat_all_g = dat_all_g[- empty_site_id_g,]

rm(list = setdiff(ls(), c("dat_all_s", "dat_all_g", "dir_pd")));gc()

# -- low richness sites -- #

# in the last run (18.08.20) I did not drop sites with low richness. This is
# artificially increased by me splitting the data set into different taxa
# levels.
# low_taxon_site_id        <- which(rowSums(x = bio2_all_spe[,-1]) < 5)
# low_taxon_site_names_all <- bio2_all$gr_sample_id[low_taxon_site_id]
# bio2_all                 <- bio2_all[-low_taxon_site_id,]
# 
# # # species spring 
# rare_species_id            <- which(colSums(x = bio2_spr[,-1]) < 5) + 1
# rare_species_names_spr     <- names(rare_species_id) %>% str_replace("\\.", " ")
# bio2_spr                 <- bio2_spr[, -rare_species_id]
# low_taxon_site_id          <- which(rowSums(x = bio2_spr[,-1]) < 5)
# low_taxon_site_names_spr   <- bio2_spr$gr_sample_id[low_taxon_site_id]
# bio2_spr                 <- bio2_spr[-low_taxon_site_id,]
# 
# # # species summer 
# rare_species_id            <- which(colSums(x = bio2_sum[,-1]) < 5) + 1
# rare_species_names_sum     <- names(rare_species_id) %>% str_replace("\\.", " ")
# bio2_sum                 <- bio2_sum[, -rare_species_id]
# low_taxon_site_id          <- which(rowSums(x = bio2_sum[,-1]) < 5)
# low_taxon_site_names_sum   <- bio2_sum$gr_sample_id[low_taxon_site_id]
# bio2_sum                 <- bio2_sum[-low_taxon_site_id,]
# 
# # # species autumn
# rare_species_id            <- which(colSums(x = bio2_aut[,-1]) < 5) + 1
# rare_species_names_aut     <- names(rare_species_id) %>% str_replace("\\.", " ")
# bio2_aut                 <- bio2_aut[, -rare_species_id]
# low_taxon_site_id          <- which(rowSums(x = bio2_aut[,-1]) < 5)
# low_taxon_site_names_aut   <- bio2_aut$gr_sample_id[low_taxon_site_id]
# bio2_aut                 <- bio2_aut[-low_taxon_site_id,]
# 
# # # species winter 
# rare_species_id            <- which(colSums(x = bio2_win[,-1]) < 5) + 1
# rare_species_names_win     <- names(rare_species_id) %>% str_replace("\\.", " ")
# bio2_win                 <- bio2_win[, -rare_species_id]
# low_taxon_site_id          <- which(rowSums(x = bio2_win[,-1]) < 5)
# low_taxon_site_names_win   <- bio2_win$gr_sample_id[low_taxon_site_id]
# bio2_win                 <- bio2_win[-low_taxon_site_id,]

# 08. Save data to file ---------------------------------------------------

saveRDS(dat_all_s, file.path(dir_pd, paste0( "001_speciesXsites_tables/008_", Sys.Date(), "_1_percent_species.RDS")))
saveRDS(dat_all_g, file.path(dir_pd, paste0( "001_speciesXsites_tables/008_", Sys.Date(), "_1_percent_genus.RDS")))


