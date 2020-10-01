### ------------------------------------- ###
### --- Create species X sites tables --- ### 
### --- Diatoms                       --- ### 
### ------------------------------------- ###

# date written/modified: 03.08.20 + 14.09
# date used: 03.08.20, 04.08., 05.08, 20.08, 14.09
# Diatoms GetReal WP2
# Jonathan Jupke 

# 01. Setup  --------------------------------------------------------------

pacman::p_load(data.table,
               fuzzySim,
               dplyr,
               magrittr,
               stringr,
               here,
               taxadb
               )

setwd(here())

# read in data ------------------------------------------------
data  = readRDS("003_processed_data/007_2020-08-18all_dia_optimal_taxon_75.RDS")

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
# dat_all_s <- dat_all[final_taxon_level == "species"]
# dat_all_g <- dat_all[final_taxon_level == "genus"]

dat_all <- dat_all[final_taxon_level %in% c("species", "genus")]

## -- Quality Check -- ## 
#if ((nrow(dat_all_s) + nrow(dat_all_g)) == nrow(dat_all)) print("Quality check passed") else print("Quality check failed")
## --               -- ##

dat_all[, final_taxon_level := NULL] 

# 04. Turn to site X species matrix --------------------------------------------------------
dat_all %<>% splist2presabs(sites.col = 1, sp.col = 2)

# 05. remove rare species/ sites --------------------------------------------------------
# i.e. < 5 taxa per site or < 5 occurrences of a taxon in the whole dataset 
# all  
# -- rare taxa -- #

# let's have a look at a histogram of occurrences  
graphics::hist(colSums(x = dat_all[,-1]), xlim = c(1,100), breaks = 10^4)
abline(v = 5, col = "red", lwd = 3)

# now let's see how many sites we loose in each "dropping regime" 
r_id_all_05    <- which(colSums(x = dat_all[,-1]) <  5) + 1
r_id_all_10    <- which(colSums(x = dat_all[,-1]) < 10) + 1
r_id_all_20    <- which(colSums(x = dat_all[,-1]) < 20) + 1

r_names_all_05 <- names(r_id_all_05) %>% str_replace("\\.", " ")
r_names_all_10 <- names(r_id_all_10) %>% str_replace("\\.", " ")
r_names_all_20 <- names(r_id_all_20) %>% str_replace("\\.", " ")

dat_all_nr_05  <- dat_all[, -r_id_all_05]
dat_all_nr_10  <- dat_all[, -r_id_all_10]
dat_all_nr_20  <- dat_all[, -r_id_all_20]
#drop sites without species 
#histograms look good not to much change here. That means that removing the
#(almost 600!) taxa with less than 20 occurrences in the whole data set does not
#affect the overall richness at sites strongly.

par(mfrow = c(1,3))
rowSums(x = dat_all_nr_05[,-1]) %>%  hist(main = "5")
rowSums(x = dat_all_nr_10[,-1]) %>%  hist(main = "10")
rowSums(x = dat_all_nr_20[,-1]) %>%  hist(main = "20")

rowSums(x = dat_all_nr_20[,-1]) %>%  table()

empty_site_id_05        <- which(rowSums(x = dat_all_nr_05[,-1]) == 0)
empty_site_id_10        <- which(rowSums(x = dat_all_nr_10[,-1]) == 0)
empty_site_id_20        <- which(rowSums(x = dat_all_nr_20[,-1]) == 0)

if (length(empty_site_id_05) != 0)  dat_all_nr_05 <- dat_all_nr_05[- empty_site_id_05,]
if (length(empty_site_id_10) != 0)  dat_all_nr_10 <- dat_all_nr_10[- empty_site_id_10,]
if (length(empty_site_id_20) != 0)  dat_all_nr_20 <- dat_all_nr_20[- empty_site_id_20,]

data_all <- dat_all_nr_05


rm(list = setdiff(ls(), c("data_all")));gc()

# - check - # 
if (length(which(colSums(x = data_all[, -1]) < 5)) == 0 
) print("Quality Check Passed") else print("Quality check failed")
# -       - # 
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

saveRDS(data_all, paste0("003_processed_data/001_speciesXsites_tables/008_", Sys.Date(), "_no_rare_20.RDS"))
names(data_all)
