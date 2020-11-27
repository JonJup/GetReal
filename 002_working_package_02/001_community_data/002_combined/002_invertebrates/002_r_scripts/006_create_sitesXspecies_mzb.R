        ### --- Create species X sites tables --- ### 

# date written/modified: 19.06.2020 01.07 + 14.09
# date used: ..., 01.07.20, 03.07.20 + 14.09 + 03.11 + 25.11
#Jonathan Jupke 

# 01. Setup  --------------------------------------------------------------

pacman::p_load(data.table,
               dplyr,
               fuzzySim,
               here,
               magrittr,
               stringr)




DIR = list(
        pd = here(
                "002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"
        ),
        rs = here(
                "002_working_package_02/001_community_data/002_combined/002_invertebrates/002_r_scripts/"
        )
)


OPT=list(flow=TRUE)
# read in and prepare data ------------------------------------------------
# load data 
dt_mzb  = readRDS(file.path(DIR$pd, "004_2020-11-03_mzb_data1585_low_impact.RDS"))

# trim names 
dt_mzb[, final_taxon := str_trim(final_taxon, side = "both")]

#unique(dt_mzb$final_taxon) %>% sort

dt_mzb[final_taxon == "Stratiomyiidae", final_taxon := "Stratiomyidae"]
dt_mzb[final_taxon == "Notonectidae", final_taxon_level := "family"]
dt_mzb$gr_sample_id %<>% as.character()
dt_mzb = dt_mzb[(is.na(year) | year >= 2000)]


# 03. Drop columns --------------------------------------------------------
dt_mzb = dt_mzb[,.(gr_sample_id, final_taxon, final_taxon_level, ls_bd_20)]
# in the last run (03.07.20) I did not consider seasonal stuff 
# bio2_spr <-      sub_spring[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_sum <-      sub_summer[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_aut <-      sub_autumn[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_win <-      sub_winter[,.(gr_sample_id, final_taxon, final_taxon_level)]

## -- different levels 
ls_mzb = list()
ls_mzb$spe = dt_mzb[final_taxon_level == "species"]
ls_mzb$gen = dt_mzb[final_taxon_level == "genus"]
ls_mzb$foh = dt_mzb[final_taxon_level %in% c("family", "order", "subclass")]

ls_mzb$spe[, final_taxon_level := NULL] 
ls_mzb$gen[, final_taxon_level := NULL] 
ls_mzb$foh[, final_taxon_level := NULL]

ls_rt = list(
        spe = unique(ls_mzb$spe[,.(gr_sample_id, ls_bd_20)], by = "gr_sample_id"),
        gen = unique(ls_mzb$gen[,.(gr_sample_id, ls_bd_20)], by = "gr_sample_id"),
        foh = unique(ls_mzb$foh[,.(gr_sample_id, ls_bd_20)], by = "gr_sample_id")
)

# sanity check 
if (!OPT$flow) {
        ls_rt$spe
        ls_rt$gen
        ls_rt$foh
}

# 04. Turn to site X species matrix --------------------------------------------------------
ls_mzb$spe %<>% splist2presabs(sites.col = 1, sp.col = 2) %>%  setDT 
ls_mzb$gen %<>% splist2presabs(sites.col = 1, sp.col = 2) %>%  setDT
ls_mzb$foh %<>% splist2presabs(sites.col = 1, sp.col = 2) %>%  setDT

# sanity check 
if (!OPT$flow) {
        ls_mzb$spe
        ls_mzb$gen
        ls_mzb$foh
}

ls_mzb$spe = ls_rt$spe[ls_mzb$spe, on = "gr_sample_id"]
ls_mzb$gen = ls_rt$gen[ls_mzb$gen, on = "gr_sample_id"]
ls_mzb$foh = ls_rt$foh[ls_mzb$foh, on = "gr_sample_id"]

# sanity check 
if (!OPT$flow) {
        ls_mzb$spe
        ls_mzb$gen
        ls_mzb$foh
}

# in the last run (03.07.20) I did not consider seasonal stuff 
# bio2_spr %<>% splist2presabs(sites.col = 1, sp.col = 2) 
# bio2_sum %<>% splist2presabs(sites.col = 1, sp.col = 2) 
# bio2_aut %<>% splist2presabs(sites.col = 1, sp.col = 2) 
# bio2_win %<>% splist2presabs(sites.col = 1, sp.col = 2) 

# 05. remove rare species/ sites --------------------------------------------------------

# 1% cutoff

nu_rate_cutoff = dt_mzb[, uniqueN(gr_sample_id), by = ls_bd_20]
nu_rate_cutoff[, one_percent :=  round(V1 * 0.05,0)]

# stream types with less than 25 samples 
nu_rate_cutoff = nu_rate_cutoff[V1 > 25]
                                
ls_mzb$spe = ls_mzb$spe[ls_bd_20 %in% nu_rate_cutoff$ls_bd_20]
ls_mzb$gen = ls_mzb$gen[ls_bd_20 %in% nu_rate_cutoff$ls_bd_20]
ls_mzb$foh = ls_mzb$foh[ls_bd_20 %in% nu_rate_cutoff$ls_bd_20]

# -- rare taxa -- #

# I cannot completely remove the taxa but I can set occurences within the stream type to zero

ls_rare = vector("list", nrow(nu_rate_cutoff))

for (i in 1:nrow(nu_rate_cutoff)){
        ls_loop        = list()
        ls_loop$rt     = nu_rate_cutoff$ls_bd_20[i]
        ls_loop$cutoff = nu_rate_cutoff$one_percent[i]
        ls_loop$spe    = ls_mzb$spe[ls_bd_20 == ls_loop$rt]
        ls_loop$gen    = ls_mzb$gen[ls_bd_20 == ls_loop$rt]
        ls_loop$foh    = ls_mzb$foh[ls_bd_20 == ls_loop$rt]
        ls_loop$spe_sum = colSums(x = ls_loop$spe[,-(1:2)])
        ls_loop$gen_sum = colSums(x = ls_loop$gen[,-(1:2)])
        ls_loop$foh_sum = colSums(x = ls_loop$foh[,-(1:2)])
        ls_loop$spe_names = names(ls_loop$spe_sum)[ls_loop$spe_sum < ls_loop$cutoff]
        ls_loop$gen_names = names(ls_loop$gen_sum)[ls_loop$gen_sum < ls_loop$cutoff]
        ls_loop$foh_names = names(ls_loop$foh_sum)[ls_loop$foh_sum < ls_loop$cutoff]
        
        
        if (sum(ls_mzb$spe[ls_bd_20 == ls_loop$rt, lapply(.SD, FUN = sum), .SDcols = ls_loop$spe_names]) != 0){
                for (k in seq_along(ls_loop$spe_names)){
                        ls_mzb$spe[ls_bd_20 == ls_loop$rt, ls_loop$spe_names[k] := 0]
                }
        }
        if (sum(ls_loop$gen[, lapply(.SD, FUN = sum), .SDcols = ls_loop$gen_names]) != 0){
                for (k in seq_along(ls_loop$gen_names)){
                        ls_mzb$gen[ls_bd_20 == ls_loop$rt, ls_loop$gen_names[k] := 0]
                }
        }
        if (sum(ls_loop$foh[, lapply(.SD, FUN = sum), .SDcols = ls_loop$foh_names]) != 0){
                for (k in seq_along(ls_loop$foh_names)){
                        ls_mzb$foh[ls_bd_20 == ls_loop$rt, ls_loop$foh_names[k] := 0]
                }
        }
        
        ls_rare[[i]]$spe = ls_loop$spe_names
        ls_rare[[i]]$gen = ls_loop$gen_names
        ls_rare[[i]]$foh = ls_loop$foh_names
        names(ls_rare)[i] = ls_loop$rt

        print(i)
        rm(ls_loop,i);gc()
}

saveRDS(ls_rare, file.path(DIR$pd, "0x_rare_taxa_list.RDS"))

rm(nu_rate_cutoff)

# old approach  # co 25.11.20
# rare_id_all_spe      <- which(colSums(x = bio2_all_spe[,-1]) < nu_rate_cutoff) + 1
# rare_id_all_gen      <- which(colSums(x = bio2_all_gen[,-1]) < nu_rate_cutoff) + 1
# rare_id_all_foh      <- which(colSums(x = bio2_all_foh[,-1]) < nu_rate_cutoff) + 1
# 
# rare_names_all_spe   <- names(rare_id_all_spe) %>% str_replace("\\.", " ")
# rare_names_all_gen   <- names(rare_id_all_gen) %>% str_replace("\\.", " ")
# rare_names_all_foh   <- names(rare_id_all_foh) %>% str_replace("\\.", " ")
# 
# ls_rare = list(rare_names_all_spe, rare_names_all_gen, rare_names_all_foh)
# saveRDS(ls_rare, file.path(dir_pd, "0x_rare_taxa_list.RDS"))

# bio2_all_spe <- bio2_all_spe[, -rare_id_all_spe]
# bio2_all_gen <- bio2_all_gen[, -rare_id_all_gen]
# bio2_all_foh <- bio2_all_foh[, -rare_id_all_foh]


# -- low richness sites -- #

# Compute number of taxa across levels for each site and output a vector of gr_sample_ids with empty sites
for (i in seq_along(unique(dt_mzb$gr_sample_id))){
        if (i == 1) ch_empty = ch_1 = ch_5 = c()
        ls_loop        = list()
        ls_loop$id     = unique(dt_mzb$gr_sample_id)[i]
        ls_loop$spe    = ls_mzb$spe[gr_sample_id == ls_loop$id]
        ls_loop$gen    = ls_mzb$gen[gr_sample_id == ls_loop$id]
        ls_loop$foh    = ls_mzb$foh[gr_sample_id == ls_loop$id]
        ls_loop$spe = sum(rowSums(ls_loop$spe[,-c(1:2)]))
        ls_loop$gen = sum(rowSums(ls_loop$gen[,-c(1:2)]))
        ls_loop$foh = sum(rowSums(ls_loop$foh[,-c(1:2)]))
        ls_loop$sum = ls_loop$spe + ls_loop$gen + ls_loop$foh
        if (ls_loop$sum == 0) ch_empty = append(ch_empty, ls_loop$id)
        #if (ls_loop$sum == 1) ch_1     = append(ch_1, ls_loop$id)
        #if (ls_loop$sum < 5)  ch_5     = append(ch_5, ls_loop$id)
        print(i)
        rm(ls_loop)
}

saveRDS(object = ch_empty, file = file.path(DIR$pd, "0x_empty_sites.RDS"))

ls_mzb$spe = ls_mzb$spe[!gr_sample_id %in% ch_empty]
ls_mzb$gen = ls_mzb$gen[!gr_sample_id %in% ch_empty]
ls_mzb$foh = ls_mzb$foh[!gr_sample_id %in% ch_empty]

# in the last run (03.07.20) I did not drop sites with low richness. This is artifically increased by me splitting the dataset into different taxon levels.
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

# 07. Extract site list  --------------------------------------------------

# co 25.11.20
# I save them in seperate files for the look up table.  
# sites_bio2_all_spe <- as.character(bio2_all_spe$gr_sample_id)
# sites_bio2_all_gen <- as.character(bio2_all_gen$gr_sample_id)
# sites_bio2_all_foh <- as.character(bio2_all_foh$gr_sample_id)

# in the last run (03.07.20) I did not consider seasonal stuff 
# sites_bio2_spr <- as.character(bio2_spr$gr_sample_id)
# sites_bio2_sum <- as.character(bio2_sum$gr_sample_id)
# sites_bio2_aut <- as.character(bio2_aut$gr_sample_id)
# sites_bio2_win <- as.character(bio2_win$gr_sample_id)

# 08. Save data to file ---------------------------------------------------

saveRDS(ls_mzb, file.path(DIR$pd, "05_sxs_list.RDS"))


# co 25.11.20
# bio2_files = ls()[grepl(x = ls(), pattern = "bio2_")]

# co ??? 
#bio2_files = bio2_files[-1]

# co 25.11.20
# for (i in seq_along(bio2_files)) {
#         save.name = paste0(bio2_files[i],".RDS")
#         save.file = get(bio2_files[i])
#         saveRDS(
#                 object = save.file,
#                 file = file.path(dir_pd, 
#                                  paste0("001_speciesXsites_tables/",
#                               Sys.Date(),
#                               "_",
#                               save.name))
#         )
# } 
