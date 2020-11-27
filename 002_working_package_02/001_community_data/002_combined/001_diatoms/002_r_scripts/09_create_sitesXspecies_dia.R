### ------------------------------------- ###
### --- Create species X sites tables --- ### 
### --- Diatoms                       --- ### 
### ------------------------------------- ###

# date written/modified: 03.08.20 + 14.09 + 15.11.
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


DIR = list(
        pd = here(
                "002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/"
        ),
        rs = here(
                "002_working_package_02/001_community_data/002_combined/001_diatoms/002_r_scripts/"
        )
)

# read in data ------------------------------------------------
dt_dia  = readRDS(file.path(DIR$pd, "007_2020-11-04all_dia_optimal_taxon_75.RDS"))
dt_rt   = readRDS(file.path(DIR$pd, "08_rt_gr_id.RDS"))

# Prepare data -------------------------------------------------------------------------
dt_dia[, c("final_taxon", "gr_sample_id") := .(
        str_trim(final_taxon, side = "both"),
        as.character(gr_sample_id)
        )]

# 02. Subset  --------------------------------------------------------------
dt_dia = dt_dia[(is.na(year) | year >= 2000)]

## -- Quality Check -- ## 
unique(dt_dia$season)
## --               -- ##


# 03. Drop columns --------------------------------------------------------

dt_dia =  dt_dia[,.(gr_sample_id, final_taxon, final_taxon_level)]
dt_dia = dt_rt[dt_dia, on = "gr_sample_id"]
dt_dia = dt_dia[!is.na(ls_bd_20)]

# in the last run (18.08.20) I did not consider seasonal stuff 
# bio2_spr <-      sub_spring[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_sum <-      sub_summer[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_aut <-      sub_autumn[,.(gr_sample_id, final_taxon, final_taxon_level)]
# bio2_win <-      sub_winter[,.(gr_sample_id, final_taxon, final_taxon_level)]


## -- different levels 
ls_dia = list(
        spe = dt_dia[final_taxon_level == "species"], 
        gen = dt_dia[final_taxon_level == "genus"]
)

## -- Quality Check -- ## 
if ((nrow(ls_dia$spe) + nrow(ls_dia$gen)) == nrow(dt_dia)) print("Quality check passed") else print("Quality check failed")
## --               -- ##

ls_dia %<>% lapply(function(x)x[,final_taxon_level := NULL])

ls_rt = list(
        spe = unique(ls_dia$spe[,.(gr_sample_id, ls_bd_20)], by = "gr_sample_id"),
        gen = unique(ls_dia$gen[,.(gr_sample_id, ls_bd_20)], by = "gr_sample_id")
)

# 04. Turn to site X species matrix --------------------------------------------------------

ls_dia %<>% lapply(function(x)splist2presabs(x,sites.col = 1, sp.col = 4))

ls_dia$spe = ls_rt$spe[ls_dia$spe, on = "gr_sample_id"]
ls_dia$gen = ls_rt$gen[ls_dia$gen, on = "gr_sample_id"]
                  
# 05. remove rare taxa --------------------------------------------------------
# i.e. < 5 taxa per site or < 5 occurrences of a taxon in the whole dataset 
# all  
# -- rare taxa -- #
nu_rate_cutoff = dt_dia[, uniqueN(gr_sample_id), by = ls_bd_20]
nu_rate_cutoff[, one_percent :=  round(V1 * 0.05,0)]

# stream types with less than 25 samples 
nu_rate_cutoff = nu_rate_cutoff[V1 > 25]

ls_dia %<>% lapply(function(x)x[ls_bd_20 %in% nu_rate_cutoff$ls_bd_20])

# -- rare taxa -- #
ls_rare = vector("list", nrow(nu_rate_cutoff))

for (i in 1:nrow(nu_rate_cutoff)){
        ls_loop        = list()
        ls_loop$rt     = nu_rate_cutoff$ls_bd_20[i]
        ls_loop$cutoff = nu_rate_cutoff$one_percent[i]
        ls_loop$spe    = ls_dia$spe[ls_bd_20 == ls_loop$rt]
        ls_loop$gen    = ls_dia$gen[ls_bd_20 == ls_loop$rt]
        ls_loop$spe_sum = colSums(x = ls_loop$spe[,-(1:2)])
        ls_loop$gen_sum = colSums(x = ls_loop$gen[,-(1:2)])
        ls_loop$spe_names = names(ls_loop$spe_sum)[ls_loop$spe_sum < ls_loop$cutoff]
        ls_loop$gen_names = names(ls_loop$gen_sum)[ls_loop$gen_sum < ls_loop$cutoff]
        if (sum(ls_dia$spe[ls_bd_20 == ls_loop$rt, lapply(.SD, FUN = sum), .SDcols = ls_loop$spe_names]) != 0){
                for (k in seq_along(ls_loop$spe_names)){
                        ls_dia$spe[ls_bd_20 == ls_loop$rt, ls_loop$spe_names[k] := 0]
                }
        }
        if (sum(ls_loop$gen[, lapply(.SD, FUN = sum), .SDcols = ls_loop$gen_names]) != 0){
                for (k in seq_along(ls_loop$gen_names)){
                        ls_dia$gen[ls_bd_20 == ls_loop$rt, ls_loop$gen_names[k] := 0]
                }
        }
        
        ls_rare[[i]]$spe = ls_loop$spe_names
        ls_rare[[i]]$gen = ls_loop$gen_names
        names(ls_rare)[i] = ls_loop$rt
        
        print(i)
        rm(ls_loop,i);gc()
}

# saveRDS(ls_rare, file.path(DIR$pd, "0x_rare_taxa_list.RDS"))

rm(nu_rate_cutoff)


# ## --- old rare CO 25.11.20 
# dat_s_id   = which(colSums(x = dat_all_s[,-1]) < nu_rate_cutoff) + 1
# dat_g_id   = which(colSums(x = dat_all_g[,-1]) < nu_rate_cutoff) + 1
# ch_rare_species = names(dat_s_id) %>% str_replace("\\.", " ")
# ch_rare_genera = names(dat_g_id) %>% str_replace("\\.", " ")
# 
# ls_rare = list(ch_rare_species, ch_rare_genera)
# saveRDS(ls_rare, file.path(dir_pd, "008_a_rare_names.RDS"))
# 
# dat_all_s = dat_all_s[, -dat_s_id]
# dat_all_g = dat_all_g[, -dat_g_id]
# 
# empty_site_id_s        <- which(rowSums(x = dat_all_s[,-1]) == 0)
# empty_site_id_g        <- which(rowSums(x = dat_all_g[,-1]) == 0)
# 
# if (length(empty_site_id_s) != 0)  dat_all_s = dat_all_s[- empty_site_id_s,]
# if (length(empty_site_id_g) != 0)  dat_all_g = dat_all_g[- empty_site_id_g,]
# 
# rm(list = setdiff(ls(), c("dat_all_s", "dat_all_g", "dir_pd")));gc()


# Remove empty sites ------------------------------------------------------

# Compute number of taxa across levels for each site and output a vector of gr_sample_ids with empty sites
for (i in seq_along(unique(dt_dia$gr_sample_id))){
        if (i == 1) ch_empty =  c()
        ls_loop        = list()
        ls_loop$id     = unique(dt_dia$gr_sample_id)[i]
        ls_loop$spe    = ls_dia$spe[gr_sample_id == ls_loop$id]
        ls_loop$gen    = ls_dia$gen[gr_sample_id == ls_loop$id]
        ls_loop$spe = sum(rowSums(ls_loop$spe[,-c(1:2)]))
        ls_loop$gen = sum(rowSums(ls_loop$gen[,-c(1:2)]))
        ls_loop$sum = ls_loop$spe + ls_loop$gen
        if (ls_loop$sum == 0) ch_empty = append(ch_empty, ls_loop$id)

        print(i)
        rm(ls_loop)
}

saveRDS(object = ch_empty, file = file.path(DIR$pd, "0x_empty_sites.RDS"))

ls_dia %<>% lapply(function(x)x[!gr_sample_id %in% ch_empty])

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

saveRDS(ls_dia, file.path(DIR$pd, "08_sxs.RDS"))


