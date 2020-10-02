# -------------------------------------- #
### --- Add cluster ids to diatoms --- ###
# -------------------------------------- #

#date: 04.08.20
#date used: 04.08.20, 05.08., 18.08
#Jonathan Jupke
# Diatoms GetReal WP 2 

# Setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, data.table, magrittr, here, purrr)
setwd(here("003_processed_data/"))

# load data -------------------------------------------------------------------
all_dia_spe <- readRDS("008_2020-08-18_spe_no_rare_20.RDS")
all_dia_gen <- readRDS("008_2020-08-18_gen_no_rare_20.RDS")
# all_dia_spr <- readRDS("001_speciesXsites_tables/2020-08-05_spe_no_rare_20")
# all_dia_ger <- readRDS("001_speciesXsites_tables/2020-08-05_gen_no_rare_20")

sites   <- readRDS("004_2020-06-30_dia_sites_low_ls_all.RDS")
ls_bd20 <- st_read("../../../../../../My Papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/m_river_fec_broad_type.shp")

# clean data -------------------------------------------------------------------
setDT(all_dia_spe)
setDT(all_dia_gen)
# setDT(all_dia_spr)
# setDT(all_dia_ger)

# set gr_sample id to character
all_dia_spe[, gr_sample_id := as.character(gr_sample_id)]
all_dia_gen[, gr_sample_id := as.character(gr_sample_id)]
# all_dia_spr[, gr_sample_id := as.character(gr_sample_id)]
# all_dia_ger[, gr_sample_id := as.character(gr_sample_id)]

length(unique(all_dia_spe$gr_sample_id))
length(unique(all_dia_gen$gr_sample_id))
# length(unique(all_dia_spr$gr_sample_id))
# length(unique(all_dia_ger$gr_sample_id))
length(unique(sites$gr_sample_id))

# add ls_btype20 to sites -------------------------------------------------
if (!st_crs(ls_bd20) == st_crs(sites))
        sites <- st_transform(x = sites, crs = st_crs(ls_bd20))


nn = st_nearest_feature(sites , ls_bd20); beepr::beep()
rivers_resorted <- ls_bd20[nn,]
distance_list <-
        map(.x = 1:nrow(sites),
            .f = ~ as.numeric(st_distance(x = sites[.x, ],
                                          y = rivers_resorted[.x, ])));beepr::beep()
distance_list2 <- unlist(distance_list)
distance_table <- data.table("gr_sample_id" = sites$gr_sample_id,
                             "nn_distance" = distance_list2,
                             "ls_bd_20" = rivers_resorted$m_btype20c)

distance_table2 <- distance_table[nn_distance <= 100]

# add stream type to sites object 
sites2 <- left_join(distance_table2, 
                    sites)

sites2[, c("nn_distance", "geom") := NULL]
# join data -------------------------------------------------------------------
join_spe <- sites2[all_dia_spe, on = "gr_sample_id"]
join_gen <- sites2[all_dia_gen, on = "gr_sample_id"]
# join_spr <- sites2[all_dia_spr, on = "gr_sample_id"]
# join_ger <- sites2[all_dia_ger, on = "gr_sample_id"]
# drop sites without RT. They are too far away from any Lyche Solheim rivers. 
join_spe <- join_spe[!is.na(ls_bd_20)]
join_gen <- join_gen[!is.na(ls_bd_20)]
# join_spr <- join_spr[!is.na(ls_bd_20)]
# join_ger <- join_ger[!is.na(ls_bd_20)]

## --- quality check --- ## 
if (sum(is.na(join_spe$ls_bd_20)) == 0 &
    sum(is.na(join_gen$ls_bd_20)) == 0 #&
    # sum(is.na(join_spr$ls_bd_20)) == 0 &
    # sum(is.na(join_ger$ls_bd_20)) == 0
    ) print ("Quality controll passed") else print ("Quality controll failed")
## --------------------- ##

# save to file  -------------------------------------------------------------------
saveRDS(object = join_spe,  file = paste0("009_", Sys.Date(), "diatom_species_no_rare_w_LSRT.RDS"))
saveRDS(object = join_gen,  file = paste0("009_", Sys.Date(), "diatom_genus_no_rare_w_LSRT.RDS"))
# saveRDS(object = join_spr,  file = paste0("010_", Sys.Date(), "diatom_species_wo_rare_w_LSRT.RDS.RDS"))
# saveRDS(object = join_ger,  file = paste0("010_", Sys.Date(), "diatom_genus_wo_rare_w_LSRT.RDS.RDS"))

