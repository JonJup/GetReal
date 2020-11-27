# ----------------------------------------- #
### --- Create species X sites tables --- ### 
### --- Macroinvertebrates -------------- ### 
### --- Seasonal changes   -------------- ### 
# ----------------------------------------- #

# date written/modified: 24.08.20
# date used: 24.08.20, 25.08. + 28 + 10.09 + 26.11
# Jonathan Jupke 
# Get Real WP 2
# Seasonal changes? 

# SETUP  --------------------------------------------------------------
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(data.table,
       dplyr,
       fuzzySim,
       here,
       magrittr,
       sf,
       stringr)

## DIRECTORIES
DIR = list(
        mzb = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"),
        mzb2= here("002_working_package_02/003_seasonality/003_results/invertebrates/"),
        re  = here("002_working_package_02/003_seasonality/003_results/invertebrates/003_auxiliary/")
)
# READ ------------------------------------------------
dt_mzb  <- readRDS(file.path(DIR$mzb2, "base_data_seasons.RDS"))

# CARPET  -------------------------------------------------------------
dt_mzb[, c("final_taxon", "gr_sample_id") := .(str_trim(final_taxon, side = "both"),
                                                as.character(gr_sample_id))]
dt_mzb[final_taxon == "Stratiomyiidae", final_taxon := "Stratiomyidae"]
dt_mzb[final_taxon == "Notonectidae", final_taxon_level := "family"]

ch_river_types = unique(dt_mzb$rt)
for (i in seq_along(ch_river_types)){
        if (i == 1) ls_rt = list()    
        ls_rt[[i]] = dt_mzb[rt == ch_river_types[i]]
        names(ls_rt)[i] = ch_river_types[i]
}

ls_rt$RT1_sub1 = ls_rt$RT1[data.set == "mzb_Ecosurv"]
ls_rt$RT1_sub2 = ls_rt$RT1[season != "spring" & 
                           data.set %in% c("mzb_Naiades", "MZB_LD") &
                           !gr_sample_id %in% c(
                                   "site_18460_date_05121_mzb_Landau",
                                   "site_18459_date_05121_mzb_Landau"
                           )] 
ls_rt$RT4_sub1 = ls_rt$RT4[!season %in% "autumn" & 
                                   !data.set %in% c("mzb_Naiades", "Cantabria_Pepe", "mzb_STARS")]
ls_rt$RT8_sub1 = ls_rt$RT8[!season %in% "winter" & 
                                   !data.set %in% c("Cantabria_Pepe", "Picos_Pepe", "rivpacs", 
                                                    "leonard_sandin", "kaisa-leena_Huttunen")]
ls_rt$RT9_sub1 = ls_rt$RT9[!season %in% "winter" & 
                                   !data.set %in% c("Cantabria_Pepe", "Picos_Pepe", "rivpacs", 
                                                    "leonard_sandin",  "kaisa-leena_Huttunen")]
ls_rt$RT10_sub1 = ls_rt$RT10[!data.set %in% c("Cantabria_Pepe", "rivpacs")]
ls_rt$RT11_sub1 = ls_rt$RT11[!data.set %in% c("Cantabria_Pepe", "rivpacs", "Picos_Pepe")]
ls_rt$RT16_sub1 = ls_rt$RT16[!season %in% c("spring") &
                                     !data.set %in% c("mzb_WISER")]
ls_rt$RT16_sub2 = ls_rt$RT16[!data.set %in% c("mzb_WISER")]
ls_rt$RT18_sub1 = ls_rt$RT18[!season %in% c("spring", "winter")]


bbox9 = c("site_15606_date_04616_mzb_Landau", "site_00004_date_00058_mzb_Naiades",
              "site_16753_date_04828_mzb_Landau","site_00052_date_NA_mzb_WISER")
bbox16_1 = c("site_00788_date_00065_mzb_Naiades", "site_00650_date_00510_mzb_Naiades",
              "site_01192_date_00069_mzb_Naiades","site_01193_date_00012_mzb_Naiades")
bbox16_2 = c("site_00950_date_01080_mzb_Naiades", "site_00862_date_00046_mzb_Naiades",
              "site_00858_date_00978_mzb_Naiades","site_00856_date_00859_mzb_Naiades")

bbox9 = filter(ls_rt$RT9_sub1, gr_sample_id %in% bbox9) %>%
        st_as_sf() %>% 
        st_bbox()
bbox16_1 = filter(ls_rt$RT16_sub1, gr_sample_id %in% bbox16_1) %>%
        st_as_sf() %>% 
        st_bbox()
bbox16_2 = filter(ls_rt$RT16_sub2, gr_sample_id %in% bbox16_2) %>%
        st_as_sf() %>% 
        st_bbox()

ls_rt$RT9_sub1   %<>% st_as_sf() %>% st_crop(bbox9)    %>% setDT
ls_rt$RT16_sub1  %<>% st_as_sf() %>% st_crop(bbox16_1) %>% setDT
ls_rt$RT16_sub2  %<>% st_as_sf() %>% st_crop(bbox16_2) %>% setDT

ls_rt %<>% lapply(function(x)x[,.(gr_sample_id, final_taxon, final_taxon_level, season)] )

ls_spe = lapply(ls_rt, function(x)x[final_taxon_level == "species"])
ls_gen = lapply(ls_rt, function(x)x[final_taxon_level == "genus"])
ls_foh = lapply(ls_rt, function(x)x[final_taxon_level == "family"])

# sXs --------------------------------------------------------
for (i in seq_along(ls_rt)) {
        
        ld_spe = ls_spe[[i]]
        ld_gen = ls_gen[[i]]
        ld_foh = ls_foh[[i]]
        
        ld_spe[, final_taxon_level := NULL]
        ld_gen[, final_taxon_level := NULL]
        ld_foh[, final_taxon_level := NULL]
        
        # extra table with id and season
        ldj_spe <- copy(ld_spe)
        ldj_gen <- copy(ld_gen)
        ldj_foh <- copy(ld_foh)
        ldj_spe[, final_taxon  := NULL] 
        ldj_gen[, final_taxon  := NULL]
        ldj_foh[, final_taxon  := NULL]
        ldj_spe %<>% unique(by = "gr_sample_id")
        ldj_gen %<>% unique(by = "gr_sample_id")
        ldj_foh %<>% unique(by = "gr_sample_id")
        
        spe_sxs =  splist2presabs(data = ld_spe, sites.col = 1, sp.col = 2) %>% setDT
        gen_sxs =  splist2presabs(data = ld_gen, sites.col = 1, sp.col = 2) %>% setDT
        foh_sxs =  splist2presabs(data = ld_foh, sites.col = 1, sp.col = 2) %>% setDT
        
        ls_spe[[i]] <- ldj_spe[spe_sxs, on = "gr_sample_id"]
        ls_gen[[i]] <- ldj_gen[gen_sxs, on = "gr_sample_id"]
        ls_foh[[i]] <- ldj_foh[foh_sxs, on = "gr_sample_id"]
        
        rm(ld_spe,ld_gen,ld_foh,
           ldj_spe,i,ldj_gen, ldj_foh,
           spe_sxs,gen_sxs,foh_sxs);gc()
        
}


# RARE --------------------------------------------------------
# i.e. < 5 taxa per site or < 5 occurrences in data set 

# -- rare taxa -- #
# CO 26.11: instead I use an additional B threshold for A criterion 
# ls_rare = readRDS(file.path(dir_mzb, "0x_rare_taxa_list.RDS"))
# 
# for (i in 1:length(files)) {
#         dt_loop = get(files[i])
#         if (str_detect(string = files[i], pattern = "spe")) {
#                 in_loop_id = which(names(dt_loop) %in% ls_rare[[1]])
#                 dt_loop = dt_loop[, -in_loop_id, with = FALSE]
#         } else if (str_detect(string = files[i], pattern = "gen")) {
#                 in_loop_id = which(names(dt_loop) %in% ls_rare[[2]])
#                 dt_loop = dt_loop[, -in_loop_id, with = FALSE]
#         } else if (str_detect(string = files[i], pattern = "foh")) {
#                 in_loop_id = which(names(dt_loop) %in% ls_rare[[3]])
#                 dt_loop = dt_loop[, -in_loop_id, with = FALSE]
#         }
#         assign(x = files[i],
#                value = dt_loop)
#         rm(i)
#         gc()
# }
        
# Remove all entries with only two columns i.e. no taxa 
# for (i in files) {
#         ld <- get(i)
#         if (ncol(ld) < 3){
#                 rm(list = i)
#                 print(paste("removed", i))
#                 id <- which(files == i)
#                 files <- files[-id]
#         } 
#         
# }

# SAVE -------------------------------------------------------

ls_save = list(ls_spe, ls_gen, ls_foh)
saveRDS(file = file.path(DIR$mzb2, "sxs_list.RDS"), ls_save)

# for (i in seq_along(files)) {
#         save.name = paste0(files[i], ".RDS")
#         save.file = get(files[i])
#         saveRDS(object = save.file,
#                 file = file.path(
#                         dir_mzb2,
#                         paste0(
#                                 "001_speciesXsites_tables/",
#                                 Sys.Date(),
#                                 "_",
#                                 save.name
#                         )
#                 ))
# } 
## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 
