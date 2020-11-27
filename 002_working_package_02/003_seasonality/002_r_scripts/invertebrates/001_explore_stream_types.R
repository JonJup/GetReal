### ------------------------------------------- ###
### --- Exploratory analysis of seasonality --- ###
### --- Macroinvertebrates -------------------- ###
### ------------------------------------------- ###

# date created: 09.09.20
# date used:    09.09.20  
# date altered: 09.11.20 

# Try to identify the optimal stream types to analyze seasonality. 

# SETUP -------------------------------------------------------------------
pacman::p_load(data.table,
               dplyr,
               here,
               magrittr,
               OpenStreetMap,
               sf,
               stringr ,
               tmaptools,
               tmap)

DIR = list(pd = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"),
           plot = here("002_working_package_02/003_seasonality/004_plots/invertebrates/maps/"),
           re = here("002_working_package_02/003_seasonality/003_results/invertebrates/"))

# FUNCTIONS ---------------------------------------------------------------
prepare_plot = function(x) {
        plot_data =
                dt_data %>%
                filter(rt == x) %>%
                unique(by = "gr_sample_id") %>%
                st_as_sf()
        
        osm = read_osm(plot_data, ext = 1.1)
        return(list(data = plot_data, osm = osm))
}
plot_list = function(x){
        out =  tm_shape(x$osm) + tm_rgb() + tm_shape(x$data) + tm_dots(
                col = "season",size = 1,
                shape = 21,
                palette = ch_col
        ) +  tm_layout(
                legend.bg.color = "white",
                frame = F,
                legend.frame = T,
                legend.position = c("left", "top")
        )
}

# LOAD ----------------------------------------------------------------
dt_data   = readRDS(file.path(DIR$pd, "004_2020-11-03_mzb_data1585_low_impact.RDS"))
ch_col  = c("#d95f02", "#666666", "#5f64ff", "#dcce00")
dt_sxs  = readRDS(file.path(DIR$pd, "06_sxs_w_LS20.RDS"))
ls_rare = readRDS(file.path(DIR$pd, "0x_rare_taxa_list.RDS"))
ch_empty = readRDS(file.path(DIR$pd, "0x_empty_sites.RDS"))
ch_acc = paste0("RT", c(1:5, 8: 16, 18))

# CARPET ---------------------------------------------------------------
# rename river type variable 
dt_data[,c("rt", "ls_bd_20") := .(ls_bd_20, NULL)]
# drop cols 
dt_data[, c("species", "genus", "family", "order") := NULL]
# drop rows 
dt_data = dt_data[!is.na(rt) & 
                          !is.na(season) & 
                          !is.na(geom) & 
                          (date >= as.Date("2000-01-01") | is.na(date)) & 
                          rt %in% ch_acc &
                          (rt %in% dt_sxs$spe$ls_bd_20 | rt %in% dt_sxs$gen$ls_bd_20) & 
                          !gr_sample_id %in% ch_empty
]

# fix season column 
dt_data[season == "Summer", season := "summer"]
dt_data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter")) %>% droplevels()

ls_loop = list()

for (i in seq_along(ls_rare)) {
        loop_spe = ls_rare[[i]]$spe
        loop_spe %<>% str_replace_all(pattern = "\\.", replacement = "\\ ")
        loop_gen = ls_rare[[i]]$gen
        loop_gen %<>% str_replace_all(pattern = "\\.", replacement = "\\ ")
        loop_foh = ls_rare[[i]]$foh
        loop_foh %<>% str_replace_all(pattern = "\\.", replacement = "\\ ")
        
        loop_rt = names(ls_rare)[i]
        
        ls_loop[[i]] = 
                rbindlist(
                        list(
                                dt_data[rt == loop_rt & 
                                                final_taxon_level == "species" & 
                                                !final_taxon %in% loop_spe, ], 
                                dt_data[rt == loop_rt & 
                                                final_taxon_level == "genus" & 
                                                !final_taxon %in% loop_gen, ],
                                dt_data[rt == loop_rt & 
                                                !final_taxon_level %in% c("species", "genus") & 
                                                !final_taxon %in% loop_foh, ]
                        )
                )
        
        rm(loop_rt, loop_gen, loop_spe, i, loop_foh)
}

dt_data = rbindlist(ls_loop)

rm(dt_sxs, ch_medium_types, ch_empty, ls_rare, ls_loop)
# saveRDS(file = file.path(DIR$re, "base_data_seasons.RDS"), object = dt_data)

# RT1 -------------------------------------------------------------------- 

# Interesting case. The western data are good for summer/ autumn and the western
# data for spring/summer.

type = "RT1"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT1 SUB1 ---------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
        filter (data.set == "mzb_Ecosurv") 
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type)
gc()

# RT1 SUB2 ----------------------------------------------------------------
ls_sub2 = ls_plot
ls_sub2$data %<>% 
        filter(season != "spring") %>%
        filter(data.set %in% c("mzb_Naiades", "MZB_LD")) %>%
        filter(!gr_sample_id %in% c(
                "site_18460_date_05121_mzb_Landau",
                "site_18459_date_05121_mzb_Landau"
        ))
ls_sub2$osm = read_osm(ls_sub2$data, ext = 1.1)
map = plot_list(ls_sub2)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub2.jpeg")))
rm(ls_sub2, ls_plot, type)
gc()


# RT2 ---------------------------------------------------------------------

# Strong seasonal separation. Not useful. 

type = "RT2"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))


# RT3 ---------------------------------------------------------------------

# Strong seasonal separation. Not useful. 

type = "RT3"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT4 ---------------------------------------------------------------------
type = "RT4"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))


# RT4 SUB1 ----------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
        filter (!season %in% "autumn") %>% 
        filter(!data.set %in% c("mzb_Naiades", "Cantabria_Pepe", "mzb_STARS"))
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type)
gc()


# RT5 ---------------------------------------------------------------------

# Strong seasonal separation. Not useful. 

type = "RT5"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT8 ---------------------------------------------------------------------
type = "RT8"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT8 SUB1---------------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
        filter (!season %in% "winter") %>% 
        filter(!data.set %in% c("Cantabria_Pepe", "Picos_Pepe", "rivpacs", "leonard_sandin", 
                                "kaisa-leena_Huttunen"))
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type)
gc()

# RT9 ---------------------------------------------------------------------
type = "RT9"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT9 SUB1---------------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
        filter (!season %in% "winter") %>% 
        filter(!data.set %in% c("Cantabria_Pepe", "Picos_Pepe", "rivpacs", "leonard_sandin", 
                                "kaisa-leena_Huttunen"))
plot_bbox = c("site_15606_date_04616_mzb_Landau", "site_00004_date_00058_mzb_Naiades",
              "site_16753_date_04828_mzb_Landau","site_00052_date_NA_mzb_WISER")
plot_bbox = filter(ls_sub1$data,gr_sample_id %in% plot_bbox) %>%
        st_bbox()
ls_sub1$data %<>% st_crop(plot_bbox)
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type)
gc()

# RT10 ---------------------------------------------------------------------
type = "RT10"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT10 SUB1---------------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
        filter(!data.set %in% c("Cantabria_Pepe", "rivpacs"))
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type)
gc()

# RT11 ---------------------------------------------------------------------
type = "RT11"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT11 SUB1 ---------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
        filter(!data.set %in% c("Cantabria_Pepe", "rivpacs", "Picos_Pepe"))
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type)
gc()

# RT 14 --------------------------------------------------------------------

# Strong seasonal separation. Not useful. 

type = "RT14"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT 16 --------------------------------------------------------------------
type = "RT16"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT16 SUB1 ---------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
        filter(!data.set %in% c("mzb_WISER")) %>%  
        filter(!season %in% c("spring")) 
plot_bbox = c("site_00788_date_00065_mzb_Naiades", "site_00650_date_00510_mzb_Naiades",
              "site_01192_date_00069_mzb_Naiades","site_01193_date_00012_mzb_Naiades")
plot_bbox = filter(ls_sub1$data,gr_sample_id %in% plot_bbox) %>%
        st_bbox()
ls_sub1$data %<>% st_crop(plot_bbox)
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
# RT16 SUB2 ---------------------------------------------------------------
ls_sub2 = ls_plot
ls_sub2$data %<>% 
        filter(!data.set %in% c("mzb_WISER"))
plot_bbox = c("site_00950_date_01080_mzb_Naiades", "site_00862_date_00046_mzb_Naiades",
              "site_00858_date_00978_mzb_Naiades","site_00856_date_00859_mzb_Naiades")
plot_bbox = filter(ls_sub2$data,gr_sample_id %in% plot_bbox) %>%
        st_bbox()
ls_sub2$data %<>% st_crop(plot_bbox)
ls_sub2$osm = read_osm(ls_sub2$data, ext = 1.1)
map = plot_list(ls_sub2)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub2.jpeg")))

# RT 18 --------------------------------------------------------------------
type = "RT18"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT18 SUB1 ---------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
        filter(!season %in% c("spring", "winter")) 
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type)
gc()

## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 


# OLD SUBSETS -------------------------------------------------------------

# RT 2 + 3 --------------------------------------------------------------------

# Reducing this to the cases inside Germany would be nice

# plot_data <- dt_data %>%
#         filter(rt == "RT2_3") %>%
#         unique(by = "gr_sample_id") %>%
#         st_as_sf()
# osm <- read_osm(plot_data, ext = 1.1)
# plot_object = make_plot(plot_data)
# tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT2_3.jpeg")))
# ## RT2_3 SUBSET -- ## 
# plot_data <-
#         dt_data %>% 
#         filter(rt == "RT2_3") %>% 
#         filter(data.set == "MZB_LD") %>% 
#         filter(season != "autumn") %>%  
#         unique(by = "gr_sample_id") %>% 
#         st_as_sf()
# osm <- read_osm(plot_data, ext = 1.1)
# plot_object = make_plot(plot_data)
# tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT2_3_sub.jpeg")))
# rm(plot_data, plot_object, osm)
# gc()
# RT 4 + 5 --------------------------------------------------------------------
# plot_data <- 
#         dt_data %>% 
#         filter(rt == "RT4_5") %>%  
#         unique(by = "gr_sample_id") %>% 
#         st_as_sf()
# osm <- read_osm(plot_data, ext = 1.1)
# plot_object = make_plot(plot_data)
# tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT4_5.jpeg")))
# 
# ## -- RT 4_5 SUBSET -- ## 
# plot_data <- 
#         dt_data %>% 
#         filter(rt == "RT4_5") %>% 
#         filter(data.set %in% c("MZB_LD", "mzb_WISER")) %>% 
#         filter(season != "winter") %>%   
#         unique(by = "gr_sample_id") %>% 
#         st_as_sf()
# osm <- read_osm(plot_data, ext = 1.1)
# plot_object = make_plot(plot_data)
# tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT4_5_sub.jpeg")))
# rm(plot_data, plot_object, osm)
# gc()
# RT 8 - 11 + 15 + 16--------------------------------------------------------------------
# 
# plot_data <- 
#         dt_data %>% 
#         filter(rt == "RT8-11_15_16") %>%
#         unique(by = "gr_sample_id") %>%
#         st_as_sf()
# osm <- read_osm(plot_data, ext = 1.1)
# plot_object = make_plot(plot_data)
# tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT8-11_15_16.jpeg")))
# rm(plot_data, plot_object, osm)
# gc()
# # RT 8+ 9 sub 1 
# plot_data <- 
#         dt_data %>% 
#         filter(rt == "RT8-11_15_16") %>% 
#         filter(!data.set %in% c("leonard_sandin", "Picos_Pepe")) %>% 
#         filter(season != "winter") %>%  
#         unique(by = "gr_sample_id") %>% 
#         st_as_sf()
# osm <- read_osm(plot_data, ext=1.1)
# plot_object = make_plot(plot_data)
# tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT8-11_15_16_sub.jpeg")))
# rm(plot_data, plot_object, osm)
# gc()

# RT17 SUB ----------------------------------------------------------------
# plot_data <- dt_data %>%
#         filter(rt == paste0("RT",ch_rt)) %>%
#         filter(season != "winter") %>%
#         unique(by = "gr_sample_id") %>%
#         st_as_sf()
# crop_box  = st_bbox(obj = filter(plot_data, gr_sample_id %in% c("site_01624_date_00069_mzb_Naiades", "site_01618_date_00563_mzb_Naiades", "site_00962_date_00581_mzb_Naiades", "site_00863_date_00046_mzb_Naiades")))
# plot_data = st_crop(plot_data, crop_box)
# 
# osm <- read_osm(plot_data, ext = 1.1)
# plot_object = make_plot(plot_data)
# tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,"_sub.jpeg"))))
# rm(plot_data, plot_object, osm)
# gc()
# RT 19 -------------------------------------------------------------------
# ch_rt = 19
# plot_data <- 
#         dt_data %>% 
#         filter(rt == paste0("RT",ch_rt)) %>%
#         unique(by = "gr_sample_id") %>%
#         st_as_sf()
# osm <- read_osm(plot_data, ext = 1.1)
# plot_object = make_plot(plot_data)
# tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,".jpeg"))))
# rm(plot_data, plot_object, osm)
# gc()
# ### --- RT19 SUBSET --- ### 
# plot_data <- dt_data %>% 
#         filter(rt == paste0("RT",ch_rt)) %>%
#         filter(!season %in% c("spring", "winter")) %>% 
#         unique(by = "gr_sample_id") %>% 
#         st_as_sf()
# osm <- read_osm(plot_data, ext=1.1)
# plot_object = make_plot(plot_data)
# tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,"_sub.jpeg"))))
# rm(plot_data, plot_object, osm)
# gc()