### ------------------------------------------- ###
### --- Exploratory analysis of seasonality --- ###
### --- Macroinvertebrates -------------------- ###
### ------------------------------------------- ###

# date created: 09.09.20
# date used:    09.09.20  
# date altered: 09.11.20 

# Try to identify the optimal stream types to analyze seasonality. 

# setup -------------------------------------------------------------------
pacman::p_load(data.table,
               dplyr,
               here,
               magrittr,
               OpenStreetMap,
               sf,
               stringr ,
               tmaptools,
               tmap)

dir_pd = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/")
dir_plot = here("002_working_package_02/003_seasonality/004_plots/invertebrates/maps/")

# -- FUNCTIONS 
make_plot = function(x){
        tm_shape(osm) + tm_rgb() + tm_shape(x) + tm_dots(
                col = "season",
                size = 1,
                shape = 21,
                palette = ch_col
        ) +  tm_layout(
                legend.bg.color = "white",
                frame = F,
                legend.frame = T,
                legend.position = c("left", "top")
        )
}
make_plot(plot_data)

# load data ----------------------------------------------------------------
dt_data   = readRDS(file.path(dir_pd, "004_2020-11-03_mzb_data1585_low_impact.RDS"))

ch_col =c("#d95f02", "#666666", "#5f64ff", "#dcce00")

# carpeting ---------------------------------------------------------------
dt_data[, c("species", "genus", "family", "order") := NULL]
dt_data$season %>% unique
dt_data[season == "Summer", season := "summer"]
dt_data <- dt_data[date >= as.Date("2000-01-01")] 
dt_data <- dt_data[!is.na(season)]
dt_data <- dt_data[!is.na(ls_bd_20)]
dt_data <- dt_data[, rt := ls_bd_20]
dt_data <- dt_data[, ls_bd_20 := NULL]

dt_data[rt %in% c("RT8", "RT9", "RT10", "RT11", "RT15", "RT16"),   rt := "RT8-11_15_16"]
dt_data[rt %in% c("RT4", "RT5"), rt := "RT4_5"]
dt_data[rt %in% c("RT2", "RT3"), rt := "RT2_3"]

unique(dt_data$rt)

dt_data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter")) %>% droplevels()

# RT 1 -------------------------------------------------------------------- 

# Interesting case. The western data are good for summer/ autumn and the western data for spring/summer. 

plot_data <-
        dt_data %>% 
        filter(rt == "RT1") %>%  
        unique(by = "gr_sample_id") %>% 
        st_as_sf()

osm = read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT1.jpeg")))

## -- RT 1 SUBSET 1 -- ##
plot_data <-
        dt_data %>%
        filter(rt == "RT1") %>%
        filter (data.set == "mzb_Ecosurv") %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT1sub1.jpeg")))
## -- RT1 SUBSET 2 -- ##
plot_data <-
        dt_data %>%
        filter(rt == "RT1") %>%
        filter(season != "spring") %>%
        filter(data.set %in% c("mzb_Naiades", "MZB_LD")) %>%
        filter(!gr_sample_id %in% c(
                        "site_18460_date_05121_mzb_Landau",
                        "site_18459_date_05121_mzb_Landau"
                )) %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT1sub2.jpeg")))
rm(plot_data, plot_object, osm)
gc()

# RT 2 + 3 --------------------------------------------------------------------

# Reducing this to the cases inside Germany would be nice

plot_data <- dt_data %>%
        filter(rt == "RT2_3") %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT2_3.jpeg")))
## RT2_3 SUBSET -- ## 
plot_data <-
        dt_data %>% 
        filter(rt == "RT2_3") %>% 
        filter(data.set == "MZB_LD") %>% 
        filter(season != "autumn") %>%  
        unique(by = "gr_sample_id") %>% 
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT2_3_sub.jpeg")))
rm(plot_data, plot_object, osm)
gc()
# RT 4 + 5 --------------------------------------------------------------------
plot_data <- 
        dt_data %>% 
        filter(rt == "RT4_5") %>%  
        unique(by = "gr_sample_id") %>% 
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT4_5.jpeg")))

## -- RT 4_5 SUBSET -- ## 
plot_data <- 
        dt_data %>% 
        filter(rt == "RT4_5") %>% 
        filter(data.set %in% c("MZB_LD", "mzb_WISER")) %>% 
        filter(season != "winter") %>%   
        unique(by = "gr_sample_id") %>% 
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT4_5_sub.jpeg")))
rm(plot_data, plot_object, osm)
gc()
# RT 6 --------------------------------------------------------------------
# No subset created. Too few sites and they are too strongly dispersed. 
plot_data <- 
        dt_data %>% 
        filter(rt == "RT6") %>%  
        unique(by = "gr_sample_id") %>% 
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT6.jpeg")))
rm(plot_data, plot_object, osm)
gc()

# RT 7 ---------------------------------------------------------------------

# no observations  

# RT 8 - 11 + 15 + 16--------------------------------------------------------------------

plot_data <- 
        dt_data %>% 
        filter(rt == "RT8-11_15_16") %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT8-11_15_16.jpeg")))
rm(plot_data, plot_object, osm)
gc()
# RT 8+ 9 sub 1 
plot_data <- 
        dt_data %>% 
        filter(rt == "RT8-11_15_16") %>% 
        filter(!data.set %in% c("leonard_sandin", "Picos_Pepe")) %>% 
        filter(season != "winter") %>%  
        unique(by = "gr_sample_id") %>% 
        st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), "_RT8-11_15_16_sub.jpeg")))
rm(plot_data, plot_object, osm)
gc()

# RT 12 --------------------------------------------------------------------
ch_rt = 12
plot_data <- 
        dt_data %>% 
        filter(rt == paste0("RT",ch_rt)) %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,".jpeg"))))
rm(plot_data, plot_object, osm, ch_rt)
gc()

# RT 13 --------------------------------------------------------------------

# only one point. 

plot_data <-
        dt_data %>% 
        filter(rt == "RT13") %>%  
        unique(by = "gr_sample_id") %>% 
        st_as_sf()

# RT 14 --------------------------------------------------------------------
# No subsets created 
ch_rt = 14
plot_data <- 
        dt_data %>% 
        filter(rt == paste0("RT",ch_rt)) %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,".jpeg"))))
rm(plot_data, plot_object, osm, ch_rt)
gc()

# RT 17 --------------------------------------------------------------------
ch_rt = 17
plot_data <- 
        dt_data %>% 
        filter(rt == paste0("RT",ch_rt)) %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,".jpeg"))))
rm(plot_data, plot_object, osm)
gc()
## -- SUBSET 17 -- ##  
plot_data <- dt_data %>%
        filter(rt == paste0("RT",ch_rt)) %>%
        filter(season != "winter") %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf()
crop_box  = st_bbox(obj = filter(plot_data, gr_sample_id %in% c("site_01624_date_00069_mzb_Naiades", "site_01618_date_00563_mzb_Naiades", "site_00962_date_00581_mzb_Naiades", "site_00863_date_00046_mzb_Naiades")))
plot_data = st_crop(plot_data, crop_box)

osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,"_sub.jpeg"))))
rm(plot_data, plot_object, osm)
gc()
# RT 18 --------------------------------------------------------------------
ch_rt = 18
plot_data <- 
        dt_data %>% 
        filter(rt == paste0("RT",ch_rt)) %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,".jpeg"))))
rm(plot_data, plot_object, osm)
gc()
### --- RT18 SUBSET --- ### 
plot_data <- dt_data %>% 
        filter(rt == paste0("RT",ch_rt)) %>% 
        filter(!season %in% c("spring", "winter")) %>% 
        unique(by = "gr_sample_id") %>%
        st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,"_sub.jpeg"))))
rm(plot_data, plot_object, osm)
gc()
rm(plot_data, plot_object, osm, ch_rt);gc()
# RT 19 -------------------------------------------------------------------
ch_rt = 19
plot_data <- 
        dt_data %>% 
        filter(rt == paste0("RT",ch_rt)) %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,".jpeg"))))
rm(plot_data, plot_object, osm)
gc()
### --- RT19 SUBSET --- ### 
plot_data <- dt_data %>% 
        filter(rt == paste0("RT",ch_rt)) %>%
        filter(!season %in% c("spring", "winter")) %>% 
        unique(by = "gr_sample_id") %>% 
        st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
plot_object = make_plot(plot_data)
tmap_save(plot_object, filename = file.path(dir_plot, paste0(Sys.Date(), paste0("_RT",ch_rt,"_sub.jpeg"))))
rm(plot_data, plot_object, osm)
gc()
## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 



