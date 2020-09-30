### ------------------------------------------- ###
### --- Exploratory analysis of seasonality --- ###
### --- Macroinvertebrates -------------------- ###
### ------------------------------------------- ###

# date created: 09.09.20
# date used:    09.09.20  

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
setwd(here())

# load data ----------------------------------------------------------------
data <- readRDS("../001_Community Data/100_Combined/002_invertebrates/003_processed_data/004_2020-07-03_mzb_data1585_low_impact.RDS")
mcp_sub <-c("#d95f02", "#666666", "#5f64ff", "#dcce00")

# carpeting ---------------------------------------------------------------
data[, c("species", "genus", "family", "order") := NULL]
data$season %>% unique
data[season == "Summer", season := "summer"]
data <- data[date >= as.Date("2000-01-01")] 
data <- data[!is.na(season)]
data <- data[!is.na(ls_bd_20)]
data <- data[, rt := ls_bd_20]
data <- data[, ls_bd_20 := NULL]

data[rt %in% c("RT2", "RT3"),   rt := "RT2_3"]
data[rt %in% c("RT4", "RT5"),   rt := "RT4_5"]
data[rt %in% c("RT8", "RT9"),   rt := "RT8_9"]
data[rt %in% c("RT10", "RT11"), rt := "RT10_11"]
data[rt %in% c("RT15", "RT16"), rt := "RT15_16"]
data[rt %in% c("RT15", "RT16"), rt := "RT15_16"]

unique(data$rt)

data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter")) %>% droplevels()

# RT 1 -------------------------------------------------------------------- 

# Interesting case. The western data are good for summer/ autumn and the western data for spring/summer. 

plot_data <- data %>% filter(rt == "RT1") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
# rt1 sub1 
plot_data <- data %>% filter(rt == "RT1") %>% filter (data.set == "mzb_Ecosurv") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
# rt1 sub2 
plot_data <- data %>% filter(rt == "RT1") %>% filter(season != "spring") %>% filter (data.set %in% c("mzb_Naiades", "MZB_LD")) %>% filter(!gr_sample_id %in% c("site_18460_date_05121_mzb_Landau", "site_18459_date_05121_mzb_Landau")) %>%  unique(by = "gr_sample_id") %>% st_as_sf()

(plot_object <- tm_shape(read_osm(plot_data, ext=1.1)) +  tm_rgb() +  tm_shape(plot_data) +  tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates//maps/", Sys.Date(), "_RT1sub1.jpeg"))

# RT 2 + 3 --------------------------------------------------------------------

# Reducing this to the cases inside Germany would be nice

plot_data <- data %>% filter(rt == "RT2_3") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
# RT2 + 3 sub 1 
plot_data <- data %>% filter(rt == "RT2_3") %>% filter(data.set == "MZB_LD") %>% filter(season != "autumn") %>%   unique(by = "gr_sample_id") %>% st_as_sf()
(plot_object <- tm_shape(read_osm(plot_data, ext=1.1)) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT2_3sub.jpeg"))


# RT 4 + 5 --------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT4_5") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
# RT4 + 5 sub 1 
plot_data <- data %>% filter(rt == "RT4_5") %>% filter(data.set %in% c("MZB_LD", "mzb_WISER")) %>% filter(season != "winter") %>%   unique(by = "gr_sample_id") %>% st_as_sf()
(plot_object <- tm_shape(read_osm(plot_data, ext=1.1)) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT4_5sub.jpeg"))

# RT 6 --------------------------------------------------------------------

# No subset created. Too few sites and they are too strongly dispersed. 

plot_data <- data %>% filter(rt == "RT6") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
(plot_object <- tm_shape(read_osm(plot_data, ext=1.1)) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT6all.jpeg"))

# RT 7 ---------------------------------------------------------------------

# no observations  

# RT 8 + 9 --------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT8_9") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

# RT 8+ 9 sub 1 
plot_data <- data %>% filter(rt == "RT8_9") %>% filter(!data.set %in% c("leonard_sandin", "Picos_Pepe")) %>% filter(season != "winter") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
(plot_object <- tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("RIGHT", "top")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT8_9sub.jpeg"))


# RT 10 + 11 --------------------------------------------------------------------

# works without subset.  

plot_data <- data %>% filter(rt == "RT10_11") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
(plot_object <- tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("RIGHT", "top")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT10_11all.jpeg"))



# RT 12 --------------------------------------------------------------------


plot_data <- data %>%  filter(rt == "RT12") %>%  unique(by = "gr_sample_id") %>%  st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
(plot_object <- tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("right", "top")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT12all.jpeg"))


# RT 13 --------------------------------------------------------------------

# only one point. 

plot_data <- data %>% filter(rt == "RT13") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

# RT 14 --------------------------------------------------------------------

# Not good -> no subsets created 

plot_data <- data %>% filter(rt == "RT14") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
#subset 14 1
plot_data <- data %>% filter(rt == "RT14") %>% filter(!data.set %in% c("leonard_sandin")) %>%   unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
(plot_object <- tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left", "top")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT14sub.jpeg"))

# RT 15 + 16 --------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT15_16") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT15_16all.jpeg"))
#subset 
plot_data <- data %>% filter(rt == "RT15_16") %>% filter(data.set == "mzb_Naiades") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
crop_box  <- st_bbox(obj = filter(plot_data, gr_sample_id %in% c("site_00719_date_00300_mzb_Naiades", "site_00823_date_00089_mzb_Naiades", "site_01192_date_00069_mzb_Naiades", "site_00142_date_00092_mzb_Naiades")))
plot_data <- st_crop(plot_data, crop_box)

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("LEFT","BOTTOM"), legend.outside=TRUE))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT15_16sub.jpeg"))

rm(plot_data, crop_box, osm);gc()
# RT 17 --------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT17") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT17all.jpeg"))
#subset 
plot_data <- data %>% filter(rt == "RT17") %>% filter(season != "winter") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
crop_box  <- st_bbox(obj = filter(plot_data, gr_sample_id %in% c("site_01624_date_00069_mzb_Naiades", "site_01618_date_00563_mzb_Naiades", "site_00962_date_00581_mzb_Naiades", "site_00863_date_00046_mzb_Naiades")))
plot_data <- st_crop(plot_data, crop_box)

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("LEFT","BOTTOM")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT17sub.jpeg"))

rm(plot_data, plot_object,crop_box, osm);gc()

# RT 18 --------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT18") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
(plot_object <- tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left", "top")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT18all.jpeg"))

#subset 18 1
plot_data <- data %>% filter(rt == "RT18") %>% filter(!season %in% c("spring", "winter")) %>% unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
(plot_object <- tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left", "top")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT18sub.jpeg"))

rm(plot_data, plot_object, osm);gc()
# RT 19 -------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT19") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
(plot_object <- tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left", "top")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT19all.jpeg"))

#subset 19 1
plot_data <- data %>% filter(rt == "RT19") %>% filter(!season %in% c("spring", "winter")) %>% unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext=1.1)
(plot_object <- tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left", "top")))
tmap_save(plot_object, filename = paste0("004_plots/invertebrates/maps/", Sys.Date(), "_RT19sub.jpeg"))



## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 



# old  --------------------------------------------------------------------

## -- number of samples in each season -- ## 
# rt_var <- unique(data$rt)
# for (i in seq_along(rt_var)) {
#         if (i == 1) {
#                 loop_df <- data.table(RT = character(length(rt_var)))
#         }
#         
#         data %>%
#                 filter(rt == rt_var[i]) %>%
#                 unique(by = "gr_sample_id") %>% 
#                 pull(season) %>%
#                 as.character() %>%
#                 table ->
#                 seasons
#         
#         
#         
#         loop_df[i, c("RT",
#                      "n_events",
#                      "n_sites",
#                      "n_spring",
#                      "n_summer",
#                      "n_autumn",
#                      "n_winter") :=
#                         .(
#                                 rt_var[i],
#                                 data %>%
#                                         filter(rt == rt_var[i]) %>%
#                                         pull(gr_sample_id) %>%
#                                         unique() %>%
#                                         length,
#                                 data %>%
#                                         filter(rt == rt_var[i]) %>%
#                                         pull(site_id) %>%
#                                         unique() %>%
#                                         length,
#                                 seasons[2],
#                                 seasons[3],
#                                 seasons[1],
#                                 seasons[4]
#                         ), ]
#         
# 
#         
# }
# loop_df
# rm(loop_df, i);gc()


