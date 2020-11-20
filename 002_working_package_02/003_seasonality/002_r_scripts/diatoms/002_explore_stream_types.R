### ------------------------------------------- ###
### --- Exploratory analysis of seasonality --- ###
### --- Diatoms --------------------------- --- ###
### ------------------------------------------- ###

# date created: 01.09.20
# date used:    01.09.20 + 2. + 14. + 15. + 09.11

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
tmap_mode("view")
DIR = list(re = here("002_working_package_02/003_seasonality/003_results/diatoms/"),
           plot = here("002_working_package_02/003_seasonality/004_plots/diatoms/")
           )

# load data ----------------------------------------------------------------
dt_data   = readRDS(file.path(DIR$re, "001_2020-11-09_diatoms_w_rt.RDS"))
ch_col = c("#d95f02", "#666666", "#5f64ff", "#dcce00")

# carpeting ---------------------------------------------------------------
dt_data[, c("original_name", "species", "genus", "family", "order", "name_source") := NULL]
unique(dt_data$season) 
dt_data <- dt_data[!is.na(rt) & !is.na(season) & !is.na(geometry)]
unique(dt_data$rt)

dt_data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter")) %>% droplevels()

dt_data <- dt_data[date >= as.Date("2000-01-01") | is.na(date)] 

ch_medium_types <- paste0("RT", c(1, 2, 3, 4, 5, 6, 8, 9, 12, 16, 17, 18, 19))
dt_data   <- dt_data[rt %in% ch_medium_types]
unique(dt_data$rt)

dt_data[rt %in% c("RT1", "RT17", "RT18", "RT19"), rt := "RT1_17_18_19"]
dt_data[rt %in% c("RT2", "RT4"), rt := "RT2_4"]
dt_data[rt %in% c("RT8", "RT9"), rt := "RT8_9"]
dt_data[rt %in% c("RT8_9", "RT2_4"), rt := "RT2_4_8_9"]
dt_data[rt %in% c("RT2_4_8_9", "RT1_17_18_19"), rt := "RT1_2_4_8_9_17_18_19"]

# RT1_2_4_8_9_17_18_19 -------------------------------------------------------------------- 

plot_data =
  dt_data %>% 
  filter(rt == "RT1_2_4_8_9_17_18_19") %>%  
  unique(by = "gr_sample_id") %>% 
  st_as_sf()
osm = read_osm(plot_data, ext = 1.1)
(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
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
)
tmap_save(plot_object, filename = file.path(
  DIR$plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT1_2_4_8_9_17_18_19.jpeg"
  )
))

# RT1_2_4_8_9_17_18_19 subset 1 -------------------------------------------

plot_data <- 
  dt_data %>%
  filter(rt == "RT1_2_4_8_9_17_18_19") %>% 
  filter(!season %in% c("winter", "spring")) %>% 
  unique(by = "gr_sample_id") %>% 
  st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 1,
      palette = ch_col  
    ) +  tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)
tmap_save(plot_object, filename = file.path(
  DIR$plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT_RT1_2_4_8_9_17_18_19_sub1.jpeg"
  )
))

rm(plot_data, plot_object, osm)
gc()

# RT1_2_4_8_9_17_18_19 subset 2-------------------------------------------

plot_data <- 
  dt_data %>%
  filter(rt == "RT1_2_4_8_9_17_18_19") %>% 
  filter(!season %in% c("winter")) %>% 
  unique(by = "gr_sample_id") %>% 
  st_as_sf()
plot_bbox = filter(plot_data, gr_sample_id %in% c("site_03147_date_00137_DIA_edwin_peters", 
                                                  "site_00332_date_00004_dia_Saul_Blanco",
                                                  "site_00136_date_00252_dia_MiguelIglesias",
                                                  "site_00102_date_00038_dia_Ecosurv")
)
plot_bbox %<>% st_bbox() 
plot_data %<>% st_crop(plot_bbox)
osm <- read_osm(plot_data, ext = 1.1)
(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 1,
      palette = ch_col  
    ) +  tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)
tmap_save(plot_object, filename = file.path(
  DIR$plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT_RT1_2_4_8_9_17_18_19_sub2.jpeg"
  )
))

rm(plot_data, plot_object, osm)
gc()

# RT 3 --------------------------------------------------------------------

# No areas with sampling sites from different seasons. 
# All areas are far apart.   
# -> NO SUBSET 

plot_data <- 
  dt_data %>% 
  filter(rt == "RT3") %>%  
  unique(by = "gr_sample_id") %>%
  st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 21,
      palette = ch_col
    ) + tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)
tmap_save(plot_object, filename = file.path(
  DIR$plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT3.jpeg"
  )
))

rm(plot_data, plot_object, osm);gc()

# RT 5 --------------------------------------------------------------------
plot_data <- 
  dt_data %>% 
  filter(rt == "RT5") %>%  
  unique(by = "gr_sample_id") %>%
  st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(
plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 21,
      #palette = ch_col
    ) + tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)
tmap_save(plot_object, filename = file.path(
  DIR$plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT5.jpeg"
  )
))

rm(plot_data, plot_object, osm);gc()
### ---- subset RT 5 
plot_data <- 
  dt_data %>% 
  filter(rt == "RT5") %>%  
  filter(data.set %in% c("dia_Naiades", "IRSTEA")) %>% 
  unique(by = "gr_sample_id") %>%
  st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 21,
      #palette = ch_col
    ) + tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)
tmap_save(plot_object, filename = file.path(
  DIR$plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT5_sub1.jpeg"
  )
))

rm(plot_data, plot_object, osm);gc()


# RT 6 --------------------------------------------------------------------

# Only four sites 
# -> NO SUBSET 
plot_data <- dt_data %>% 
  filter(rt == "RT6") %>%  
  unique(by = "gr_sample_id") %>% 
  st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 21,
      palette = ch_col
    ) + tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)
tmap_save(plot_object, filename = file.path(
  DIR$plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT6.jpeg"
  )
))
rm(plot_data, plot_object, osm);gc()

# RT12 --------------------------------------------------------------------
# only one site 

plot_data <- dt_data %>% 
  filter(rt == "RT12") %>%  
  unique(by = "gr_sample_id") %>% 
  st_as_sf()


# RT 16 --------------------------------------------------------------------
plot_data <-
  dt_data %>% 
  filter(rt == "RT16") %>% 
  unique(by = "gr_sample_id") %>% 
  st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)

(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 21,
      palette = ch_col
    ) + tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)
tmap_save(plot_object, filename = file.path(
  DIR$plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT16.jpeg"
  )
))

### ---- SUBSET1 
plot_data <-
  dt_data %>% 
  filter(rt == "RT16") %>% 
  filter(!season %in% c("spring", "autumn")) %>% 
  unique(by = "gr_sample_id") %>% 
  st_as_sf()


plot_bbox = filter(plot_data, gr_sample_id %in% c("site_00793_date_00984_dia_Naiades", 
                                                  "site_00855_date_00988_dia_Naiades",
                                                  "site_00725_date_00993_dia_Naiades",
                                                  "site_00738_date_00983_dia_Naiades")
)
plot_bbox %<>% st_bbox() 
plot_data %<>% st_crop(plot_bbox)
osm <- read_osm(plot_data, ext = 1.1)
(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 21,
      palette = ch_col
    ) + tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)
tmap_save(plot_object, filename = file.path(
  DIR$plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT16_sub1.jpeg"
  )
))
### ---- SUBSET2
plot_data <-
  dt_data %>% 
  filter(rt == "RT16") %>% 
  filter(!season %in% c("winter", "spring")) %>% 
  unique(by = "gr_sample_id") %>% 
  st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 21,
      palette = ch_col
    ) + tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)
tmap_save(plot_object, filename = file.path(
  DIR$plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT16_sub2.jpeg"
  )
))
