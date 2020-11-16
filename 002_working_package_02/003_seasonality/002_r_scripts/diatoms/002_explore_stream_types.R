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

dir_re = here("002_working_package_02/003_seasonality/003_results/diatoms/")
dir_plot = here("002_working_package_02/003_seasonality/004_plots/diatoms/")

# load data ----------------------------------------------------------------
data   =readRDS(file.path(dir_re, "001_2020-11-09_diatoms_w_rt.RDS"))
ch_col = c("#d95f02", "#666666", "#5f64ff", "#dcce00")

# carpeting ---------------------------------------------------------------
data[, c("original_name", "species", "genus", "family", "order", "name_source") := NULL]
data$season %>% unique
data <- data[!is.na(rt) & !is.na(season) & !is.na(geometry)]


data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter")) %>% droplevels()
data <- data[date >= as.Date("2000-01-01")] 

data[rt %in% c("RT1", "RT2", "RT4", "RT5", "RT8", "RT16", "RT17", "RT18", "RT19"), rt := "RT_large"]

data$rt %>% unique

# RT large -------------------------------------------------------------------- 

plot_data <-
  data %>% filter(rt == "RT_large") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
tmap_mode("view")
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
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT_large.jpeg"
  )
))
## -- SUBSET RT LARGE -- ##
plot_data <- 
  data %>%
  filter(rt == "RT_large") %>% 
  filter(!season %in% c("winter")) %>% 
  filter(!data.set %in% c("JJM", "dia_Ecosurv")) %>% 
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
    ) +  tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)
tmap_save(plot_object, filename = file.path(
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT_large_sub.jpeg"
  )
))

rm(plot_data, plot_object, osm)
gc()


# RT 3 --------------------------------------------------------------------

# No areas with sampling sites from different seasons. 
# All areas are far apart.   
# -> NO SUBSET 

plot_data <- data %>% 
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
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT3.jpeg"
  )
))

rm(plot_data, plot_object, osm);gc()

# RT 6 --------------------------------------------------------------------

# Only four sites 
# -> NO SUBSET 

plot_data <- data %>% 
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
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT6.jpeg"
  )
))
rm(plot_data, plot_object, osm);gc()

# RT 9 --------------------------------------------------------------------

# There is a region where a summer autumn subset would be possible - but with
# only three autumn sites and many summer sites.
# -> NO SUBSET 
plot_data <-
  data %>% 
  filter(rt == "RT9") %>% 
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
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT9.jpeg"
  )
))

# RT 10 --------------------------------------------------------------------
plot_data <-
  data %>% 
  filter(rt == "RT10") %>%  
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
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT10.jpeg"
  )
))

## -- SUBSET -- ##
plot_data %<>% 
  filter(season != "spring") %>% 
  filter(data.set != "dia_Miguel_Iglesias")

osm <- read_osm(plot_data, ext = 1.1)
(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 21,
      palette = ch_col,
    ) + tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)

tmap_save(plot_object, filename = file.path(
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT10_sub.jpeg"
  )
))
rm(plot_data, plot_object, osm);gc()
# RT 11 --------------------------------------------------------------------

plot_data <-
  data %>% filter(rt == "RT11") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(
  plot_object <-
    tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(
      col = "season",
      size = 1,
      shape = 21,
      palette = ch_col,
    ) + tm_layout(
      legend.bg.color = "white",
      frame = F,
      legend.frame = T,
      legend.position = c("left", "top")
    )
)

tmap_save(plot_object, filename = file.path(
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT11.jpeg"
  )
))

# -- SUBSET -- ##
plot_data %<>% 
  filter(season != "spring") %>% 
  filter(data.set != "dia_Miguel_Iglesias")

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
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT11sub.jpeg"
  )
))

rm(plot_data, plot_object, osm);gc()

# RT 14 --------------------------------------------------------------------

plot_data <- data %>%
  filter(rt == "RT14") %>%  
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
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT14.jpeg"
  )
))

# ## -- RT14 subset -- ##  
plot_data %<>% filter(season != "winter") 
crop_box  <-
  st_bbox(obj = filter(
    plot_data,
    gr_sample_id %in% c(
      "site_00425_date_00459_dia_MiguelIglesias",
      "site_00202_date_00497_dia_MiguelIglesias",
      "site_00617_date_01057_dia_Naiades",
      "site_00616_date_01072_dia_Naiades"
    )
  ))
plot_data <- st_crop(plot_data, crop_box)
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
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT14sub.jpeg"
  )
))

# extract sites for 003_create_sitesXspecies_diatoms_seasonal.R
plot_data$original_site_name %>% unique() %>% as.character() %>% paste(sep = "\",\"", collapse = "\",\"") %>% writeClipboard()

rm(plot_object, plot_data, crop_box, osm);gc()

# RT 15 -------------------------------------------------------------------
plot_data <-
  data %>% 
  filter(rt == "RT15") %>% 
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
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT15.jpeg"
  )
))
## -- RT15 subset -- ## 
plot_data %<>% 
  filter(season != "spring") %>%
  filter(data.set != "dia_Miguel_Iglesias")
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
  dir_plot,
  paste0(
    "maps/",
    Sys.Date(),
    "_RT15sub.jpeg"
  )
))

rm(plot_data, plot_object, osm)
gc()

## -- ## 

if (readline("remove all? ") == "yes") rm(list = ls()) 


