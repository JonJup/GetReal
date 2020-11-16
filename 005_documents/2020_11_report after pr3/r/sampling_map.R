### ------------------------------------------- ###
### --- Exploratory analysis of seasonality --- ###
### --- Macroinvertebrates -------------------- ###
### ------------------------------------------- ###
### ------------------------------- ###
### --- Sampling map RLT report --- ### 
### ------------------------------- ### 

# date created: 08.10.20
# date used:    08.10.20  

# Short version of 001_explore_stream_types.R

# setup -------------------------------------------------------------------
pacman::p_load(
               here,
               OpenStreetMap,
               sf,
               stringr,
               tmaptools,
               tmap)

dir_pd = "~/01_Uni/02_getreal/002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"

# load data ----------------------------------------------------------------
data <- readRDS(file.path(dir_pd, "004_2020-11-03_mzb_data1585_low_impact.RDS"))
mcp_sub <-c("#d95f02", "#666666", "#5f64ff", "#dcce00")
# 
# # carpeting ---------------------------------------------------------------
data[, c("species", "genus", "family", "order") := NULL]
data[season == "Summer", season := "summer"]
data <- data[date >= as.Date("2000-01-01") & !is.na(season) & !is.na(ls_bd_20)]
data <- data[, c("rt", "ls_bd_20") := .(ls_bd_20, NULL)]
data[rt %in% c("RT10", "RT11"), rt := "RT10_11"]
data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter")) %>% droplevels()

# RT 10 + 11 --------------------------------------------------------------------

plot_data <- data %>% 
        filter(rt == "RT10_11") %>%  
        filter(data.set!="Picos_Pepe") %>% 
        unique(by = "gr_sample_id") %>% 
        st_as_sf()

osm <- read_osm(plot_data, ext=1.1)

tm_sampling <- tm_shape(osm) + tm_rgb() + tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("RIGHT", "top"))

ch_remove = c("osm", "ch_remove", "plot_data", "data", "mcp_sub", "dir_pd")
rm(list = ch_remove)
