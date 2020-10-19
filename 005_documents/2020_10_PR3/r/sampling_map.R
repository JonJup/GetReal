### ------------------------------------------------- ###
### --- seasonal sampling maps PRIII presentation --- ### 
### ------------------------------------------------- ### 

# date created: 19.10.20
# date used:    19.10.20  

# Short version of 001_explore_stream_types.R
# The plots are saved and loaded into the presentation as Images because the resolution is bad otherwise. 

# setup -------------------------------------------------------------------
pacman::p_load(
        here,
        data.table,
        dplyr,
        OpenStreetMap,
        pkgcond,
        sf,
        stringr,
        tmaptools,
        tmap)


options(warn = - 1) 

setwd(here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"))
# load data ----------------------------------------------------------------
data <- readRDS("004_2020-07-03_mzb_data1585_low_impact.RDS")
mcp_sub <-c("#d95f02", "#666666", "#5f64ff", "#dcce00")


# # carpeting ---------------------------------------------------------------
data[, c("species", "genus", "family", "order") := NULL]
data[season == "Summer", season := "summer"]
data <- data[date >= as.Date("2000-01-01") & 
                     !is.na(season) & 
                     !is.na(ls_bd_20)]
data <- data[, c("rt", "ls_bd_20") := .(ls_bd_20, NULL)]
data[rt %in% c("RT10", "RT11"), rt := "RT10_11"]
data[rt %in% c("RT15", "RT16"), rt := "RT15_16"]
data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter")) %>% droplevels()

# RT 6 --------------------------------------------------------------------
plot_data_6 <- data %>% 
        filter(rt == "RT6") %>%  
        unique(by = "gr_sample_id") %>% 
        st_as_sf()

osm_6 <- read_osm(plot_data_6, ext=1.1)
plot_object <- tm_shape(osm_6) + tm_rgb() + tm_shape(plot_data_6) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) +  tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("RIGHT", "top"))

rm(plot_data_6, osm_6)

# 15-16 ---------------------------------------------------------------
plot_data1 <- data %>%
        dplyr::filter(rt=="RT15_16") %>% 
        unique(by = "gr_sample_id") %>% 
        st_as_sf()
osm1 <- read_osm(x=plot_data1)
plot_object1 <- tm_shape(osm1) + tm_rgb() + 
        tm_shape(plot_data1) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + 
        tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top"))

#subset 
plot_data2 <- data %>% 
        filter(rt=="RT15_16") %>% 
        filter(data.set=="mzb_Naiades") %>%  
        unique(by = "gr_sample_id") %>% 
        st_as_sf()
crop_box  <- st_bbox(obj = dplyr::filter(plot_data2, 
                                         gr_sample_id %in% 
                                                 c("site_00719_date_00300_mzb_Naiades", 
                                                   "site_00823_date_00089_mzb_Naiades", 
                                                   "site_01192_date_00069_mzb_Naiades", 
                                                   "site_00142_date_00092_mzb_Naiades")
)
)
plot_data2 <- suppress_messages(
        expr=st_crop(
                plot_data2, 
                crop_box
                )
        )
        
osm2 <- read_osm(plot_data2, 
                 ext = 1.1)
plot_object2 <- tm_shape(osm2) + tm_rgb() + 
        tm_shape(plot_data2) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + 
        tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("LEFT","BOTTOM"), legend.outside=FALSE)

options(warn = 1) 

tmap_save(tm=plot_object,filename= here("005_documents/2020_10_PR3/figures/map_6.png"),outer.margins=c(0,0,0,0))
tmap_save(tm=plot_object1,filename=here("005_documents/2020_10_PR3/figures/map_15_1.png"),outer.margins=c(0,0,0,0))
tmap_save(tm=plot_object2,filename=here("005_documents/2020_10_PR3/figures/map_15_2.png"),outer.margins=c(0,0,0,0))
