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
dt_data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter")) %>% droplevels()


plot_data <- 
        dt_data %>% 
        filter(rt == "RT4_5") %>% 
        filter(data.set %in% c("MZB_LD", "mzb_WISER")) %>% 
        filter(season != "winter") %>%   
        unique(by = "gr_sample_id") %>% 
        st_as_sf()
osm         = read_osm(plot_data, ext = 1.1)
season_plot = make_plot(plot_data)


