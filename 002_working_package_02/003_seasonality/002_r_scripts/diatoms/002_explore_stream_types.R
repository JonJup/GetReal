### ------------------------------------------- ###
### --- Exploratory analysis of seasonality --- ###
### --- Diatoms --------------------------- --- ###
### ------------------------------------------- ###

# date created: 01.09.20
# date used:    01.09.20 + 2. + 14. + 15. + 09.11 + 26.11

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
tmap_mode("view")
DIR = list(re = here("002_working_package_02/003_seasonality/003_results/diatoms/"),
           plot = here("002_working_package_02/003_seasonality/004_plots/diatoms/maps/"),
           pd = here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/")
           )



# FUNCTIONS ---------------------------------------------------------------
prepare_plot = function(x, crit = NULL){
  
  if (is.null(crit)){
  plot_data =
    dt_data %>% 
    filter(rt == x) %>%  
    unique(by = "gr_sample_id") %>% 
    st_as_sf()
  } else {
    plot_data =
      dt_data %>% 
      filter(rt == x) %>%  
      unique(by = "gr_sample_id") %>% 
      st_as_sf()
  }
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

# LOAD  ----------------------------------------------------------------
dt_data = readRDS(file.path(DIR$re, "001_2020-11-09_diatoms_w_rt.RDS"))
ch_col  = c("#d95f02", "#666666", "#5f64ff", "#dcce00")
dt_sxs  = readRDS(file.path(DIR$pd, "08_sxs.RDS"))
ls_rare = readRDS(file.path(DIR$pd, "0x_rare_taxa_list.RDS"))
ch_empty = readRDS(file.path(DIR$pd, "0x_empty_sites.RDS"))
ch_medium_types <- paste0("RT", c(1, 2, 3, 4, 5, 6, 8, 9, 12, 16, 17, 18, 19))
# CARPET ---------------------------------------------------------------
# drop cols 
dt_data[, c("original_name", "species", "genus", "family", "order", "name_source") := NULL]
# drop rows 
dt_data = dt_data[!is.na(rt) & 
                    !is.na(season) & 
                    !is.na(geometry) & 
                    (date >= as.Date("2000-01-01") | is.na(date)) & 
                    rt %in% ch_medium_types &
                    (rt %in% dt_sxs$spe$ls_bd_20 | rt %in% dt_sxs$gen$ls_bd_20) & 
                    !gr_sample_id %in% ch_empty
                  ]
# season as factor 
dt_data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter")) %>% droplevels()

# check that all river types are in ls_rare 
if (sum(unique(dt_data$rt) %in% names(ls_rare)) == uniqueN(dt_data$rt)) {
  print("sanity check passed")
} else {
  print("sanity check failed")
}

ls_loop = list()

for (i in seq_along(ls_rare)) {
  loop_spe = ls_rare[[i]]$spe
  loop_spe %<>% str_replace_all(pattern = "\\.", replacement = "\\ ")
  loop_gen = ls_rare[[i]]$gen
  loop_gen %<>% str_replace_all(pattern = "\\.", replacement = "\\ ")
  loop_rt = names(ls_rare)[i]
  
  ls_loop[[i]] = 
    rbindlist(
      list(
        dt_data[rt == loop_rt & 
                  final_taxon_level == "species" & 
                  !final_taxon %in% loop_spe, ], 
        dt_data[rt == loop_rt & 
                  final_taxon_level == "genus" & 
                  !final_taxon %in% loop_gen, ]
        )
      )
  
  rm(loop_rt, loop_gen, loop_spe,i)
}

dt_data = rbindlist(ls_loop)

# combine river types
dt_data[rt %in% c("RT17", "RT18"), rt := "RT17_18"]
dt_data[rt %in% c("RT1", "RT2", "RT4"), rt := "RT1_2_4"]

rm(dt_sxs, ch_medium_types, ch_empty, ls_rare, ls_loop)

#saveRDS(file = file.path(DIR$re, "base_data_seasons.RDS"), object = dt_data)

# RT1_2_4 -------------------------------------------------------------------- 
type = "RT1_2_4"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT1_2_4 SUB 1 -------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
  filter(!season %in% c("winter", "spring"))
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type)
gc()

# RT 3 --------------------------------------------------------------------
# No areas with sampling sites from different seasons. 
# All areas are far apart.   
# -> NO SUBSET
type = "RT3"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))
 
rm(type, ls_plot, map);gc()

# RT 5 --------------------------------------------------------------------
type = "RT5"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT5 SUB 1 ---------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
  filter(data.set %in% c("dia_Naiades", "IRSTEA")) 
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type,map)
gc()

# RT 8 --------------------------------------------------------------------
type = "RT8"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT8 SUB 1 ---------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
  filter(data.set  != "Janne_Soininen" & !season %in% c("winter", "spring"))
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type,map)
gc()

# RT 9 --------------------------------------------------------------------
type = "RT9"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT9 SUB 1 ---------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
  filter(data.set  != "dia_Ecosurv" & !season %in% c("winter", "spring"))
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))
rm(ls_sub1, ls_plot, type,map)
gc()
# RT16 --------------------------------------------------------------------
type = "RT16"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT16 SUB 1 --------------------------------------------------------------
ls_sub1 = ls_plot
ls_sub1$data %<>% 
  filter(!season %in% c("autumn", "spring"))
plot_bbox = c("site_00793_date_00984_dia_Naiades", "site_00855_date_00988_dia_Naiades","site_00725_date_00993_dia_Naiades","site_00738_date_00983_dia_Naiades")
plot_bbox = filter(ls_sub1$data,gr_sample_id %in% plot_bbox) %>%
  st_bbox()
ls_sub1$data %<>% st_crop(plot_bbox)
ls_sub1$osm = read_osm(ls_sub1$data, ext = 1.1)
map = plot_list(ls_sub1)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub1.jpeg")))

# RT16 SUB 2 ---------------------------------------------------------------
ls_sub2 = ls_plot
ls_sub2$data %<>% 
  filter(!season %in% c("winter", "spring"))
ls_sub2$osm = read_osm(ls_sub2$data, ext = 1.1)
map = plot_list(ls_sub2)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub2.jpeg")))

# RT17_18 -----------------------------------------------------------------
type = "RT17_18"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# RT19 --------------------------------------------------------------------
type = "RT19"
ls_plot = prepare_plot(type)
map = plot_list(ls_plot)
tmap_save(map, filename = file.path(DIR$plot, paste0(type, ".jpeg")))

# ALTERNATIVE SUBS  ------------------------------------------------------------
# RT1_2_4 SUB 2-------------------------------------------
# ls_sub2 = ls_plot
# ls_sub2$data %<>% 
#   filter(!season %in% c("winter"))
# plot_bbox = c("site_03147_date_00137_DIA_edwin_peters", "site_00306_date_00004_dia_Saul_Blanco","site_00085_date_00001_dia_Saul_Blanco","site_00087_date_00002_dia_Ecosurv")
# plot_bbox = filter(ls_sub2$data,gr_sample_id %in% plot_bbox) %>% 
#   st_bbox() 
# ls_sub2$data %<>% st_crop(plot_bbox)
# ls_sub2$osm = read_osm(ls_sub2$data, ext = 1.1)
# map = plot_list(ls_sub2)
# tmap_save(map, filename = file.path(DIR$plot, paste0(type, "_sub2.jpeg")))
# rm(type, map, plot_bbox, ls_sub2, ls_plot)

