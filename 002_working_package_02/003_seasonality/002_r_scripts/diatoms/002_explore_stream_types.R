### ------------------------------------------- ###
### --- Exploratory analysis of seasonality --- ###
### --- Diatoms --------------------------- --- ###
### ------------------------------------------- ###

# date created: 01.09.20
# date used:    01.09.20 + 2. + 14. + 15. 

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
data <- readRDS("003_results/diatoms/001_2020-09-01_diatoms_w_rt.RDS")
mcp_sub <-c("#d95f02", "#666666", "#5f64ff", "#dcce00")

# carpeting ---------------------------------------------------------------
data[, c("original_name", "species", "genus", "family", "order", "name_source") := NULL]
data$season %>% unique
data <- data[!is.na(season)]
data <- data[!is.na(rt)]


# --  old --  
# data[rt %in% c("RT2", "RT8"), rt := "RT2_8"]
# data[rt %in% c("RT1", "RT18"), rt := "RT1_18"]
# remove badly represented stream types 
# data <- data[!rt %in% c("RT7", "RT10", "RT11", "RT13", "RT15", "RT20")]
# --  ------

data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter")) %>% droplevels()
data <- data[date >= as.Date("2000-01-01")] 


# RT 1 -------------------------------------------------------------------- 

# no subset created

plot_data <- data %>% filter(rt == "RT1") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT1all.jpeg"))

rm(plot_data, plot_object, osm);gc()


# RT 2  --------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT2") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT2all.jpeg"))

rm(plot_data, plot_object, osm);gc()

# RT 3 --------------------------------------------------------------------

# No areas with sampling sites from different seasons. All areas are far apart.   

plot_data <- data %>% filter(rt == "RT3") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT3all.jpeg"))

rm(plot_data, plot_object, osm);gc()

# RT 4 --------------------------------------------------------------------

# Sites in NL, FR and HU. HU all except one spring, FR spatial and seasonal
# signal too similar and NL mostly spring, too few sites from other seasons.

plot_data <- data %>% filter(rt == "RT4") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT4all.jpeg"))

rm(plot_data, plot_object, osm);gc()

# RT 5 --------------------------------------------------------------------

# No sunset created. Sites in NL, FR and HU. HU only spring, FR summer and
# autumn but only two sites in autumn (potentially over multiple years, did not
# check); NL only one site in summer (same as above).

plot_data <- data %>% filter(rt == "RT5") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT5all.jpeg"))

rm(plot_data, plot_object, osm);gc()

# old subset 
# plot_data <- data %>% filter(rt == "RT5") %>% filter(!data.set %in% c("dia_Ecosurv", "ediwn_peters_dia")) %>% filter(original_site_name != "PREUILLE A BRAM") %>% unique(by = "gr_sample_id") %>%  st_as_sf()

# RT 6 --------------------------------------------------------------------

# Only four sites 

plot_data <- data %>% filter(rt == "RT6") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT6all.jpeg"))

rm(plot_data, plot_object, osm);gc()


# RT 7 ---------------------------------------------------------------------

# no observations  

# RT 8 --------------------------------------------------------------------

tmap_mode("view")
tmap_mode("plot")

plot_data <- data %>% filter(rt == "RT8") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT8all.jpeg"))

## subset ..
plot_data %<>% filter(data.set == "dia_Naiades") %>% filter(season != "spring")
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT8sub.jpeg"))

rm(plot_data, plot_object, osm);gc()

# RT 9 --------------------------------------------------------------------

# There is a region where a summer autumn subset would be possible - but with only three autumn sites and many summer sites. 
# For now, I created no subset. 

tmap_mode("view")
tmap_mode("plot")

plot_data <- data %>% filter(rt == "RT9") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT9all.jpeg"))


# RT 10 --------------------------------------------------------------------


plot_data <- data %>% filter(rt == "RT10") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT10all.jpeg"))

## subset ..
plot_data %<>% filter(season != "spring") %>% filter(data.set != "dia_Miguel_Iglesias")
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT10sub.jpeg"))

rm(plot_data, plot_object, osm);gc()



# RT 11 --------------------------------------------------------------------


plot_data <- data %>% filter(rt == "RT11") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT11all.jpeg"))

## subset ..
plot_data %<>% filter(season != "spring") %>% filter(data.set != "dia_Miguel_Iglesias")
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT11sub.jpeg"))

rm(plot_data, plot_object, osm);gc()

# RT 12 --------------------------------------------------------------------

# no observations 

# RT 13 --------------------------------------------------------------------

# no observations 

# RT 14 --------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT14") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT14all.jpeg"))

## subset ..
plot_data %<>% filter(season != "winter") 
crop_box  <- st_bbox(obj = filter(plot_data, gr_sample_id %in% c("site_00425_date_00459_dia_MiguelIglesias", "site_00202_date_00497_dia_MiguelIglesias",
                                                                  "site_00617_date_01057_dia_Naiades", "site_00616_date_01072_dia_Naiades")))
plot_data <- st_crop(plot_data, crop_box)
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT14sub.jpeg"))

plot_data$original_site_name %>% unique() %>% as.character() %>% paste(sep = "\",\"", collapse = "\",\"") %>% writeClipboard()

rm(plot_object, plot_data, crop_box, osm);gc()

# RT 15 -------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT15") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT15all.jpeg"))

## subset ..
plot_data %<>% filter(season != "spring") %>% filter(data.set != "dia_Miguel_Iglesias")
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT15sub.jpeg"))

rm(plot_data, plot_object, osm);gc()




# RT 16 -------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT16") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT16all.jpeg"))

## subset ..
plot_data %<>% filter(season != "winter") 
crop_box  <- st_bbox(obj = filter(plot_data, gr_sample_id %in% c("site_00940_date_00723_dia_Naiades", "site_00946_date_01039_dia_Naiades",
                                                                 "site_01032_date_01066_dia_Naiades", "site_00942_date_00840_dia_Naiades")))
plot_data <- st_crop(plot_data, crop_box)

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT16sub.jpeg"))

plot_data$original_site_name %>% unique() %>% as.character() %>% paste(sep = "\",\"", collapse = "\",\"") %>% writeClipboard()

rm(plot_object, plot_data, crop_box, osm);gc()

# RT 17 -------------------------------------------------------------------- 

plot_data <- data %>% filter(rt == "RT17") %>%  unique(by = "gr_sample_id") %>% st_as_sf()
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT17all.jpeg"))

## subset ..
plot_data %<>%
  filter(data.set != "dia_Miguel_Iglesias") %>%
  filter(!original_site_name %in% c(  # all sites on corsica
    "REGINO A OCCHIATANA 2",
    "ALISO A OLETTA 2",
    "GOLO A CAMPILE",
    "GOLO A VOLPAJOLA",
    "GOLO A LUCCIANA 3",
    "FIUM ALTO A PENTA-DI-CASINCA",
    "TAVIGNANO A ANTISANTI",
    "TAVIGNANO A ANTISANTI 1",
    "TAVIGNANO A ALERIA 1",
    "TAGNONE A ALERIA",
    "FIUMORBO A GHISONACCIA",
    "RIZZANESE A SARTENE",
    "TARAVO A CASALABRIVA",
    "TARAVO A URBALACONE",
    "PRUNELLI A BASTELICACCIA",
    "LIAMONE A ARBORI",
    "LIAMONE A MURZO",
    "FANGO A GALERIA",
    "FANGO A MANSO"
  )) 

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT17sub.jpeg"))


# RT 18 -------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT18") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT18all.jpeg"))

## subset ..
plot_data %<>% filter(!season %in% c("spring", "winter")) %>% filter(!original_site_name %in% c(
          "TARTAGINE A CASTIFAO", # island: Corsica
          "ASCO A MOLTIFAO",      # island: Corsica
          "TAVIGNANO A ALTIANI",  # island: Corsica
          "TARAVO A FORCIOLO"     # island: Corsica
        ))
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT18sub.jpeg"))

rm(plot_data, plot_object, osm);gc()

# RT 19 --------------------------------------------------------------------

plot_data <- data %>% filter(rt == "RT19") %>%  unique(by = "gr_sample_id") %>% st_as_sf()

osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT19all.jpeg"))

## subset ..
plot_data %<>%  filter(season != "winter") %>%  
        filter(! original_site_name %in% c(
                "LURI A LURI","RUISSEAU DE PIETRACORBARA A PIETRACORBARA","ALISO A SAN-GAVINO-DI-TENDA",
                "BEVINCO A RUTALI","FIUME SECCU A MONTEGROSSO","CASALUNA A GAVIGNANO","BRAVONE A PIANELLO",
                "RIVIERE DU BUSSO A NOVALE","ALESANI A SAN-GIULIANO 2","BRAVONE A LINGUIZZETTA","RU D'AITONE A EVISA",
                "CRUZINI A AZZANA","GRAVONE A BOCOGNANO 1","FIUMORBO A GHISONI","PRUNELLI A BASTELICA 1",
                "ABATESCO A SERRA-DI-FIUMORBO","TRAVO A VENTISERI","FIUMICELLU A FORCIOLO","CAVO A ZONZA 2",
                "RUISSEAU DE VENTILEGNE A BONIFACIO"
        )) 
        
osm <- read_osm(plot_data, ext = 1.1)
(plot_object <- tm_shape(osm) + tm_rgb()+ tm_shape(plot_data) + tm_dots(col = "season", size = 1, shape = 21, palette = mcp_sub) + tm_layout(legend.bg.color = "white", frame = F, legend.frame = T, legend.position = c("left","top")))
tmap_save(plot_object, filename = paste0("004_plots/diatoms/maps/", Sys.Date(), "_RT19sub.jpeg"))

rm(plot_data, plot_object, osm);gc()

## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 


# old parts  --------------------------------------------------------------

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

