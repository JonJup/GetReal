# ----------------------------------------- #
### --- create maps of sampling sites --- ### 
### --- Presentation 
###     GetReal
###     Progress Review 3 --------------- ###
# ----------------------------------------- #

#date created: 19.10.20
#date used   : 


# Setup -------------------------------------------------------------------
pacman::p_load(
               dplyr, 
               ggplot2, 
               here,
               magrittr,
               sf,
               tmap)
setwd(here())

# load data     
# on data stick 
europe = st_read(dsn = "E:\\Data/Natural_Earth_quick_start/2020_06_29_europe.gpkg")
ls20   = st_read(dsn = "../my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/m_river_fec_broad_type.shp")
sites  = readRDS("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/004_2020-07-03_mzb_sites1585_low_impact.RDS")


# carpeting --------------------------------------------------------------
ls20 %<>% st_transform(crs = 4326)
#ls20_t1 <- ls20 %>% filter(m_btype20c == "RT1")
 
# sites %<>% filter(gr_sample_id %in% site_names)
# sites_t1 <- sites %>% filter(ls_bd_20 == "RT1")
# rm(site_names)

# Map  --------------------------------------------------------------------

# rt10 --- 
sf_rt11 <- filter(ls20, m_btype20c == "RT11")
sf_rt11_sites = filter(sites, ls_bd_20 == "RT11")

map_rt11 <- 
        tm_shape(europe) + tm_polygons() + 
        tm_shape(sf_rt11) + tm_lines(col = "blue") + 
        tm_shape(sf_rt11_sites) + tm_dots(col = "red", size = .1) + 
        tm_layout(title = "River Type 11") 

# rt18 --- 
sf_rt18 <- filter(ls20, m_btype20c == "RT18")
sf_rt18_sites = filter(sites, ls_bd_20 == "RT18")    

map_rt18 <- 
                tm_shape(europe) + tm_polygons() + 
                tm_shape(sf_rt18) + tm_lines(col = "blue") + 
                tm_shape(sf_rt18_sites) + tm_dots(col = "red", size = .1) + 
                tm_layout(title = "River Type 18") 


tmap_save(tm=map_rt11, filename = "005_documents/2020_10_PR3/figures/sample_map_rt11.png")
tmap_save(tm=map_rt18, filename = "005_documents/2020_10_PR3/figures/sample_map_rt18.png")

       
