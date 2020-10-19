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
europe     <- st_read(dsn = "../../../../001_WP_01/MISC/Natural_Earth_quick_start/2020_06_29_europe.gpkg")
ls20       <- st_read(dsn = "../../../../../My Papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/m_river_fec_broad_type.shp")
site_names <- readRDS("003_processed_data/001_speciesXsites_tables/2020-06-29_sites_bio2_all.RDS")
sites      <- readRDS("003_processed_data/004_2020-06-29_mzb_sites1585_low_impact.RDS")
#sites_ls20 <- readRDS("003_processed_data/005_2020-06-29obs_low_impact_w_LS20.RDS") 


# EDA ---------------------------------------------------------------------



# carpenting --------------------------------------------------------------
ls20 %<>% st_transform(crs = 4326)
ls20_t1 <- ls20 %>% filter(m_btype20c == "RT1")
 
sites %<>% filter(gr_sample_id %in% site_names)
sites_t1 <- sites %>% filter(ls_bd_20 == "RT1")
rm(site_names)

# Map  --------------------------------------------------------------------

for (i in 1:20) {
        
        # declare variables 
        river     <- paste0("RT", i)
        map_title <- paste("Rivertype", i) 
        save_name <- paste0("003_processed_data/002_sampling_site_maps/sampling_sites_in_", river, ".pdf")
        
        loop_sub_rivers <- filter(ls20, m_btype20c == river)
        loop_sub_sites  <- filter(sites, ls_bd_20 == river)
        
        if (nrow(loop_sub_sites) == 0) next()
        
        loop_map <- tm_shape(europe) + tm_polygons() + 
                tm_shape(loop_sub_rivers) + tm_lines(col = "blue") + 
                tm_shape(loop_sub_sites) + tm_dots(col = "red", size = .1) + 
                tm_layout(title = map_title) + 
                tm_compass(position = c("left", "top")) + 
                tm_scale_bar(position = c("center", "bottom"))
        
        tmap_save(tm = loop_map, 
                  filename = save_name, 
                  )
        print(paste(i))
}


       
