### -------------------------------------------- ###
### --- create maps of diatom sampling sites --- ### 
### -------------------------------------------- ###

# date_created: 04.08.20
# date_used: 04.08.20
# Jonathan Jupke 
# Diatoms GetReal WP2

# create separate maps of diatom sampling (saved in pdf format) for each lyche solheim River Type. 

# setup -------------------------------------------------------------------
pacman::p_load(here, sf, dplyr, ggplot2, magrittr, tmap)
setwd(here())

# load data ---------------------------------------------------------------
europe <- st_read(dsn = "../../../../001_WP_01/MISC/Natural_Earth_quick_start/2020_06_29_europe.gpkg")
ls20   <- st_read(dsn = "../../../../../My Papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/m_river_fec_broad_type.shp")
spe    <- readRDS("003_processed_data/010_2020-08-04diatom_species_w_rare_w_LSRT.RDS")
gen    <- readRDS("003_processed_data/010_2020-08-04diatom_genus_w_rare_w_LSRT.RDS.RDS")
sites  <- readRDS("003_processed_data/004_2020-06-30_dia_sites_low_ls_all.RDS")

# carpeting --------------------------------------------------------------
sites %<>% filter(gr_sample_id %in% spe$gr_sample_id)
sites %<>% left_join(spe[,c("gr_sample_id", "ls_bd_20")])
ls20 %<>% st_transform(crs = 4326)

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


       
