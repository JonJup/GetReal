# ------------------------------------------------------ #
### --- Join catchments for all relevant countires --- ### 
# ------------------------------------------------------ #

pacman::p_load(sf, 
               dplyr, 
               here, 
               stringr, 
               tmap, 
               magrittr)

setwd("~/01_Uni/03_GetReal/01_WP_01/01_Stream_Network/01_CCM2/02_GPKG/Catchment/")

# Read in data ------------------------------------------------------------


for (i in 1:10) {
        assign(paste0("Cat", i - 1),
               st_read(paste0(
                       "LAEA_W200", i - 1, ".gpkg"
               )))
}

europe = st_read("../../../../Natural_Earth_quick_start/Europe/Europe.shp")

europe %<>% 
        filter(GU_A3 %in%  c("AUT", "FIN", "SWE", "DEU",
                             "ESP", "ENG", "FXX", "HUN",
                             "ROU", "NIR", "NLX", "SCT",
                             "WLS"
                             )) %>% 
        st_transform(crs = st_crs(Cat0)) %>% 
        st_union 
        

for (i in 1:10) {
        
        assign(
                paste0(
                        "Cat", 
                        i - 1, 
                        "_",
                        "intersection"),
               get(
                       paste0(
                               "Cat",
                               i - 1)
                       )[
                               lengths(
                                       st_intersects(
                                               x = get(
                                                       paste0("Cat",
                                                              i - 1)
                                                       ),
                                               y = europe)
                                       ) > 0,]
               )
}

cat_all = rbind(Cat0_intersection, Cat1_intersection, Cat2_intersection,Cat3_intersection,
             Cat4_intersection, Cat5_intersection, Cat6_intersection, Cat7_intersection,
             Cat8_intersection, Cat9_intersection)
#tm_shape(cat_all) + tm_polygons()
st_write(cat_all, paste0(Sys.Date(),"_allGRcountires02.gpkg"))
