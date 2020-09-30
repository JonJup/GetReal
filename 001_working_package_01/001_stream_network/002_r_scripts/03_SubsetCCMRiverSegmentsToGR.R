# ---------------------------------------------------------- #
### --- Join river segments for all relevant countires --- ### 
# ---------------------------------------------------------- #

pacman::p_load(sf, 
               dplyr, 
               here, 
               stringr, 
               tmap, 
               magrittr)

setwd("~/01_Uni/03_GetReal/01_WP_01/01_Stream_Network/01_CCM2/03_data_processeed/Riversegments/")

# Read in data ------------------------------------------------------------

# read in separate riversegment files 
for (i in 1:10) {
        assign(paste0("Cat", i - 1),
               st_read(paste0(
                       "LAEA_W200", i - 1, "_Riversegments.gpkg"
               )))
}

# load Natural Earth layers and subset them to GR countires 
europe = st_read("../../../../MISC/Natural_Earth_quick_start/Europe/Europe.shp")

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
                        "RiverSegement", 
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

rivseg_all = rbind(
        RiverSegment0_intersection,
        RiverSegment1_intersection,
        RiverSegment2_intersection,
        RiverSegment3_intersection,
        RiverSegment4_intersection,
        RiverSegment5_intersection,
        RiverSegment6_intersection,
        RiverSegment7_intersection,
        RiverSegment8_intersection,
        RiverSegment9_intersection
)

# save file 
st_write(cat_all, paste0(Sys.Date(),"_allRiverSegmentsGR.gpkg"))
