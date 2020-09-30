####################################
#### ---- Compile datasets ---- ####
####################################

# Jonathan Jupke 
# date: 14.02.20
# Ecotox Typology small rivers 

# purpose: The relevant variables for the classification are spread over several
# different data sets. Here I gather all these data sets and create one data set
# with all the rELEVant variables but not one more. This data set does not need
# to be spatial as all informations can be joined via WSO1_ID.

# Setup -------------------------------------------------------------------
pacman::p_load(sf,
               data.table,
               dplyr,
               ggplot2)


setwd("~/01_Uni/03_GetReal/001_WP_01/")
# Loading data sets  ------------------------------------------------------

#1. the ccm2 catchments data base  
ccm <- st_read("001_Stream_Network/01_CCM2/03_data_processed/Catchment/2019_09_23_RivStrahl.gpkg")
#2 the ccm2 rivers data base 
rivers <- st_read("001_Stream_Network/01_CCM2/03_data_processed/Riversegments/2019-05-29_allGRcountires_rivers.gpkg")
#3. the lakes file 
lakes = readRDS("008_lakes/03_data_processed/190902_lakes_final.RDS") %>% 
        setDT
#4. geology 
ihme = readRDS("002_Geology/003_data_processed/final_ihme_dg_fixed.RDS") %>% 
        setDT
#5. discharge 
discharge = readRDS("004_modelling_discharge/03_data_processed/magnificent7_final.RDS")

#6. lgm 
lgm = readRDS("005_lgm/03_data_processed/LGM_WSO1ID.RDS") %>% 
        setDT

#7. sinuosity
sinus = readRDS("007_sinuosity/03_data_processed/sinuosity.RDS")

#8. cumulative upstream area 
upstream <- readRDS("006_cum_upstream_area/003_data_processed/cumulative_upstream_area.RDS")
sum(duplicated(upstream$WSO1_ID))

# subset data sets  ---------------------------------------------------------
ccm2 = ccm %>%
        st_drop_geometry() %>%
        setDT

ccm2 = ccm2[,list(
        WSO1_ID,
        strahler = river_strahl,
        area = AREA_KM2,
        elevation = ELEV_MEAN,
        elev_range = ELEV_MAX - ELEV_MIN,
        slope =  SLOPE_MEAN,
        slope_range = SLOPE_MAX - SLOPE_MIN,
        precipitation = RAIN_MEAN,
        precipitation_range = RAIN_MAX - RAIN_MIN,
        temperature = TEMP_MEAN,
        temperature_range = TEMP_MAX - TEMP_MIN,
        y_Coords = Y_CENTROID_LAEA,
        x_Coords = X_CENTROID_LAEA
        
)]

rivers2 <-
        rivers %>% 
        st_drop_geometry() %>% 
        setDT

rivers2 = rivers2[,.(WSO1_ID, 
           Segement_length = LENGTH,
           distance_to_source = CUM_LEN)]

lakes2 = lakes[,list(
        WSO1_ID,
        prop_Lakes = round(proportionRiver,2)
)]


dis_id = which(discharge == "Only NAs")
discharge2 = discharge[-dis_id] %>% rbindlist
discharge3 = discharge2[,list(amplitude, ar1_correlation = ar1, discharge = lam1, phase,
                              tau2, tau3, tau4, WSO1_ID)]

discharge3[WSO1_ID == 633907]

# remove one duplicated row 
discharge3 <- discharge3[-which(duplicated(discharge3$WSO1_ID))]

ihme[,finished := NULL]

## set factors 
ccm2$strahler = factor(ccm2$strahler,
                       levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                       ordered = T)


lgm[, lgm_name := ifelse(veg_id == 0, "NA", 
                       ifelse(veg_id == 11, "Open boreal woodlands",
                               ifelse(veg_id == 13, "Tundra",
                                       ifelse(veg_id == 14, "Steppe-tundra",
                                               ifelse(veg_id == 18, "Forest steppe", "permanent ice")
                                               )
                                       )
                               )
                        )
                       ]

lgm$lgm_name = factor(lgm$lgm_name)
lgm[, veg_id := NULL]
ihme$dominant_geology = factor(ihme$dominant_geology) 

# Join data sets  ---------------------------------------------------------

joind = ccm2[rivers2, on = "WSO1_ID"]
joind = joind[lakes2, on = "WSO1_ID"]
joind = joind[lgm, on = "WSO1_ID"]
joind = joind[discharge3, on = "WSO1_ID"]
joind = joind[ihme, on = "WSO1_ID"]
joind = joind[sinus, on = "WSO1_ID"]
joind = joind[upstream, on = "WSO1_ID"]
# Size criterion ----------------------------------------------------------

joind[WSO1_ID == 633907]

# remove elements with AREA = NA - lose ~ 11.000 catchments 
joind2 = joind[-which(is.na(area))]
# extract size limits 
# joind2$Area %>% summary %>% .["Median"] -> area_median
# joind2$Area %>% summary %>% .["Mean"] -> area_mean
# joind2$Area %>% summary %>% .["3rd Qu."] -> area_3rdQ

joind3 = joind2[dominant_geology != "unknown -- island" & 
                        lgm_name != "NA" &
                        !is.na(strahler)]

joind3$dominant_geology <- droplevels(joind3$dominant_geology)              
joind3$lgm_name <- droplevels(joind3$lgm_name)              
                
#joind_strahler4 = joind2[strahler <= 4]

## -- plot of discharge breaks -- ## 
joind3 %>% 
        ggplot(aes(x = strahler, y = discharge)) + 
        geom_boxplot() +
        scale_y_log10()

joind2 %>% 
        group_by(strahler) %>% 
        count()

# joind2 %>% 
#         group_by(strahler) %>% 
#         summarize(median = median(discharge)) -> breaks1

joind2[discharge < 10]

midpoints = c()
for (i in 1:(nrow(breaks1) - 1)) {
        
        midpoints[i] = mean(
                as.numeric(breaks1[i,2]),
                as.numeric(breaks1[i + 1,2]))
        
}

joind3 = joind2

joind3 = joind3[discharge <= 10]

## save files 
# saveRDS(joind_strahler6, "03_data_processed/base_data_for_typology_strahler6.RDS")
# saveRDS(joind_median, "03_data_processed/base_data_for typology_median.RDS")
# saveRDS(joind_mean, "03_data_processed/base_data_for typology_mean.RDS")
# saveRDS(joind_3rdQ, "03_data_processed/base_data_for typology_3rdQ.RDS")
# saveRDS(joind_strahler5, "03_data_processed/base_data_for_typology_strahler5.RDS")
#saveRDS(joind_strahler4, "03_data_processed/base_data_for_typology_strahler4.RDS")
saveRDS(joind3, paste0("009_create_typology/003_data_processed/",Sys.Date(),"base_data_for_typology.RDS"))

        