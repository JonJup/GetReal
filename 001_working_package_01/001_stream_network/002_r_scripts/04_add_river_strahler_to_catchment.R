#############################################
### --- River_Strahler for Catchments --- ###
#############################################

# date: 19.09.19 

# The STRAHLER variable in the catchment database does not refer to the Strahler
# Order of the river but of the catchment. In later steps I do need the
# catchment database to hold the river Strahler order as information and hence I
# add it in this script.

# Setup -------------------------------------------------------------------
pacman::p_load(
                sf, 
               dplyr
               )
## -- load data -- ## 
ccm = st_read("../01_stream_network/01_CCM2/03_data_processed/Catchment/2019-06-13_allGRcountires_WGS84_withOV.gpkg") 
riv = st_read("../01_stream_network/01_CCM2/03_data_processed/Riversegments/2019-05-29_allGRcountires_rivers.gpkg")
        
## -- add variable to catchment data base 
riv2 = select(riv, WSO1_ID, river_strahl = STRAHLER) %>% 
        st_drop_geometry()

ccm2 = left_join(ccm, riv2)

## -- save as spatial file
st_write(ccm2, "../01_stream_network/01_CCM2/03_data_processed/Catchment/2019_09_23_RivStrahl.gpkg")
