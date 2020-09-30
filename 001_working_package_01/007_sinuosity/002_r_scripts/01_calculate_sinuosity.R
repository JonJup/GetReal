#######################################
#### ---- calculate sinuosity ---- #### 
#######################################

# date: 10.09.19 

# In this script I calculate the sinuosity of each stream. The sinuosity is
# defined as actual length divided by the shortest path between the two
# endpoints.


# Setup -------------------------------------------------------------------
#load libraries 
pacman::p_load(sf, dplyr)
#load river layer 
ccm2river = st_read("01_Uni/GetReal/01_WP_01/01_Stream_Network/01_CCM2/03_data_processed/Riversegments/2019-05-29_allGRcountires_rivers.gpkg")
#add sinuosity column 
ccm2river$sinuosity = 0

#for each river segment the actual length (L) is divided by the two endpoints.
#The endpoints are derived by casting the line as points and then calculating
#the distance between all the different points. The last row in the first column
#is the distance from the first to the last point (D). 

for (i in seq_len(nrow(ccm2river))) {
        
        segment =  ccm2river[i,]
        L = st_length(segment)
        points = st_cast(segment, "POINT")
        D = st_distance(points)[1,nrow(points)]
        ccm2river$sinuosity[i] = L/D
        print(i)
}

# the final prodcut is reduced to a non-spatial tibble with only ID and sinuosity.
ccm2river %>% st_drop_geometry() %>% 
        select("WSO1_ID", "sinuosity") -> sinu.save

# ... and saved as RDS.
saveRDS(sinu.save, "sinuosity.RDS")

