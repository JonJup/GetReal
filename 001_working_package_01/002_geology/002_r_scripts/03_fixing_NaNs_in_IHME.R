#######################################
#### ---- Fixing NaNs in IHME ---- #### 
#######################################

# date: 10.09.19 

# In the IHME version I got from Florian Borgwardt there are some missing cells.
# Catchments that lie partially or completely within this (task one) recieve a
# NaN for all classes. This results in errors during the clustering. So I have
# to find a way to fix this.


# Setup -------------------------------------------------------------------

pacman::p_load(data.table, 
               dplyr,
               sf,
               tmap)

# load geology layer 
ihme = readRDS("03_data_processed/final_ihme_dg.RDS")

# subset to NaN rows 
ihme[is.na(dominant_geology)]

# ok so all rows are NaN and it concerns 9486 catchments. Now I would like to
# see them. So I extract the WSO1ID.
nan_id = ihme[is.na(dominant_geology), WSO1_ID]
ihme_nan = ihme[is.na(dominant_geology)]
# to see whether they all lie completly in one of the holes or just partially I
# will create a modified ccm2 with just these catchments.

# load ccm2 
ccm = st_read("../01_Stream_Network/01_CCM2/03_data_processed/Catchment/2019-06-05_allGRcountires_WGS84.gpkg")

# subset to NaN catchments 
ccm_sub = ccm %>% filter(WSO1_ID %in% nan_id)

# load original ihme 
ihme_old = st_read("01_data_raw/IHME/Florian Borgwardt/IHME1500_v11/ihme_1500_litho4changed.shp")
#tmap_mode("view")
#tm_shape(ihme_old) + tm_polygons() + tm_shape(ccm_sub) + tm_polygons()

# Ok its only those catchments that lie completely inside a gap. 

# join spatial information ihme_nan to find neighbours 

ccm %>% 
        left_join(ihme,
                  by = "WSO1_ID") %>% 
        select(WSO1_ID,
                dominant_geology
               ) %>% 
        mutate(
           finished = F     
        ) -> ihme_sp


ihme_sp[which(!(is.na(ihme_sp$dominant_geology))), 4] = T
# Here wo go writing a nice loop 

# started @ 13:13 10.09.19
# started @ 8:00 13.09.19
ihme_sp = readRDS("fix_na_ihme_19round.RDS")
loop = 20
while (ihme_sp %>% filter(is.na(dominant_geology)) %>% nrow  != 0) {
        
     
for (i in 1:nrow(ihme_sp)) {
        print(i)
        if (ihme_sp$finished[i]) next()
        
        catchment_i = ihme_sp[i,]
        intersection_i = st_intersection(ihme_sp[i,], ihme_sp)
        if (nrow(intersection_i) == 1) {
                ihme_sp$dominant_geology[i] = "unknown -- island"
                ihme_sp$finished[i] = T
                next()
        }
        intersection_i_no_nan = intersection_i %>% filter(!(is.na(dominant_geology.1)))
        
        # yes there is one neighbours 
        if (nrow(intersection_i_no_nan) == 1) {
                ihme_sp$dominant_geology[i] <- intersection_i_no_nan$dominant_geology.1
                ihme_sp$finished[i] = T
                next()
        }
        # there are multiple neighbours
        else if (nrow(intersection_i_no_nan) > 1) {
                # all are equal? 
                if (length(unique(intersection_i_no_nan$dominant_geology.1)) == 1) {
                        ihme_sp$dominant_geology[i] <- intersection_i_no_nan$dominant_geology.1[1]
                        ihme_sp$finished[i] = T
                        next()
                }
                # different dominant geologies
                else if (length(unique(intersection_i_no_nan$dominant_geology.1)) != 1) {
                         n_geology = table(intersection_i_no_nan$dominant_geology.1) %>% as.matrix
                         max_names = row.names(n_geology)[which(n_geology == max(n_geology))]
                         if (length(max_names) != 1) {
                             postition = sample(seq_len(length(max_names)), 1) 
                             ihme_sp$dominant_geology[i] <- max_names[postition]
                             ihme_sp$finished[i] = T
                             next()
                         }
                         else if (length(max_names) == 1) {
                                 ihme_sp$dominant_geology[i] <- max_names
                                 ihme_sp$finished[i] = T
                                 next()   
                         }
                }                
        }
        # there is no neighbour
        else if (nrow(intersection_i_no_nan) == 0) {
                next()
        }
        
}
        save.name = paste0("fix_na_ihme_",loop,"round.RDS")
        saveRDS(object = ihme_sp, file = save.name)
        loop = loop + 1  
}
# save first round 
saveRDS(ihme_sp, "fix_na_ihme_19round.RDS")
st_write(ihme_sp, "tester.gpkg")

# there are some errors in this version. Some dominant geologies are still NA
# but the finished is set to TRUE. First I will correct this in the data and
# then fix the loop.

ihme_sp[which(is.na(ihme_sp$dominant_geology)), 4] = FALSE
ihme_sp[which(!(is.na(ihme_sp$dominant_geology))), 4] = T
#how many are still missing 
 ihme_sp %>% filter(is.na(dominant_geology)) %>% nrow
 
# checks 
ihme_sp %>% filter(is.na(dominant_geology), finished == T)



ihme_dt = 
        ihme_sp %>% 
        st_drop_geometry() %>% 
        setDT

unique(ihme_dt$dominant_geology)
ihme_dt[is.na(dominant_geology), dominant_geology := "unknown -- island"]

saveRDS(file = "final_ihme_dg_fixed.RDS", object = ihme_dt)
