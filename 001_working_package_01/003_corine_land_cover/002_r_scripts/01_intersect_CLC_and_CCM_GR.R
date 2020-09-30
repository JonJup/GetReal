#######################################################################
### --- Calculate percentage of landcover classes per catchment --- ###
#######################################################################

# date: 11.09.19

# This scripts takes the CLC raster and the GR CCM2 catchtments and calculates
# the prevalence of each land cover class in % in each catchment.


# Setup -------------------------------------------------------------------
pacman::p_load(sf, data.table, raster)
# wd on server -- just for my info 
#setwd("~/media/disk/_cygdrive_C_Users_jonat_Desktop_Freigabe/")

setwd(here::here(""))
# read in CLC 
clc_raster = raster(x = "01_data_raw/raster100m/CLC2018_CLC2018_V2018_20b2.tif")
# read in and transform ccm2
grcat = st_read("../01_Stream_Network/01_CCM2/03_data_processed/Catchment/2019-06-05_allGRcountires_WGS84.gpkg")
grcat = st_transform(grcat, crs =  "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80")

#Prepare result table. It should have one column for the WSO1_ID and then one
#for each land cover class

lu_codes =
        raster::levels(clc_raster)[[1]][, 1] %>%
        as.character()

WSO1_IDs = unique(grcat$WSO1_ID)

#only in first run 
out.table =
        data.table(matrix(0,
                          ncol = length(lu_codes) + 1,
                          nrow = length(WSO1_IDs)))


names(out.table) = append("WSO1_ID", lu_codes)
out.table[,1] = grcat$WSO1_ID

out.table = readRDS("out.table_327681_2019-06-26 10:44:15 CEST")

# start 14.06.19 @ 14:38

# this loops over each catchment. 

        for (i in 327681:nrow(grcat)) {
                # single out catchment 
                iteration_catchment  = grcat[i,]
                # crop clc to the extend of that catchment 
                cropped = raster::crop(clc_raster, as(iteration_catchment, "Spatial")) 
                # skip catchment if cropped raster is empty 
                if (sum(is.na(cropped@data@values)) == length(cropped@data@values)) next()
                # extract all values of cropped that lie within the catchment 
                extracted = raster::extract(x = cropped, y = as(iteration_catchment, "Spatial"))
                # agian skip catchment if the extracted elements are only NAs
                if (is.na(unique(extracted[[1]])[1])) next()
                
                # how many instances of each class are present? As all cells
                # have equal sizes, the number of cells per category reflects
                # the relatiev area accurately. The number of cells is scaled to
                # a percentage value.
                table_temp = round(table(extracted)/length(extracted[[1]]) * 100, 0)
                # the responding row in out.table is filled with the percentage values 
                out.table = out.table[WSO1_ID == grcat$WSO1_ID[i], (names(table_temp)) := as.list(table_temp)]
                # updater 
                print(i)
                # autosave 
                if (i %% 10000 == 0) {
                  savename = paste0(
                    "out.table",
                    "_",
                    i,
                    "_",
                    Sys.time(),
                    ".RDS"
                  )
                  saveRDS(out.table, savename)
                }
        }

saveRDS(object = out.table,
        file = "cat_clc_all.RDS")
