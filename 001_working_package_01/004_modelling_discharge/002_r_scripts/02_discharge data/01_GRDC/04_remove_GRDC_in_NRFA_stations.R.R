######################################################################
### --- Remove GRDC Stations which are also availabe from NRFA --- ###
######################################################################

# 11.07.19
# In this script I remove the stations from the GRDC data that are also covered by NRFA


# Setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, tmap, data.table)
setwd(here::here())

# Load both station data sets and the GRDC work data set 
nrfa = st_read("03_Data_Processed/01_Discharge/NRFA/nrfa_locations.gpkg")
grdc = st_read("03_Data_Processed/01_Discharge/GRDC_Stations.gpkg")
grdc_work = fread("03_Data_Processed/01_Discharge/discharge_work.csv")


# Cleaning ----------------------------------------------------------------
# Subset GRDC to Great Britain
grdc %<>% filter(Country == "GB")

# I want to buffer NRFAs Stations by 400m (aproxiamte distance between NRFA and
# GRDC points for same station). NRFA data are saved in a geographic coordinate
# system, so to buffer them by a meter amount I have to transfrom them into a
# projected coordinate system.

# I choose the British National Grid - EPSG 27700
nrfa_BNG = st_transform(nrfa, crs = 27700)

# use the units package to set buffer distance 
bufdis = units::set_units(400, m)

# buffer data 
nrfa_400 = st_buffer(nrfa_BNG, dist = bufdis)

#transform WGS84 so I can intersect them with GRDC data  
nrfa_400_t = st_transform(nrfa_400, crs = 4326)

# Intersect 
grdcXnrfa400 = st_intersection(x = grdc,
                y = nrfa_400_t)

# GRDC numbers of intersected stations 
duplicates = grdcXnrfa400$GRDC_Number

# create new working table without stations in duplicates
grdc_work2 = grdc_work[!(GRDC_Number %in% duplicates)]

# Save to File ------------------------------------------------------------
saveRDS(grdc_work2, "19-07-11_grdc_work_xnrfa.RDS")
