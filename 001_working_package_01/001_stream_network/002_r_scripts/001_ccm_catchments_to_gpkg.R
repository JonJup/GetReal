
# Taking catchments form gdb files and turning them into gpkg files. 
# 25.05.19

# Setup -------------------------------------------------------------------

pacman::p_load(dplyr,
               here,
               fs, 
               rgdal,
               stringr,
               sf)

setwd(here())


# Select all files that include .gdb, that is all original CCM2 files. 
files <- dir_ls(path = "001_working_package_01/001_stream_network/001_raw_data/", 
                regexp = "\\.gdb") 

org_names <- names(files) %>% 
        str_split(string = ., pattern = "\\.", n = 2) %>% 
        sapply(function(x) x[[1]]) 

# Loop --------------------------------------------------------------------
# 1 ist LAEA_RiverBasins und passt nicht in das Schema 
for (i in 2:length(files))
{
          CATCHMENTS <- readOGR(dsn = files[i], layer = "CATCHMENTS")
          catchments <- st_as_sf(CATCHMENTS)
          saveRDS(catchments, paste0(org_names[i], ".RDS"))
}

