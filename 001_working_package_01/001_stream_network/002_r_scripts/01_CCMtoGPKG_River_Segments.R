
# Taking River Segments form gdb files and turning them into gpkg files. 
# 25.05.19

# Setup -------------------------------------------------------------------

pacman::p_load(dplyr,
               sf)

rstudioapi::getActiveDocumentContext() %>% .$path %>% dirname -> wd
setwd(wd)

# Select all files that include .gdb, that is all original CCM2 files. 
files <- fs::dir_ls(regexp = "\\.gdb") 
org_names <-
      names(files) %>%
      stringr::str_split(string = ., 
                         pattern = "\\.", 
                         n = 2) %>% 
      sapply(function(x) x[[1]]) 

# Loop --------------------------------------------------------------------
# 1 ist LAEA_RiverBasins und passt nicht in das Schema 
for (i in 2:length(files))
{
          #rgdal::ogrListLayers("LAEA_W2000.gdb")
          CATCHMENTS <- rgdal::readOGR(dsn = files[i], layer = "RIVERSEGMENTS")
          catchments <- st_as_sf(CATCHMENTS)
          st_write(catchments, paste0(org_names[i],"_Riversegments", ".gpkg"))
}

