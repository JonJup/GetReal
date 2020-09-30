### --- create streams per country --- ### 

# GR WP 1 
# 07.05.19
# Create the stream network of all relevant countries 

pacman::p_load(sf, dplyr, here, stringr)

setwd(here("..","01_Stream_Network","01_CCM2"))


# load data  --------------------------------------------------------------

for (i in 1:10) {
   assign(paste0("Cat", i - 1),
          st_read(paste0(
             "02_GPKG/Catchment/LAEA_W200", i - 1, ".gpkg"
          )))
}

data(world, package = "spData")
# assign FR to France, previously NA 
world[which(world$name_long == "France"), ]$iso_a2 = "FR"
# extract relevant countries and transfrom CRS
for(j in c("AT", "FI", "SE", "DE", "ES", "GB", "FR", "HU", "NL", "RO")) {
   assign(j,
          world %>% filter(iso_a2 == j) %>% st_transform(crs = st_crs(Cat0)))
   
}
for (j in c("AT", "FI", "SE", "DE", "ES", "GB", "FR", "HU", "NL", "RO")) {
   for (k in 0:9) {
      assign(paste0("Cat", k, "_", j),
             get(paste0("Cat", k))[lengths(st_intersects(x = get(paste0("Cat", k)), y = get(j))) > 0,])
      if (nrow(get(paste0("Cat", k, "_", j))) == 0)
         rm(list = c(paste0("Cat", k, "_", j)))
   }
}
for(j in c("AT", "FI", "SE", "DE", "ES", "GB", "FR", "HU", "NL", "RO")) {
   rivs <- ls()[str_detect(string = ls(),
                           pattern = regex(paste0("Cat.*", j, "$")))]
   
   n <- length(rivs)
   
   list <- mget(rivs)
   assign(paste0(j, "_rivers"),
          do.call(rbind, list))
   
   st_write(get(paste0(j, "_rivers")), paste0("Countries/", j, "_Catchment.gpkg"))
}


