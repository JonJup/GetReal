### --------------------------------------------- ###
# --- Compile Physico Chemical data for naiades --- #
### --------------------------------------------- ###

pacman::p_load(data.table, dplyr, sf, tmap)

# create conductivity table 
years = 2005:2019
sites2 = fread("~/Daten/naiades/Naiades_Export_France_Entiere_PC/stations.csv")
site2.2 = sites2[,list(site = CdStationMesureEauxSurface,
                       x.coord = CoordXStationMesureEauxSurface, 
                       y.coord = CoordYStationMesureEauxSurface)]
sites2_sf = st_as_sf(site2.2, coords = c("x.coord", "y.coord"), crs = 2154)
tm_shape(sites2_sf) + tm_dots()

# load pc data - conductivity 
cond_list = vector(mode  = "list")
for (i in 1:15) {
        
        t1 = Sys.time()
        print(paste("start", i, "@", t1))
        year = years[i]
        cond_table = fread(paste0("~/Daten/naiades/analyses_", year,".csv"))
        cond_table2 = cond_table[CdParametre == 1304]
        cond_table3 = cond_table2[, list(site = CdStationMesureEauxSurface,
                                       date = lubridate::ymd(DatePrel),
                                       conductivity = as.numeric(RsAna)
        )]
        cond_list[[i]] =   cond_table3   
        t2 = Sys.time()
        print(paste("end", i, "after", t2 - t1))
}
cond_all = rbindlist(cond_list)

cond_sf = left_join(cond_all, sites2_sf) %>% st_as_sf()

# some sites from the PC measurements still dont have coordinates. Their geometry comuln is "empty". We can find them with st_dimensions 
index = which(is.na(st_dimension(cond_sf)))
cond_sf2 = cond_sf[-index,]

cond_sf_dt = setDT(cond_sf2)
cond_sf_dt2 = unique(cond_sf_dt, by = "site")
cond_sf_dt2 = st_as_sf(cond_sf_dt2)
tm_shape(cond_sf_dt2) + tm_dots()


mean_conductivites = cond_sf2 %>% group_by(site) %>% summarise(mean = mean(conductivity))

cond_sf3 = left_join(cond_sf2, mean_conductivites) %>% 
        setDT %>% 
        unique(by = "site") %>% 
        st_as_sf
str(cond_sf3)
tm_shape(cond_sf3) + tm_dots(col = "mean")

st_write(cond_sf3, "cond_naiades.gpkg")
