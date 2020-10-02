# ------------------------------ #
### --- Inspect Thresholds --- ### 
# ------------------------------ #

# 17.06.20
# Invertebrates GetReal 

# I created the complete invertebrate spatial datasets with different threshold
# to determine the finally used taxon name. This will result in different i)
# taxonomical resolutions, ii) number of observations. Their will likely be a
# tradeoff between both. Also including Plathelminthes might come down to what
# effect it has on typical communities. 



# Setup -------------------------------------------------------------------

pacman::p_load(sf, dplyr, magrittr, here, beepr, data.table, tmap, ggplot2)

# data IO  ----------------------------------------------------------------

# observation data sets 
d1595 <- st_read("003_processed_data/002_2020-06-17all_inv_15_95.gpkg")
d1585 <- st_read("003_processed_data/002_2020-06-17all_inv_15_85.gpkg")
d1575 <- st_read("003_processed_data/002_2020-06-17all_inv_15_75.gpkg")
d1195 <- st_read("003_processed_data/002_2020-06-17all_inv_11_95.gpkg")
d1185 <- st_read("003_processed_data/002_2020-06-17all_inv_11_85.gpkg")
d1175 <- st_read("003_processed_data/002_2020-06-17all_inv_11_75.gpkg")

# sites 
d1595_sites <- st_read("003_processed_data/002_2020-06-17all_inv_sites_15_95.gpkg")
d1585_sites <- st_read("003_processed_data/002_2020-06-17all_inv_sites_15_85.gpkg")
d1575_sites <- st_read("003_processed_data/002_2020-06-17all_inv_sites_15_75.gpkg")
d1195_sites <- st_read("003_processed_data/002_2020-06-17all_inv_sites_11_95.gpkg")
d1185_sites <- st_read("003_processed_data/002_2020-06-17all_inv_sites_11_85.gpkg")
d1175_sites <- st_read("003_processed_data/002_2020-06-17all_inv_sites_11_75.gpkg")


# analysis  ---------------------------------------------------------------

str(table(d1595$final_taxon_level))

d1595_t <- data.table(taxon_level = names(table(d1595$final_taxon_level)), 
                           number = as.vector(table(d1595$final_taxon_level)), 
                          percent = as.vector(table(d1595$final_taxon_level)) / nrow(d1595), 
                          thresholds = "1595")

d1585_t <- data.table(taxon_level = names(table(d1585$final_taxon_level)), 
                           number = as.vector(table(d1585$final_taxon_level)), 
                          percent = as.vector(table(d1585$final_taxon_level)) / nrow(d1585), 
                          thresholds = "1585")

d1575_t <- data.table(taxon_level = names(table(d1575$final_taxon_level)), 
                           number = as.vector(table(d1575$final_taxon_level)), 
                          percent = as.vector(table(d1575$final_taxon_level)) / nrow(d1575), 
                          thresholds = "1575")

d1195_t <- data.table(taxon_level = names(table(d1195$final_taxon_level)), 
                           number = as.vector(table(d1195$final_taxon_level)), 
                          percent = as.vector(table(d1195$final_taxon_level)) / nrow(d1195), 
                      thresholds = "1195")

d1185_t <- data.table(taxon_level = names(table(d1185$final_taxon_level)), 
                           number = as.vector(table(d1185$final_taxon_level)), 
                          percent = as.vector(table(d1185$final_taxon_level)) / nrow(d1185),
                      thresholds = "1185")

d1175_t <- data.table(taxon_level = names(table(d1175$final_taxon_level)), 
                           number = as.vector(table(d1175$final_taxon_level)), 
                          percent = as.vector(table(d1175$final_taxon_level)) / nrow(d1175),
                      thresholds = "1175")
        
d_all_t <- rbindlist(list(d1595_t, d1585_t, d1575_t, d1195_t, d1185_t, d1175_t))


d_all_t$thresholds  %<>% factor
d_all_t$taxon_level %<>% factor

d_all_t[thresholds == "1595", total_observations := nrow(d1595)]
d_all_t[thresholds == "1585", total_observations := nrow(d1585)]
d_all_t[thresholds == "1575", total_observations := nrow(d1575)]
d_all_t[thresholds == "1195", total_observations := nrow(d1195)]
d_all_t[thresholds == "1185", total_observations := nrow(d1185)]
d_all_t[thresholds == "1175", total_observations := nrow(d1175)]

d_all_t %>% 
        group_by(taxon_level)  %>%  
        ggplot(aes(x = thresholds, y = number)) + 
        geom_point() + 
        facet_wrap(.~taxon_level)+ 
        scale_y_log10()

d_all_t %>% 
        group_by(taxon_level)  %>%  
        ggplot(aes(x = thresholds, y = total_observations)) + 
        geom_point()



nrow(d1595)
nrow(d1585)
nrow(d1575)
nrow(d1195)
nrow(d1185)
nrow(d1175)

saveRDS(d_all_t, "003_processed_data/003_analysis_of_different_thresholds.RDS")
d_all_t <- readRDS("003_processed_data/003_analysis_of_different_thresholds.RDS")
