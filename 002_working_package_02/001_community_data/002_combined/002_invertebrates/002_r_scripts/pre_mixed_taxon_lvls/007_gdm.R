## -- Prepare data for GDM -- ## 

# date: 18.02.20
# Prepare data to fit a GDM 


# 01 setup ----------------------------------------------------------------

pacman::p_load(dplyr, sf, magrittr, parallelDist, dplyr, gdm, data.table)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/")
source("002_r_scripts/998_function_my_gdm.R")

# load data  --------------------------------------------------------------
# species than genus than family 
bio       <- readRDS("003_processed_data/001_speciesXsites_tables/2020-03-24bio2.s.all.RDS")
#bio       <- readRDS("003_processed_data/001_speciesXsites_tables/2020-03-24bio2.g.all.RDS")
#bio       <- readRDS("003_processed_data/001_speciesXsites_tables/2020-03-24bio2.f.all.RDS")

gr_to_wso <- readRDS("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/003_processed_data/003_2020-03-18_mzb_sites_close_to_river.RDS") 
env_complete <- readRDS("~/01_Uni/03_GetReal/001_WP_01/009_create_typology/003_data_processed/2020-02-14_base_data_no_corr.RDS")
env_complete2 <- readRDS("~/01_Uni/03_GetReal/001_WP_01/009_create_typology/003_data_processed/2020-02-14_base_data_for_typology.RDS")
# clean data --------------------------------------------------------------

env_complete
env_complete2 <- env_complete2[, c("WSO1_ID", "y_Coords", "x_Coords")]

env_complete <- env_complete2[env_complete, on = "WSO1_ID"]

bio_ncol <- ncol(bio)  

# turn factor to character 
bio$gr_sample_id <- as.character(bio$gr_sample_id)
gr_to_wso$gr_sample_id <- as.character(gr_to_wso$gr_sample_id)

# drop colmns and sf geom 
gr_to_wso %<>% 
        dplyr::select(gr_sample_id, WSO1_ID) %>% 
        st_drop_geometry()

# join WSO to bio data 
bio2 <- left_join(bio, 
                  gr_to_wso, 
                  by = "gr_sample_id")
#qs 
bio2$WSO1_ID %>% 
        is.na() %>% 
        sum()

# drop categorical variables. GDMs cant handle them. 
env_complete %<>% 
        dplyr::select(-c("dominant_geology"))
env_complete <- env_complete[-which(duplicated(env_complete$WSO1_ID))]
# turn strahler order (a factor) to numeric
# env_complete$strahler <- as.numeric(as.character(env_complete$strahler))

# join environmental data to bio data via WSO 
bio3 <- left_join(bio2, 
                  env_complete, 
                  by = "WSO1_ID")

# na_id <- which(is.na(bio3$strahler))

# bio4 <- bio3[-na_id,]
bio4 <- bio3

rm(bio2, bio, gr_to_wso, bio3, gdmTab); gc()
result_list <- list()
for (i in 1:10) {
        gc()
        print(paste("Computing format table", i, "@", Sys.time()))
gdmTab <-
        my_format_table(
                bioData = bio4[, 1:bio_ncol],
                siteColumn = "gr_sample_id",
                predData = bio4[, c(1, (bio_ncol + 2):(bio_ncol + 17))],
                XColumn = "x_Coords",
                YColumn = "y_Coords",
                sampleSites = .333
        )#; beepr::beep(4)
gc()                
print(paste("Computing model", i, "@", Sys.time()))
gdm.1 <- gdm(gdmTab, geo = T)#; beepr::beep(4)

# same order as gdmTab
env_trans <- env_complete[,-1]
env_trans <- env_trans[,c(2,1,3:16)]

# gdm.transfrom only takes data frames or rasters
env_trans <- as.data.frame(env_trans)
tab_trans <- gdm.transform(gdm.1, env_trans)
# gdm.transform outputs a matrix
tab_trans <- as.data.frame(tab_trans)
tab_trans$WSO1_ID <- env_complete$WSO1_ID
# Quality Controll: duplicates?
result_list[[i]] <- tab_trans
rm(
   gdmTab,
   gdm.1,
   env_trans,
   tab_trans)
gc()
print(paste("Finished Run", i,"@", Sys.time()))
}

result_list_bound <- rbindlist(result_list)

result_list_bound2 <- result_list_bound[, lapply(.SD, mean), by = WSO1_ID]

# all catchments? -- yes 
length(unique(result_list_bound2$WSO1_ID))

saveRDS(object = result_list_bound2,
        file = paste0("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/003_processed_data/gdm",
        Sys.Date(),
        "_species.RDS"))
