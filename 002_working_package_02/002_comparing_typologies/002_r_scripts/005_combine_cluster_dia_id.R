### --- Combine Dia cluster ids --- ### 

#date: 03.03.20

# setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, data.table, magrittr)
setwd("~/01_Uni/03_GetReal/002_WP_02/002_comparing_typologies/")

# load data ---------------------------------------------------------------
ls <- readRDS("003_processed_data/2020-03-03_clean_dia_with_ls_broad_type.RDS")
gloric <- readRDS("003_processed_data/2020-03-03_clean_dia_with_gloric_kmeans.RDS")
my_base <- readRDS("003_processed_data/2020-03-03_clean_dia_with_my_base_typology.RDS")
my_dia <- readRDS("003_processed_data/2020-03-03_clean_dia_with_my_dia_typology.RDS")

# clean data --------------------------------------------------------------
ls %<>% 
        st_drop_geometry() %>% 
        setDT()
gloric %<>% 
        st_drop_geometry() %>% 
        setDT()
my_base %<>% 
        st_drop_geometry() %>% 
        setDT()
my_dia %<>% 
        st_drop_geometry() %>% 
        setDT()

# join --------------------------------------------------------------------

join1 <- ls[gloric, 
   on = "gr_sample_id"]
join2 <- join1[my_base,
      on = "gr_sample_id"]
join3 <- join2[my_dia,
      on = "gr_sample_id"]



# save to file  -----------------------------------------------------------

saveRDS(object = join3, 
        file = paste0("003_processed_data/",Sys.Date(),"combined_cluster_ids.RDS"))
