### --- Combine Dia cluster ids --- ### 

#date: 03.03.20

# setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, data.table, magrittr)
setwd("~/01_Uni/03_GetReal/002_WP_02/002_comparing_typologies/")

# load data ---------------------------------------------------------------
lychsol  <- readRDS("003_processed_data/001_2020-03-26_clean_mzb_with_ls_broad_type.RDS")
gloric_  <- readRDS("003_processed_data/001_2020-03-26_clean_mzb_with_gloric_kmeans.RDS")
my_base  <- readRDS("003_processed_data/001_2020-03-26_clean_mzb_with_my_base_typology.RDS")
my_mzb_  <- readRDS("003_processed_data/001_2020-03-26_clean_mzb_with_my_mzb_typology.RDS")

# clean data --------------------------------------------------------------

names(lychsol)[2] <- "WSO1_ID"
names(gloric_)[2] <- "WSO1_ID"
names(my_base)[2] <- "WSO1_ID"
names(my_mzb_)[2] <- "WSO1_ID"

for (i in c("lychsol", "gloric_", "my_base", "my_mzb_")) {
  assign(x = i,
         value = get(i) %>%
           st_drop_geometry() %>%
           setDT())
}

# join --------------------------------------------------------------------

join1 <- lychsol[gloric_, 
   on = "gr_sample_id"]
join1[, i.WSO1_ID := NULL]
join2 <- join1[my_base,
      on = "gr_sample_id"]
join2[, c("i.WSO1_ID", "i.WSO1_ID.1") := NULL]
join3 <- join2[my_mzb_,
      on = "gr_sample_id"]
join3[, i.WSO1_ID := NULL]
join3[, my_mzb_typology := my_dia_typology]
join3[, my_dia_typology := NULL]

# remove zeros 
join3 <- join3[my_mzb_typology != 0 & my_base_typology != 0]

# save to file  -----------------------------------------------------------

saveRDS(object = join3, 
        file = paste0("003_processed_data/002_",Sys.Date(),"_mzb_combined_cluster_ids.RDS"))
