### --------------------------- ###
# --- Combine all diatom data --- #
### --------------------------- ###

# 06.01.20

# Combine single diatom data sets to one big data set

### --- OVERVIEW --- ### 
#01. Setup
#02. Data input
### ---------------- ###


# 01.Setup -------------------------------------------------------------------

pacman::p_load(sf, magrittr, dplyr, data.table, stringr, rlang, magrittr, here, taxize, taxadb)

setwd(here())

# 02.Data input --------------------------------------------------------------
set1 =  readRDS("../../001_Landau/03_FinalData/diatoms/08_2020-06-26_diatoms_landau_w_org_names.RDS") 
set2 =  readRDS("../../002_Miguel_Iglesias/03_FinalData/diatoms/08_2020-06-29_diatoms_MI_w_org_names.RDS") 
set3 =  readRDS("../../003_SAUL_BLANCO/03_FinalData/08_2020-06-29_diatoms_sb_w_org_names.RDS") 
set4 =  readRDS("../../004_Naiades/03_FinalData/diatoms/08_2020-06-29_diatoms_naiades_w_org_names.RDS") 
set5 =  readRDS("../../006_Janne_Soininen/03_FinalData/08_2020-06-29_diatoms_janne_soininen_w_org_names.RDS") 
set6 =  readRDS("../../009_Hermann van Dam/03_FinalData/diatoms/08_2020-06-29_diatoms_ecosurv_w_org_names.RDS")
set7 = readRDS("../../010_Irstea/03_FinalData/08_2020-06-29_diatoms_irstea_w_org_names.RDS")
set8 =  readRDS("../../012_Christian Feld/03_FinalData/diatoms/08_2020-06-29_diatoms_stars_w_org_names.RDS") 
set9 =  readRDS("../../012_Christian Feld/03_FinalData/diatoms/08_2020-06-29_diatoms_WISER_w_org_names.RDS") 
set10 =  readRDS("../../017_Joan_Goma/03_FinalData/08_2020-06-29_diatoms_joan_goma_w_org_names.RDS")
set11 = readRDS("../../018_Jenny Jyrkaenkallio-Mikkola//03_FinalData/08_2020-06-29_diatoms_jjm_w_org_names.RDS") 
set12 = readRDS("../../019_Edwin Peters/03_FinalData/diatoms/08_2020-06-29_diatoms_edwin_peters_w_org_names.RDS") 
set13 = readRDS("../../021_Mirela Cimpea/03_FinalData/diatoms/08_2020-06-29_diatoms_mirella_cimpean_w_org_names.RDS") 
set14 = readRDS("../../014_Sandin_Kahlert/03_FinalData/diatoms/08_2020-06-29_diatoms_sandin_kahlert_w_org_names.RDS") 

# make it spatial  --------------------------------------------------------
set1 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set2 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set3 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set4 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set5 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set6 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set7 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set8 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set9 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set10 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set11 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set12 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set13 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)
set14 %<>% st_as_sf(coords = c("x.coord", "y.coord"), crs = .$EPSG[1]) %>% st_transform(crs = 3035)

# harmonize ---------------------------------------------------------------
all_dia = rbindlist(
        list(
                set1, 
                set2,
                set3,
                set4,
                set5,
                set6,
                set7,
                set8,
                set9,
                set10,
                set11,
                set12,
                set13, 
                set14
        ),
        use.names = T
)

rm(set1, set2, set3, set4, set5, set6 ,set7, set8, set9, set10, set11, set12, set13, set14)

# Higher order taxonomy ---------------------------------------------------
un_gen <- sort(unique(all_dia$genus))
for (i in seq_along(un_gen)) {
        loop_var <- all_dia[genus == un_gen[i]]
        l_fam    <- unique(loop_var$family) %>% length()
        if (l_fam > 1) {
                family_name <- readline(paste0("What is the correct family name of ",  un_gen[i]))
                all_dia[genus == un_gen[i], family := family_name]
        }
}

# checked with second run -- seems to have worked! 
un_fam <- sort(unique(all_dia$family))
for (i in seq_along(un_fam)) {
        loop_var <- all_dia[family == un_fam[i]]
        l_ord    <- unique(loop_var$order) %>% length()
        if (l_ord > 1) {
                order_name <- readline(paste0("What is the correct order name of ",  un_fam[i]))
                all_dia[family == un_fam[i], order := order_name]
        }
}

# extract all unique orders 
dia_unique_orders <- unique(all_dia$order) %>% sort
loop_hold_classification_results <- list()
for (i in seq_along(dia_unique_orders)) { 
        
        loop_var <- dia_unique_orders[i]
        loop_hold_classification_results[[i]] <- classification(sci_id = loop_var, db = "eol")
}

higher_taxa_order_table <-
        data.table(order_name = character(41), 
                   subclass_name = character(41), 
                   class_name = character(41))

for (i in seq_along(loop_hold_classification_results)) {
        loop_var <- loop_hold_classification_results[[i]][[1]]
        if (is.na(loop_var)) next()
        order_name_var <- loop_var[which(loop_var$rank == "order"),1]
        if ("subclass" %in% loop_var$rank) 
                subclass_name_var <- loop_var[which(loop_var$rank == "subclass"),1]  else
                        subclass_name_var <- NA
        if ("class" %in% loop_var$rank) 
                class_name_var <- loop_var[which(loop_var$rank == "class"),1]  else
                        class_name_var <- NA
        if (length(class_name_var)>1) class_name_var <- sort(class_name_var)[1]
        higher_taxa_order_table[i, c("order_name", "subclass_name", "class_name") := .(order_name_var, subclass_name_var, class_name_var)]
        
}

higher_taxa_order_table[class_name %in% c("Diatomeae", "Diatomea"), class_name := "Bacillariophyceae"]
unique(higher_taxa_order_table$class_name)

higher_taxa_order_table[order_name == "Aulacoseirales Crawford 1990", order_name := "Aulacoseirales"]
higher_taxa_order_table[4, c("order_name", "class_name") := .("Bacillariophyta ordo incertae sedis", "Bacillariophyceae")]
higher_taxa_order_table[order_name == "Leptocylindrales Round & Crawford 1990", order_name := "Leptocylindrales"]

dia_unique_orders[23]
# both no success 
classification("Plagiogrammales", db = "eol")
classification("Plagiogrammales", db = "gbif")
#But found on Algaebase.org and it is mentioned in what I can see from Williams
#(2020) Diatom Classifications: What Purpose Do They Serve? in Cristóbal, Balnco
#and Bueno(2020) Modern Trends in Diatom Identification. Springer (
#https://link.springer.com/book/10.1007/978-3-030-39212-3. ) There it is says:
#The classification of Cox differs from Round et al. [19] as it includes the new
#Order Plagiogrammales (as a “nom. prov.;” validly published later by Gogorev
#and Stepanova)
higher_taxa_order_table[23, c("order_name", "class_name") := .("Plagiogrammales", "Bacillariophyceae")]

# Stephanodiscales has no class - only because entry is from another website
all_dia[order == "Stephanodiscales", class := "Bacillariophyceae"]
names(higher_taxa_order_table) <- c("order", "subclass", "class")

all_dia <- higher_taxa_order_table[all_dia, on = "order"]
all_dia <- all_dia[,c(4:14,1:3,15:19)]


# extract sites 
diatom_sites = unique(all_dia, by = "gr_sample_id")
diatom_sites %<>% select(gr_sample_id, data.set, geometry)
diatom_sites_sf = st_as_sf(diatom_sites)

# save to file ------------------------------------------------------------
st_write(diatom_sites_sf, 
         paste0("003_processed_data/001_",
         Sys.Date(),
         "_all_dia_sites_w_org_names.gpkg"))

saveRDS(all_dia, 
         paste0("003_processed_data/001_",
         Sys.Date(),
         "_all_dia_data_w_org_names.RDS"))
