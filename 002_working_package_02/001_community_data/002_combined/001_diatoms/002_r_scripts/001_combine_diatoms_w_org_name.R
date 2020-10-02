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

pacman::p_load(sf, magrittr, dplyr, data.table, stringr, rlang, magrittr, here, taxize)

setwd(here(... = "002_working_package_02/001_community_data/001_individual_data_sets/"))

# 02.Data input --------------------------------------------------------------

set1 =  st_read("001_ld/003_processed_data/diatoms/06_200305_DIA_Landau.gpkg",                    quiet = TRUE, stringsAsFactors = FALSE) 
set2 =  st_read("002_mi/003_processed_data/diatoms/06_191217_DIA_Miguel_Iglesias.gpkg",  quiet = TRUE, stringsAsFactors = FALSE) 
set3 =  st_read("003_sb/003_processed_data/06_191217_DIA_Saul_Blanco.gpkg",          quiet = TRUE, stringsAsFactors = FALSE) 
set4 =  st_read("004_na/003_processed_data/06_200312_DIA_Naiades.gpkg",                  quiet = TRUE, stringsAsFactors = FALSE) 
set5 =  st_read("006_js/003_processed_data/06_191216_DIA_Janne_Soininen.gpkg",    quiet = TRUE, stringsAsFactors = FALSE) 
set6 =  st_read("009_hd/003_processed_data/06_191216_DIA_Ecosurv.gpkg",          quiet = TRUE, stringsAsFactors = FALSE) 
set7 =  st_read("012_cf/003_processed_data/06_191212_dia_Star.gpkg",              quiet = TRUE, stringsAsFactors = FALSE) 
set8 =  st_read("012_cf/003_processed_data/06_191212_dia_WISER.gpkg",             quiet = TRUE, stringsAsFactors = FALSE) 
set9 =  st_read("017_jg/003_processed_data/06_191210_DIA_Joan_Goma.gpkg",              quiet = TRUE, stringsAsFactors = FALSE) 
set10 = st_read("018_jj/003_processed_data/06_200313_DIA_JJM.gpkg", quiet = TRUE, stringsAsFactors = FALSE) 
set11 = st_read("014_sk/003_processed_data/06_191211_dia_Leonard_Sandin.gpkg",    quiet = TRUE, stringsAsFactors = FALSE) 
set12 = st_read("019_ep/003_processed_data/06_191209_DIA_ediwn_peters.gpkg",        quiet = TRUE, stringsAsFactors = FALSE) 
set13 = st_read("021_mc/003_processed_data/06_191128_DIA_mirella_cimpean.gpkg",    quiet = TRUE, stringsAsFactors = FALSE) 
set14 = st_read("010_ir/003_processed_data/06_191212_DIA_IRSTEA.gpkg",                    quiet = TRUE, stringsAsFactors = FALSE)  

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
saveRDS(diatom_sites_sf, 
         paste0("003_processed_data/001_",
         Sys.Date(),
         "_all_dia_sites_w_org_names.RDS"))

saveRDS(all_dia, 
         paste0("003_processed_data/001_",
         Sys.Date(),
         "_all_dia_data_w_org_names.RDS"))
