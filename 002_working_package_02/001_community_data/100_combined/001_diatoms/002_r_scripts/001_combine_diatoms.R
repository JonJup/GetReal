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

setwd(here())

# 02.Data input --------------------------------------------------------------

set1 =  st_read("../../001_Landau/03_FinalData/06_200305_DIA_Landau.gpkg",                    quiet = TRUE, stringsAsFactors = FALSE) 
set2 =  st_read("../../002_Miguel_Iglesias/03_FinalData/06_191217_DIA_Miguel_Iglesias.gpkg",  quiet = TRUE, stringsAsFactors = FALSE) 
set3 =  st_read("../../003_SAUL_BLANCO/03_FinalData/06_191217_DIA_Saul_Blanco.gpkg",          quiet = TRUE, stringsAsFactors = FALSE) 
set4 =  st_read("../../004_Naiades/03_FinalData/06_200312_DIA_Naiades.gpkg",                  quiet = TRUE, stringsAsFactors = FALSE) 
set5 =  st_read("../../006_Janne_Soininen/03_FinalData/06_191216_DIA_Janne_Soininen.gpkg",    quiet = TRUE, stringsAsFactors = FALSE) 
set6 =  st_read("../../009_Hermann van Dam/03_FinalData/06_191216_DIA_Ecosurv.gpkg",          quiet = TRUE, stringsAsFactors = FALSE) 
set7 =  st_read("../../012_Christian Feld/03_FinalData/06_191212_dia_Star.gpkg",              quiet = TRUE, stringsAsFactors = FALSE) 
set8 =  st_read("../../012_Christian Feld/03_FinalData/06_191212_dia_WISER.gpkg",             quiet = TRUE, stringsAsFactors = FALSE) 
set9 =  st_read("../../017_Joan_Goma/03_FinalData/06_191210_DIA_Joan_Goma.gpkg",              quiet = TRUE, stringsAsFactors = FALSE) 
set10 = st_read("../../018_Jenny Jyrkaenkallio-Mikkola//03_FinalData/06_200313_DIA_JJM.gpkg", quiet = TRUE, stringsAsFactors = FALSE) 
set11 = st_read("../../014_Sandin_Kahlert/03_FinalData/06_191211_dia_Leonard_Sandin.gpkg",    quiet = TRUE, stringsAsFactors = FALSE) 
set12 = st_read("../../019_Edwin Peters/03_FinalData/06_191209_DIA_ediwn_peters.gpkg",        quiet = TRUE, stringsAsFactors = FALSE) 
set13 = st_read("../../021_Mirela Cimpea/03_FinalData/06_191128_DIA_mirella_cimpean.gpkg",    quiet = TRUE, stringsAsFactors = FALSE) 
set14 = st_read("../../010_Irstea/03_FinalData/06_191212_DIA_IRSTEA.gpkg",                    quiet = TRUE, stringsAsFactors = FALSE) 

# clean data  -------------------------------------------------------------

# check crs 
data = ls()
for (i in seq_along(data)) {
        print(paste(i,
                    st_crs(get(data[i]))[[1]]))
        
}
assign(data[4], st_transform(get(data[4]), crs = 4326))
# check for doubles 
set1 %<>%  setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set2 %<>%  setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set3 %<>%  setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set4 %<>%  setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set5 %<>%  setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set6 %<>%  setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set7 %<>%  setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set8 %<>%  setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set9 %<>%  setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set10 %<>% setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set11 %<>% setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set12 %<>% setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set13 %<>% setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))
set14 %<>% setDT %>%  unique(by = c("gr_sample_id", "species", "genus", "family", "order"))

# fix names
names(set1)
set3[,date := as.Date(date)]
set5[,date := as.Date(date)]
set6[,date := as.Date(date)]
set7[,date := as.Date(date)]
set8[,date := as.Date(date)]
set10[,artificial := NULL]
set11[, date := as.Date(date)]
set12[, date := as.Date(date)]
set14[, date := as.Date(date)]
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


# Higher order taxonomy ---------------------------------------------------

# some clean up - insights come from code below but I moved this snipped up so the code runs smoother 
all_dia[species == "Martyana martyi", c("family", "order") := .("Fragilariaceae", "Fragilariales")]
all_dia <- all_dia[species != "Bracteamorpha trainorii"]
# extract all unique orders 
dia_unique_orders <- unique(all_dia$order) %>% sort
loop_hold_classification_results <- list()
# find associated higher taxa levels 

for (i in seq_along(dia_unique_orders)) { 

        loop_var <- dia_unique_orders[i]
        loop_hold_classification_results[[i]] <- classification(sci_id = loop_var, db = "eol")
}

saveRDS(loop_hold_classification_results, paste0("003_processed_data/001_a_", Sys.Date(), "results_higher_taxa_taxize.RDS"))


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

# - clean htot 
higher_taxa_order_table[class_name %in% c("Diatomeae", "Diatomea"), class_name := "Bacillariophyceae"]
unique(higher_taxa_order_table$class_name)

# Chlorophylacae are "green algae". All observations with that order are
# observations of the species Bracteamorpha trainorii and eol confirms that this
# species belongs to the Chlorophyceae. Remove as is no Diatom.
higher_taxa_order_table[class_name  == "Chlorophyceae"]
all_dia[order == "Sphaeropleales", unique(species)]
classification("Bracteamorpha trainorii", db = "eol")

higher_taxa_order_table[5, c("order_name", "class_name") := .("Bacillariophyta ordo incertae sedis", "Bacillariophyceae")]
dia_unique_orders[26]
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
higher_taxa_order_table[26, c("order_name", "class_name") := .("Plagiogrammales", "Bacillariophyceae")]

# Stephanodiscales has no class - only because entry is from another website
# which is badly formated. Why not on eol? Lets have a look at the species.
classification("Stephanodiscales", db = "eol")
all_dia[order == "Stephanodiscales", unique(species)]
classification("Discostella pseudostelligera", db = "eol")
# There seems to be some confuison as to which order the family Stephanodiscaceae bekongs to. 
# I will just stick with Stephanodiscales. The other option would have been Thalassiosirales. 
# Do I have some of those? 
all_dia[order == "Thalassiosirales", unique(species)]
# Yes quite a few and I am afraid that: 
all_dia[genus == "Cyclotella", unique(order)]
all_dia[genus == "Cyclotella", unique(family)]
# three different orders for one genus ... 
# ok this needs some clean up .... 

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
# checked with second run -- seems to have worked! 
# I stick with Algaebase whenever possible! 


## ok do we now have any orders that are not in the higher_taxa_order_table?
higher_taxa_order_table[order_name == "Aulacoseirales Crawford 1990", order_name := "Aulacoseirales"]
higher_taxa_order_table[order_name == "Leptocylindrales Round & Crawford 1990", order_name := "Leptocylindrales"]
unique(all_dia$order) %in% higher_taxa_order_table$order_name

names(higher_taxa_order_table) <- c("order", "subclass", "class")

all_dia <- higher_taxa_order_table[all_dia, on = "order"]
all_dia <- all_dia[,c(4:13,1:3,14:23)]

# extract sites 

diatom_sites = unique(all_dia, by = "gr_sample_id")
diatom_sites %<>% select(gr_sample_id, data.set, geom)
diatom_sites_sf = st_as_sf(diatom_sites)



## test plotting ## 

# library(tmap)
# tmap_mode("view")
# tm_shape(diatom_sites_sf) + tm_dots(col = "data.set")


## ----------- ## 

#all_dia_sf = st_as_sf(all_dia)

st_write(diatom_sites_sf, 
         paste0("003_processed_data/001_",
         Sys.Date(),
         "_all_dia_sites.gpkg"))

saveRDS(all_dia, 
         paste0("003_processed_data/001_",
         Sys.Date(),
         "_all_dia_data.RDS"))
