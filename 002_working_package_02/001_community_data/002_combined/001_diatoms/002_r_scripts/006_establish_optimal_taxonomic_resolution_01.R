# -------------------------------------------------------- #
### --- establish optimal taxonomic resolution -01- --- ### 
# -------------------------------------------------------- #

# created: 03.08.20
# used   : 03.08 + 18.08
# GetReal Working Package 2 Diatoms 
# Jonathan Jupke 

# Setup -------------------------------------------------------------------

pacman::p_load(here, 
               dplyr, 
               data.table,
               magrittr, 
               taxize, 
               beepr, 
               stringr,
               purrr)

dir_pd = here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/")

# data IO  ----------------------------------------------------------------
data <- readRDS(file.path(dir_pd,"/005_2020-08-17_clean_diatoms_observations.RDS"))
# Prepare data  -----------------------------------------------------------

# This section contains cleaning steps that were results from exploratory
# analysis with the script below.

data[family == "Leptocylindraceae", order := "Chaetocerotales"]
data <- data[species != "Fontigonium rectangulare" | is.na(species)] 
data[genus == "Kobayasiella", c("family", "order") := .("Naviculales incertae sedis", "Naviculales")]

data[species != species_clean]

data = data[,c("species_old", "original_name", "name_source", "species_clean", "species_new", "double_checked", "genus_new2", "genus_check2") := NULL]
data[str_detect(string=species, "Complex"), species := str_replace(string=species, pattern = "Complex", replacement = "complex")]

data[str_detect(string=species, pattern="complex"), sort(unique(genus))]

# delete genus complexes into genera 
data[str_detect(string=species, pattern="complex"), sort(unique(species))]
ch_genus_complexes = c("Adlafia complex", 
                       "Craticula complex",
                       "Cyclostephanos complex",
                       "Discostella complex",
                       "Eunotia complex",
                       "Kobayasiella complex",
                       "Luticola complex",
                       "Mastogloia complex",
                       "Mayamaea complex",
                       "Navicula complex",
                       "Placoneis complex",
                       "Stauroneis complex small",
                       "Thalassiosira complex")
data[species %in% ch_genus_complexes, species := NA]

###### ------ BEGIN ORDER ------ ###### 
n_order       <- length(data[!is.na(order), sort(unique(order))])
unique_orders <- sort(unique(data$order))

level_data_order <- data.table(
        order_name     = character(n_order),
        species        = numeric(n_order),
        genus          = numeric(n_order),
        family         = numeric(n_order),
        order          = numeric(n_order),
        n_observations = numeric(n_order)
)
        
for (i in 1:n_order) {
        
        loop_order <- unique_orders[i]
        
        level_data_order[i, order_name    := loop_order]
        loop_sub <- data[order == loop_order]
        loop_obs <- nrow(loop_sub)
        
        level_data_order[i, species        := round(loop_sub[!is.na(species), .N] / loop_obs * 100,2)]
        level_data_order[i, genus          := round(loop_sub[ is.na(species) & !is.na(genus), .N] / loop_obs * 100,2)]
        level_data_order[i, family         := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family), .N] / loop_obs * 100,2)]
        level_data_order[i, order          := round(loop_sub[ is.na(species) &  is.na(genus) &  is.na(family) & !is.na(order), .N] / loop_obs * 100,2)]
        level_data_order[i, n_observations := nrow(loop_sub)]
}
setorderv(level_data_order, cols = "order_name");beep()
      
# Have a look at the rare cases 
# Bacillariophyta ordo incertae sedis  n = 4 
data[order == "Bacillariophyta ordo incertae sedis", unique(species)]
# Chaetocerotales;  n = 2; marine diatoms 
data[order == "Chaetocerotales", unique(species)]
# Eupodiscales    ; n = 8; good 
data[order == "Eupodiscales", unique(species)]
# Hemiaulales     ; n = 1; the one species "Fontigonium rectangulare" might be
# fossil? Three papers on google scholar. Will kick this species.
data[order == "Hemiaulales", unique(species)]
# Lyrellales      ; n = 2 ; good 
data[order == "Lyrellales", unique(species)]
# Rhabdonematales ; n = 1 ; good
data[order == "Rhabdonematales", unique(species)]
# Stictodiscales  ; n = 1 
data[order == "Stictodiscales", unique(species)]
# Triceratiales   ; n = 5  ; good 
data[order == "Triceratiales", unique(species)]

###### ------ END ORDER ------ ######
###### ------ BEGIN FAMILY ------ ###### 
n_family        <- length(data[!is.na(family), unique(family)])
unique_families <- sort(unique(data$family))
level_data_family <- data.table(
        order_name     = character(n_family),
        family_name    = character(n_family),
        species        = numeric(n_family),
        genus          = numeric(n_family),
        family         = numeric(n_family),
        n_observations = numeric(n_family)
)
        
for (i in 1:n_family) {
        
        loop_family <- unique_families[i]
        
        level_data_family[i, family_name   := loop_family]
        level_data_family[i, order_name    := data[family == loop_family, unique(order)]]
        
        loop_sub <- data[family == loop_family]
        loop_obs <- nrow(loop_sub)
        
        level_data_family[i, species        := round(loop_sub[!is.na(species),.N] / loop_obs * 100,2)]
        level_data_family[i, genus          := round(loop_sub[ is.na(species) & !is.na(genus),.N] / loop_obs * 100,2)]
        level_data_family[i, family         := round(loop_sub[ is.na(species) &  is.na(genus) & !is.na(family),.N] / loop_obs * 100,2)]
        level_data_family[i, n_observations := nrow(loop_sub)]
}
setorderv(level_data_family, cols = c("order_name", "family_name"));beep()

###### ------ END FAMILY ------ ######
###### ------ BEGIN GENUS ------ ######

n_genus        <- length(data[!is.na(genus), unique(genus)])
unique_genera  <- sort(unique(data$genus))
level_data_genus <- data.table(
        order_name     = character(n_genus),
        family_name    = character(n_genus),
        genus_name     = character(n_genus),
        species          = numeric(n_genus),
        genus            = numeric(n_genus),
        n_observations   = numeric(n_genus)
)

for (i in 1:n_genus) {
        
        loop_genus <- unique_genera[i]
        
        level_data_genus[i, genus_name    :=  loop_genus]
        level_data_genus[i, family_name   := data[genus == loop_genus, unique(family)]]
        level_data_genus[i, order_name    := data[genus == loop_genus, unique(order)]]
        
        loop_sub <- data[genus == loop_genus]
        loop_obs <- nrow(loop_sub)
        
        level_data_genus[i, species        := round(loop_sub[!is.na(species),.N] / loop_obs * 100,2)]
        level_data_genus[i, genus          := round(loop_sub[is.na(species) & !is.na(genus),.N] / loop_obs * 100,2)]
        level_data_genus[i, n_observations := nrow(loop_sub)]
        print(paste(i,n_genus))
} 
setorderv(level_data_genus, cols = c("order_name", "family_name", "genus_name")); beep()

###### ------ END GENUS ------ ######

# Save to file  -----------------------------------------------------------

saveRDS(object = level_data_genus,   file = file.path(dir_pd, paste0("006_", Sys.Date(), "_taxon_list_genus.RDS")))
saveRDS(object = level_data_family,  file = file.path(dir_pd, paste0("006_", Sys.Date(), "_taxon_list_family.RDS")))
saveRDS(object = level_data_order,   file = file.path(dir_pd, paste0("006_", Sys.Date(), "_taxon_list_order.RDS")))

