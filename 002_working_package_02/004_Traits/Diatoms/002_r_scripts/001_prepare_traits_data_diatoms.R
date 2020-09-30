##### ------------------------------------------ ####
##### ----- Prepare trait data for diatoms ----- #### 
##### ------------------------------------------ ####

# date written: 21.09.20 
# date changed: 22.09.20
# date used   : 21.09.20 + 22. 

# Get Real WP2 Diatoms Traits 

# Prepare the original trait data set. That means reduce to rows which represent
# taxa in the TAs and drop columns that I do not need. Also join traits to TAs 


# setup -------------------------------------------------------------------

pacman::p_load(data.table, dplyr, here,  magrittr,  purrr, readxl, stringr)
setwd(here())

# load data  --------------------------------------------------------------

# diatom typical assemblages 
dt_ta       <- readRDS("../001_Community Data/100_Combined/001_Diatoms/003_processed_data/011_2020-09-21_daitom_typical_assemblages.RDS")
# trait data base 
dt_o_traits <- read_excel("Diatoms/001_raw_data/2019-02-12-Diat.barcode-release-version 7.1.xlsx", sheet = 1)
# reference table: which original names were mapped to new names 
dt_old_new  <- read_excel("../001_Community Data/100_Combined/001_Diatoms/2020-09-21_new_and_old_names.xlsx")

# prepare data  -----------------------------------------------------------
setDT(dt_o_traits)
setDT(dt_old_new)

# remove dots between genus and species 
dt_ta[, taxon2 := str_replace_all(taxon, pattern = "\\.", "\\ ")]
# remove slashes
dt_old_new[, species := str_replace_all(string = species, pattern ="\\/", replacement = "\\ ")]

# subset data -------------------------------------------------------------

# drop several columns of the trait db 
vec_columns_to_keep <- c(26,28:96)
dt_sub_traits       <- dt_o_traits[,vec_columns_to_keep, with = F]
rm(vec_columns_to_keep, dt_o_traits);gc()

# unique taxa to loop over 
vec_ta_taxa <- unique(dt_ta$taxon2)

# Loop that takes assigns the rows in dt_sub_traits that correspond to a taxon to dt_ta to the column trait_id of dt_ta
for (i in seq_along(vec_ta_taxa)) {
        loop_name <- vec_ta_taxa[i]
        if (loop_name %in% dt_sub_traits$Species) {
                id <- which(dt_sub_traits$Species == loop_name)
                id <- paste(id, collapse = ",")
                dt_ta[taxon2 == loop_name, trait_id := id]
        }
        rm(loop_name, id, i);gc()
        
}

# This did not assign trait_ids to all taxa. First batch can be fixed by using the look up table dt_old_new
# 161 remain  

vec_ta_taxa <- dt_ta[is.na(trait_id), unique(taxon2)]
for (i in seq_along(vec_ta_taxa)) {
        loop_name <- vec_ta_taxa[i]
        if (loop_name %in% dt_old_new$species) {
        original_names <- dt_old_new[species == loop_name]
        original_names <- original_names$original_name
        if (any(original_names %in% dt_sub_traits$Species)) {
        id <- which(dt_sub_traits$Species %in%  original_names)
        id <- paste(id, collapse = ",")
        dt_ta[taxon2 == loop_name, trait_id := id]
        }
        }
        rm(loop_name, original_names, id, i);gc()
}

# Now there are some differences in how names are written. Also in some cases if have to go to the genus level. 
vec_ta_taxa <- dt_ta[is.na(trait_id), unique(taxon2)]
# 114 remain 


dt_ta[taxon2 == "Achnanthes rupestoides kryophila broenlundense", c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Achnanthes"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Achnanthidium atomoides",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Achnanthidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Achnanthidium lauenburgianum",                  c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Achnanthidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Achnanthidium linearioide",                      c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Achnanthidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Amphora micra",                                  c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Amphora"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Brachysira garrensis",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Brachysira"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Brachysira styriaca",                            c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Brachysira"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Brachysira zellensis",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Brachysira"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Caloneis bacillum Complex",                      c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Caloneis"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Caloneis tenuis Complex",                        c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Caloneis"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Cocconeis euglypta",                             c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Cocconeis"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Cocconeis pseudolineata",                        c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Cocconeis"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Cymbella kappii",                                c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Cymbella"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Cymbella tridentina",                            c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Cymbella"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Cymbella tumidula",                              c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Cymbella"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Cyclotella wuethrichiana",                       c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species%in% unique(dt_sub_traits[str_detect(Species, "Cyclotella"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Diatoma anceps",                                 c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Diatoma"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Diatoma mesodon",                                c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Diatoma"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Dimeregramma minor",                             c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Dimeregramma"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Delphineis surirella",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Delphineis"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Encyonema gaeumannii perpusillum",               c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Encyonema"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Encyonema geisslerae",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Encyonema"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Encyonema gracile luna",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Encyonema"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Encyonema simile",                               c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Encyonema"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Epithemia goeppertiana",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Epithemia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Eunotia subarcuatoides",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Eunotia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Eunotia subtilissima",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Eunotia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Eunotia incisa Complex",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Eunotia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Eunotia naegelii",                               c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Eunotia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Eunotia parallela Complex",                      c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Eunotia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Eunotia praerupta Complex",                      c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Eunotia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Eunotia septentrionalis",                        c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Eunotia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Fallacia crassicostata",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Fallacia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Fallacia muraloides",                            c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Fallacia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Fallacia subhamulata helensis",                  c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Fallacia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Fragilaria arcus",                               c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Fragilaria"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Fragilaria heidenii",                            c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Fragilaria"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Fragilaria incognita",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Fragilaria"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Gomphoneis eriense",                             c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Gomphoneis"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Gomphonema olivaceum olivaceoides",              c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Gomphonema"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Gomphonema",                                     c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Gomphonema"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Gomphonema bavaricum",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Gomphonema"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Halamphora thumensis",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Halamphora"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Hantzschia abundans",                            c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Hantzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Humidophila perpusilla",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Humidophila"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Humidophila schmassmannii",                      c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Humidophila"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Karayevia amoena nitidiformis",                  c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Karayevia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Lindavia rossii complex",                        c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Lindavia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Luticola kotschyi",                              c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Luticola"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Luticola ventriconfusa",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Luticola"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Mayamaea alcimonica",                            c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Mayamaea"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Navicula gottlandica",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Navicula"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Navicula reichardtiana caterva",                 c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Navicula"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Navicula rotunda",                               c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Navicula"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Navicula venezuelensis",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Navicula"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Neidium iridis Complex",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Neidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Nitzschia angustiforaminata",                    c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Nitzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Nitzschia sociabilis",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Nitzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Nitzschia bavarica Complex",                     c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Nitzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Nitzschia flexa",                                c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Nitzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Nitzschia frequens",                             c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Nitzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Nitzschia inducta",                              c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Nitzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Nitzschia labella",                              c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Nitzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Nitzschia perspicua",                            c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Nitzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Nitzschia subcapitellata",                       c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Nitzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Nitzschia valdecostata",                         c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Nitzschia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Pinnularia appenticulata perirrorata silvatica", c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Pinnularia"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Plagiogramma laeve",                             c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Plagiogramma"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Planothidium holstii",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Planothidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Planothidium reichardtii",                       c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Planothidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Psammothidium curtissimum",                      c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Psammothidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Psammothidium marginulatum scoticum lacus vulcani levenderi", c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Psammothidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Psammothidium rossii altaica",                   c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Psammothidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Psammothidium subatomoides",                     c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Psammothidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Psammothidium ventrale Complex",                 c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Psammothidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Pseudostaurosira neoelliptica",                  c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Pseudostaurosira"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Reimeria uniseriata",                            c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Reimeria"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Rossithidium linearis",                          c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Rossithidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Rossithidium nodosum",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Rossithidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Rossithidium petersennii",                       c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Rossithidium"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Sellaphora submuralis Complex",                  c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Sellaphora"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Stauroneis separanda",                           c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Stauroneis"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Staurosira punctiformis",                        c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Staurosira"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Stephanodiscus niagarae complex",                c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Stephanodiscus"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Tabellaria fenestrata Complex",                  c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Tabellaria"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Tabellaria quadriseptata Complex",               c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Tabellaria"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Thalassiosira bramaputrae",                      c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Thalassiosira"), Species])) , collapse = ","), TRUE)]
dt_ta[taxon2 == "Thalassiosira decipiens",                        c("trait_id", "ll_traits") := .(paste(which(dt_sub_traits$Species %in% unique(dt_sub_traits[str_detect(Species, "Thalassiosira"), Species])) , collapse = ","), TRUE)]



dt_ta[taxon2 == "Conticribra guillardii",                         trait_id := paste(which(dt_sub_traits$Species %in% c("Thalassiosira guillardii")), collapse = ",")]
dt_ta[taxon2 == "Discostella complex",                            trait_id := paste(which(dt_sub_traits$Species %in% c("Discostella pseudostelligera", "Discostella stelligera", "Discostella woltereckii")), collapse = ",")]
dt_ta[taxon2 == "Encyonema silesicacum minutum lange bertalotii", trait_id := paste(which(dt_sub_traits$Species %in% c("Encyonema silesiacum", "Encyonema minutum", "Encyonema ventricosum", "Encyonopsis minuta", "Encyonopsis subminuta")), collapse = ",")]
dt_ta[taxon2 == "Eolimna minima seminulum atomoides",             trait_id := paste(which(dt_sub_traits$Species %in% c("Sellaphora joubaudii", "Sellaphora minima", "Sellaphora seminulum")), collapse = ",")]
dt_ta[taxon2 == "Fragilaria construens pseudoconstruens",         trait_id := paste(which(dt_sub_traits$Species %in% c("Staurosira construens")), collapse = ",")]
dt_ta[taxon2 == "Gomphonema parvulum Complex",                    trait_id := paste(which(dt_sub_traits$Species %in% c("Gomphonema parvulum", "Gomphonema exilissimum", "Gomphonema tergestinum")), collapse = ",")]
dt_ta[taxon2 == "Gomphonema pumilum complex",                     trait_id := paste(which(dt_sub_traits$Species %in% c("Gomphonema pumilum var. pumilum")), collapse = ",")]
dt_ta[taxon2 == "Melosira varians Complex",                       trait_id := paste(which(dt_sub_traits$Species %in% c("Melosira varians")), collapse = ",")]
dt_ta[taxon2 == "Navicula cryptotenella cryptotenelloides",       trait_id := paste(which(dt_sub_traits$Species %in% c("Navicula cryptotenella", "Navicula cryptotenelloides", "Navicula phyllepta")), collapse = ",")]
dt_ta[taxon2 == "Navicula lanceolata complex",                    trait_id := paste(which(dt_sub_traits$Species %in% c("Fallacia sp.")), collapse = ",")]
dt_ta[taxon2 == "Navicula margalithii tripunctata",               trait_id := paste(which(dt_sub_traits$Species %in% c("Navicula tripunctata")), collapse = ",")]
dt_ta[taxon2 == "Navicula menisculus antonii",                    trait_id := paste(which(dt_sub_traits$Species %in% c("Navicula antonii")), collapse = ",")]
dt_ta[taxon2 == "Navicula notha leptostriata",                    trait_id := paste(which(dt_sub_traits$Species %in% c("Navicula veneta")), collapse = ",")]
dt_ta[taxon2 == "Nitzschia dissipata recta Complex",              trait_id := paste(which(dt_sub_traits$Species %in% c("Nitzschia dissipata", "Nitzschia dissipata var. media", "Nitzschia recta")), collapse = ",")]
dt_ta[taxon2 == "Nitzschia fonticola Complex",                    trait_id := paste(which(dt_sub_traits$Species %in% c("Nitzschia fonticola")), collapse = ",")]
dt_ta[taxon2 == "Nitzschia inconspicua Complex",                  trait_id := paste(which(dt_sub_traits$Species %in% c("Nitzschia inconspicua")), collapse = ",")]
dt_ta[taxon2 == "Nitzschia lacuum alpina bryophila  ",            trait_id := paste(which(dt_sub_traits$Species %in% c("Nitzschia supralitorea")), collapse = ",")]
dt_ta[taxon2 == "Nitzschia palea paleacea",                       trait_id := paste(which(dt_sub_traits$Species %in% c("Nitzschia palea", "Nitzschia paleacea", "Nitzschia paleaeformis")), collapse = ",")]
dt_ta[taxon2 == "Nitzschia pura linearis Complex",                trait_id := paste(which(dt_sub_traits$Species %in% c("Nitzschia gracilis", "Nitzschia linearis")), collapse = ",")]
dt_ta[taxon2 == "Pinnularia alpina lata borealis complex",        trait_id := paste(which(dt_sub_traits$Species %in% c("Pinnularia borealis")), collapse = ",")]
dt_ta[taxon2 == "Rhoicosphenia",                                  trait_id := paste(which(dt_sub_traits$Species %in% c("Rhoicosphenia abbreviata", "Rhoicosphenia stoermeri")), collapse = ",")]
dt_ta[taxon2 == "Stenopterobia",                                  trait_id := paste(which(dt_sub_traits$Species %in% c("Stenopterobia curvula")), collapse = ",")]

# calls to work out the manual fixing above 
# dt_sub_traits[str_detect(Species, "Eolimna"), Species] %>% unique
# vec_ta_taxa %<>% sort()
# vec_ta_taxa[97]

vec_ta_taxa <- dt_ta[is.na(trait_id), sort(unique(taxon2))]
unique_taxa <- unique(dt_ta$taxon)

# add numeric trait columns to dt_ta 
dt_ta[, c("IPS_stenoecy_degree", 
          "IPS_pollution_sensitivity",
          "TDI_of_kelly_stenoecy_degree",
          "TDI_of_kelly_pollution_sensitivity",
          "salinity_value",
          "nitroge_uptake_value",
          "oxygen_requirement_value",
          "saprobic_value",
          "trophic_value",
          "aerophily_value",
          "index_of_hoffman_stenoecy_degree",
          "index_of_hoffman_pollution_sensitivity",
          "TDI_of_kelly_stenoecy_degree_modifed_by_Kahlert",
          "TDI_of_kelly_pollution_sensitivity_modifed_by_Kahlert",
          "length",
          "width",
          "thickness",
          "biovolume",
          "size_class",
          "length/width_ratio",
          "benthic", 
          "planctonic", 
          "epipsamic",
          "epipelic",
          "marine",
          "mobile",
          "pioneer",
          "not_attached",
          "attached",
          "adnate",
          "pedunculate",
          "pad",
          "stalk",
          "colonial",
          "non_colonial", 
          "mucous_tubule_colony", 
          "chain_colony",
          "zig-zag_colony",
          "rosette_colony",
          "ribbon_colony",
          "stellate_colony",
          "arbscular_colony",
          "other_colony",
          "high_profile_guild",
          "low_profile_guild",
          "motile_guild",
          "euplanctonic_guild") := numeric(0)]

# add character coded trait columns to dt_ta  
dt_ta[, c(
          "chloroplast_shape",
          "shape"
) := character(0)]
          
# function to compute the mode of a character vector
my_mode <- function(x) {
        #x <- pull(x)
        ux <- unique(x)
        out <- ux[which.max(tabulate(match(x, ux)))]
        return(out)
}


# fix shape levels in dt_sub_traits 
dt_sub_traits[,unique(Forme)]
dt_sub_traits[Forme == "omnidia", Forme := "Omnidia"]
dt_sub_traits[Forme == "box", Forme := "Box"]
dt_sub_traits[Forme == "tub", Forme := "Tub"]
dt_sub_traits[Forme == "0", Forme := NA]
dt_sub_traits[Forme == "Omnidia - Same as U ulna", Forme := "Omnidia"]


# loop that takes the median of all numeric trait values and the mode of all
# character trait values that are assigned to a taxon.

for (i in seq_along(unique_taxa)) {
        loop_taxon    <- unique(dt_ta$taxon)[i]
        loop_trait_id <- dt_ta[taxon == loop_taxon, trait_id]
        if (all(is.na(loop_trait_id))) {next()}
        if (length(loop_trait_id) > 1) {
                if (all(loop_trait_id == loop_trait_id[1])){
                        loop_trait_id <- loop_trait_id[1]
                }
                
        }
        
        
        #loop_trait_id <- unique(dt_ta$trait_id)[i] 
        loop_trait_id <- str_replace_all(loop_trait_id, pattern = ",", replacement = "-")
        loop_trait_id <- str_split(loop_trait_id, pattern = "-") %>% unlist %>% as.integer()
        tb_traits_num <- dt_sub_traits[loop_trait_id, c(11:24, 28:32,34,43:46,48:70), with = F] %>% 
                tibble() %>% 
                mutate(IPSVV = as.numeric(IPSVV)) %>% 
                mutate(IPSS = as.numeric(IPSS)) %>% 
                map_df(median, na.rm = TRUE)
        
        tb_traits_str <-  dt_sub_traits[loop_trait_id, c(26,33), with = F] %>% 
                tibble() %>% 
                map_df(my_mode)
        
        # assign new traits to dt_ta 
        dt_ta[taxon == loop_taxon,c(11:57) := tb_traits_num]
        dt_ta[taxon == loop_taxon,c(58:59) := tb_traits_str]
        
        # end of loop 
        print(i)
        rm(loop_taxon, i, loop_trait_id, tb_traits_num, tb_traits_str);gc()
        
}


# Reshaping data  ---------------------------------------------------------

dt_ta_mod  <- dt_ta[length != 0]

dt_ta_mod <- dt_ta_mod[benthic + planctonic != 0, ]
dt_ta_mod <- dt_ta_mod[taxon != "Skeletonema.subsalsum"]
# are any traits never true? 
colSums(dt_ta_mod[,11:54])
# yes: stellate_colony, other_colony, marine (after removing Skeletonema.subsalsum). 
# These columns can be dropped. 
dt_ta_mod[,c("marine", "other_colony", "stellate_colony") := NULL]

# review levels of string based traits
dt_ta_mod[, sort(unique(chloroplast_shape))]
# I drop this variable, because it is strangely formatted. 
dt_ta_mod[, chloroplast_shape := NULL]

dt_ta_mod[, sort(unique(shape))]

# The two entries that have NA for shape both come from the same genus
# (Planothidium). While NA is the most common entry for taxa associated with
# taht genus, there are some non-NA entries and among them Omnidia ist the most
# common.
dt_ta_mod[is.na(shape), shape := "Omnidia"]



# save to file  -----------------------------------------------------------
saveRDS(dt_ta_mod, paste0("Diatoms/003_processed_data/", Sys.Date(), "_diatom_ta_w_traits.RDS"))

 
if (readline("Remove all? ") == "yes") rm(list=ls()) 
