###_______________________________________________###
# ----- Create sitesXspecies for all seasons ----- #
###______________________________________________###

# date: 17.02.2020
# take the big all data matrix for diatoms and subset to different seasons, resolution genus or species. 

# 01. Setup  --------------------------------------------------------------

pacman::p_load(data.table,
               fuzzySim,
               dplyr,
               magrittr,
               stringr
               )

setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/003_processed_data/")

# read in and prepare data ------------------------------------------------

mzb  = readRDS("004c_2020-03-31_mzb_data_low_ls_all.RDS")
setDT(mzb)

# Quality Check -------------------------------------------------------------------------

mzb[species == "NA", species := NA]
mzb[genus == "NA", genus := NA]
mzb[family == "NA", family := NA]
mzb[order == "NA", order := NA]

mzb[, species := str_trim(species, side = "both")]
mzb[, genus   := str_trim(genus, side = "both")]
mzb[, family  := str_trim(family, side = "both")]
mzb[, order   := str_trim(order, side = "both")]


# species -----------------------------------------------------------------

# unique(mzb$species) %>% 
#         sort %>%   
#         unique %>% 
#         .[1400:2000]
# unique(mzb$genus) %>% 
#         sort 
# unique(mzb$family) %>% 
#         sort 



mzb[species == "Molanna angusta", species := "Molanna angustata"]
mzb[genus == "Ferrisia", genus := "Ferrissia"]
mzb[genus == "Ormosia", c("family", "order") := .("Limoniidae", "Diptera")]
mzb[family == "Cordulegasteridae", family := "Cordulegastridae"]
mzb[family == "Grynidae", family := "Gyrinidae"]
mzb[family == "Helophorida", family := "Helophoridae"]
mzb[genus == "Limonia", c("family", "order") := .("Limoniidae", "Diptera")]
mzb[family == "Hydrachnidiae", family := "Hydrachnidae"]
mzb[family == "Libellulinae", family := "Libellulidae"]
mzb[family %in% c("Stratiomydae", "Stratiomyiidae"), family := "Stratiomyidae"]

mzb$family %>% unique %>% stringr::str_detect(pattern = "cae") %>% sum(na.rm = T)
# 1 which is lymnaeidae
mzb$family %>% unique %>% stringr::str_detect(pattern = "nae") %>% sum(na.rm = T)

# 02. Subset  --------------------------------------------------------------
mzb$gr_sample_id %<>% as.character()
general_subset <- mzb[(is.na(year) | year >= 2000)]
spe_all_seasons <- general_subset[!is.na(species)]
gen_all_seasons <- general_subset[!is.na(genus)]
fam_all_seasons <- general_subset[!is.na(family)]

#gen_all_seasons = gen_all_seasons[,   .SD[!duplicated(genus)], by = gr_sample_id]
#fam_all_seasons = fam_all_seasons[, .SD[!duplicated(family)], by = gr_sample_id]

spe_spring <- spe_all_seasons[season == "spring"]
gen_spring <- gen_all_seasons[season == "spring"]
fam_spring <- fam_all_seasons[season == "spring"]

spe_summer <- spe_all_seasons[season == "summer"]
gen_summer <- gen_all_seasons[season == "summer"]
fam_summer <- fam_all_seasons[season == "summer"]

spe_autumn <- spe_all_seasons[season == "autumn"]
gen_autumn <- gen_all_seasons[season == "autumn"]
fam_autumn <- fam_all_seasons[season == "autumn"]

spe_winter <- spe_all_seasons[season == "winter"]
gen_winter <- gen_all_seasons[season == "winter"]
fam_winter <- fam_all_seasons[season == "winter"]

# 03. Drop columns --------------------------------------------------------

bio2.s.all <-  spe_all_seasons[,.(gr_sample_id, species)]
bio2.s.spr <-  spe_spring[,.(gr_sample_id, species)]
bio2.s.sum <-  spe_summer[,.(gr_sample_id, species)]
bio2.s.aut <-  spe_autumn[,.(gr_sample_id, species)]
bio2.s.win <-  spe_winter[,.(gr_sample_id, species)]



# 04. Turn to site X species matrix --------------------------------------------------------
bio2.s.all %<>% splist2presabs(sites.col = 1, sp.col = 2) 
bio2.s.spr %<>% splist2presabs(sites.col = 1, sp.col = 2) 
bio2.s.sum %<>% splist2presabs(sites.col = 1, sp.col = 2) 
bio2.s.aut %<>% splist2presabs(sites.col = 1, sp.col = 2) 
bio2.s.win %<>% splist2presabs(sites.col = 1, sp.col = 2) 

# 05. remove rare species/ sites --------------------------------------------------------
# i.e. < 5 species/ genuses per site or < 5 occurences in dataset 

# # species all 
rare_species_id            <- which(colSums(x = bio2.s.all[,-1]) < 5) + 1
rare_species_names_all     <- names(rare_species_id) %>% str_replace("\\.", " ")
bio2.s.all                 <- bio2.s.all[, -rare_species_id]
low_taxon_site_id          <- which(rowSums(x = bio2.s.all[,-1]) < 5)
low_taxon_site_names_all   <- bio2.s.all$gr_sample_id[low_taxon_site_id]
bio2.s.all                 <- bio2.s.all[-low_taxon_site_id,]

# # species spring 
rare_species_id            <- which(colSums(x = bio2.s.spr[,-1]) < 5) + 1
rare_species_names_spr     <- names(rare_species_id) %>% str_replace("\\.", " ")
bio2.s.spr                 <- bio2.s.spr[, -rare_species_id]
low_taxon_site_id          <- which(rowSums(x = bio2.s.spr[,-1]) < 5)
low_taxon_site_names_spr   <- bio2.s.spr$gr_sample_id[low_taxon_site_id]
bio2.s.spr                 <- bio2.s.spr[-low_taxon_site_id,]

# # species summer 
rare_species_id            <- which(colSums(x = bio2.s.sum[,-1]) < 5) + 1
rare_species_names_sum     <- names(rare_species_id) %>% str_replace("\\.", " ")
bio2.s.sum                 <- bio2.s.sum[, -rare_species_id]
low_taxon_site_id          <- which(rowSums(x = bio2.s.sum[,-1]) < 5)
low_taxon_site_names_sum   <- bio2.s.sum$gr_sample_id[low_taxon_site_id]
bio2.s.sum                 <- bio2.s.sum[-low_taxon_site_id,]

# # species autumn
rare_species_id            <- which(colSums(x = bio2.s.aut[,-1]) < 5) + 1
rare_species_names_aut     <- names(rare_species_id) %>% str_replace("\\.", " ")
bio2.s.aut                 <- bio2.s.aut[, -rare_species_id]
low_taxon_site_id          <- which(rowSums(x = bio2.s.aut[,-1]) < 5)
low_taxon_site_names_aut   <- bio2.s.aut$gr_sample_id[low_taxon_site_id]
bio2.s.aut                 <- bio2.s.aut[-low_taxon_site_id,]

# # species winter 
rare_species_id            <- which(colSums(x = bio2.s.win[,-1]) < 5) + 1
rare_species_names_win     <- names(rare_species_id) %>% str_replace("\\.", " ")
bio2.s.win                 <- bio2.s.win[, -rare_species_id]
low_taxon_site_id          <- which(rowSums(x = bio2.s.win[,-1]) < 5)
low_taxon_site_names_win   <- bio2.s.win$gr_sample_id[low_taxon_site_id]
bio2.s.win                 <- bio2.s.win[-low_taxon_site_id,]

### ---  now genus 

gen_all_seasons <- gen_all_seasons[!species %in% rare_species_names_all]
gen_spring      <- gen_spring[!species %in% rare_species_names_spr]
gen_summer      <- gen_summer[!species %in% rare_species_names_sum]
gen_autumn      <- gen_autumn[!species %in% rare_species_names_aut]
gen_winter      <- gen_winter[!species %in% rare_species_names_win]

gen_all_seasons <- gen_all_seasons[!gr_sample_id %in% low_taxon_site_names_all]
gen_spring      <- gen_spring[!gr_sample_id %in% low_taxon_site_names_spr]
gen_summer      <- gen_summer[!gr_sample_id %in% low_taxon_site_names_sum]
gen_autumn      <- gen_autumn[!gr_sample_id %in% low_taxon_site_names_aut]
gen_winter      <- gen_winter[!gr_sample_id %in% low_taxon_site_names_win]

bio2.g.all <-  gen_all_seasons[,.(gr_sample_id, genus)]
bio2.g.spr <-  gen_spring[,.(gr_sample_id, genus)]
bio2.g.sum <-  gen_summer[,.(gr_sample_id, genus)]
bio2.g.aut <-  gen_autumn[,.(gr_sample_id, genus)]
bio2.g.win <-  gen_winter[,.(gr_sample_id, genus)]

bio2.g.all %<>% splist2presabs(sites.col = 1, sp.col = 2)
bio2.g.spr %<>% splist2presabs(sites.col = 1, sp.col = 2)
bio2.g.sum %<>% splist2presabs(sites.col = 1, sp.col = 2)
bio2.g.aut %<>% splist2presabs(sites.col = 1, sp.col = 2)
bio2.g.win %<>% splist2presabs(sites.col = 1, sp.col = 2)

### --- families 

fam_all_seasons <- fam_all_seasons[!species %in% rare_species_names_all]
fam_spring      <- fam_spring[!species %in% rare_species_names_spr]
fam_summer      <- fam_summer[!species %in% rare_species_names_sum]
fam_autumn      <- fam_autumn[!species %in% rare_species_names_aut]
fam_winter      <- fam_winter[!species %in% rare_species_names_win]

fam_all_seasons <- fam_all_seasons[!gr_sample_id %in% low_taxon_site_names_all]
fam_spring      <- fam_spring[!gr_sample_id %in% low_taxon_site_names_spr]
fam_summer      <- fam_summer[!gr_sample_id %in% low_taxon_site_names_sum]
fam_autumn      <- fam_autumn[!gr_sample_id %in% low_taxon_site_names_aut]
fam_winter      <- fam_winter[!gr_sample_id %in% low_taxon_site_names_win]

bio2.f.all <-  fam_all_seasons[,.(gr_sample_id, family)]
bio2.f.spr <-  fam_spring[,.(gr_sample_id, family)]
bio2.f.sum <-  fam_summer[,.(gr_sample_id, family)]
bio2.f.aut <-  fam_autumn[,.(gr_sample_id, family)]
bio2.f.win <-  fam_winter[,.(gr_sample_id, family)]

bio2.f.all %<>% splist2presabs(sites.col = 1, sp.col = 2)
bio2.f.spr %<>% splist2presabs(sites.col = 1, sp.col = 2)
bio2.f.sum %<>% splist2presabs(sites.col = 1, sp.col = 2)
bio2.f.aut %<>% splist2presabs(sites.col = 1, sp.col = 2)
bio2.f.win %<>% splist2presabs(sites.col = 1, sp.col = 2)

# 06. Check how many sites remain  ----------------------------------------
files = grep(x = ls(), pattern = "bio2")
for (i in files) {
        
        print(
                paste(
                        nrow(
                                get(
                                        ls()[files[i]]
                                        )
                                ),
                        ls()[files[i]]
                        )
        )
}




# 07. Extract site list  --------------------------------------------------

# I save them in seperate files for the look up table.  
sites.bio2.s.all <- as.character(bio2.s.all$gr_sample_id)
sites.bio2.s.spr <- as.character(bio2.s.spr$gr_sample_id)
sites.bio2.s.sum <- as.character(bio2.s.sum$gr_sample_id)
sites.bio2.s.aut <- as.character(bio2.s.aut$gr_sample_id)
sites.bio2.s.win <- as.character(bio2.s.win$gr_sample_id)
# 
sites.bio2.g.all = as.character(bio2.g.all$gr_sample_id)
sites.bio2.g.spr = as.character(bio2.g.spr$gr_sample_id)
sites.bio2.g.sum = as.character(bio2.g.sum$gr_sample_id)
sites.bio2.g.aut = as.character(bio2.g.aut$gr_sample_id)
sites.bio2.g.win = as.character(bio2.g.win$gr_sample_id)
#
sites.bio2.f.all = as.character(bio2.f.all$gr_sample_id)
sites.bio2.f.spr = as.character(bio2.f.spr$gr_sample_id)
sites.bio2.f.sum = as.character(bio2.f.sum$gr_sample_id)
sites.bio2.f.aut = as.character(bio2.f.aut$gr_sample_id)
sites.bio2.f.win = as.character(bio2.f.win$gr_sample_id)

# 08. Save data to file ---------------------------------------------------

bio2.files = ls()[grepl(x = ls(), pattern = "bio2.")]
#bio2.files = bio2.files[-1]

for (i in seq_along(bio2.files)) {
        save.name = paste0(bio2.files[i], "_ls_all.RDS")
        save.file = get(bio2.files[i])
        saveRDS(
                object = save.file,
                file = paste0("001b_speciesXsites_tables_ls_all/",
                              Sys.Date(),
                              save.name)
        )
} 
