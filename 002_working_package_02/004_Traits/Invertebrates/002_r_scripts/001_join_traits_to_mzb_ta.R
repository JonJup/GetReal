##### ------------------------------------------------------ ####
##### ----- Join trait data to MZB typical assemblages ----- #### 
##### ------------------------------------------------------ ####

# date written: 30.09.20 
# date changed: 
# date used   : 30.09.20  

# Get Real WP2 macroinvertebrate Traits 

# Prepare the original trait data set. That means reduce to rows which represent
# taxa in the TAs and drop columns that I do not need. Also join traits to TAs 

#TODO investigate strange taxa 
# setup -------------------------------------------------------------------

pacman::p_load(
 data.table,
 here,
 stringr
)

setwd(here())

# load data  --------------------------------------------------------------

# macroinvertebrate typical assemblages 
dt_ta       <- readRDS("002_WP2/001_community_data/100_combined/002_invertebrates/003_processed_data/08_2020-09-30_macroinvertebrate_typical_assemblages.RDS")
# trait data base 
dt_o_traits <- readRDS("002_WP2/004_Traits/Invertebrates/001_raw_data/stefan_2020_09/Traits_Stefan/Trait_freshecol_2020_pp.rds")


# carpet data  ------------------------------------------------------------
# replace "." in taxon names with a space
dt_ta[,taxon := str_replace(taxon, pattern = "\\.", replacement = " ")]
# drop columns 
dt_s_ta <- dt_ta[, c("taxon", "group")]

# Voltinism trait contains thermal conditions, change every string to 1

col <- grep("(?=volt.*)(?!.*tachet)",
            names(dt_o_traits), value = TRUE, perl = TRUE)

dt_o_traits[, (col) := lapply(.SD, function(y) {
        ifelse(!is.na(y), 1, NA)
}), .SDcols = col] 

# prepare join  -----------------------------------------------------------
# which taxa are in the trait db
dt_s_ta[, trait_db := (taxon %in% dt_o_traits$species) + 
              (taxon %in% dt_o_traits$genus) + 
              (taxon %in% dt_o_traits$family)]

unique(dt_s_ta, by = "taxon") %>% .$trait_db %>% sum()
unique(dt_s_ta, by = "taxon") %>% .$trait_db %>% length()

# 69 of 79 taxa are represented in the trait db. This is corresponds to 87.3% of unique taxa. 

dt_s_ta[, sum(trait_db)/.N]

# 92% of taxa actually have traits. Hence taxa without trait data are slightly less
# common (i.e. occur in fewer TAs) that those with trait data.

# looking for best alternative representation for taxa. For this I copy the data
# table so that subsequent analyses can be done on data with and without alternative representations.

dt_s_ta[trait_db == 0, unique(taxon)]
dt_a_traits <- copy(dt_o_traits)
## -- Esperiana esperi -- ##
# DONE

# Belongs to the Family Melanopsidae, for which there are two entries. Both are
# for another Genus (Holandriana).

id <- which(str_detect(string = dt_a_traits$family, pattern = "Melanopsidae"))
dt_a_traits <- rbindlist(list(dt_a_traits, dt_a_traits[id[2], ]))
id <- which(str_detect(string = dt_a_traits$family, pattern = "Melanopsidae"))
dt_a_traits[id[3], species := "Esperiana esperi"]

## -- Hippolyte desmaresti -- ##
# DONE 

# Atyaephyra desmaresti is a synonym for  Hippolyte desmaresti and included in
# the trait data base

dt_a_traits[species == "Atyaephyra desmaresti", species := "Hippolyte desmaresti"]

## -- Microcolpia daudebartii -- ##
# DONE 

# Belongs to the Family Melanopsidae. I use the entry from Esperiana esperi. 

id <- which(dt_a_traits$species == "Esperiana esperi")
dt_a_traits <- rbindlist(list(dt_a_traits, dt_a_traits[id, ]))
id <- which(dt_a_traits$species == "Esperiana esperi")
dt_a_traits[id[2], species := "Microcolpia daudebartii"]

## -- Nigrobaetis niger -- ## 
# DONE 

# Baetis niger is a synonym for Nigrobaetis niger and included in
# the trait data base

dt_a_traits[species == "Baetis niger", species := "Nigrobaetis niger"]

## -- Peregriana peregra -- ## 
# DONE 

# A Genus belonging to the family Lymnaeidae. There is an entry on the family
# level which will I assign to this genus.

id <- which(dt_a_traits$family == "Lymnaeidae" & is.na(dt_a_traits$species) & is.na(dt_a_traits$genus))
dt_a_traits <- rbindlist(list(dt_a_traits, dt_a_traits[id, ]))
id <-which(dt_a_traits$family == "Lymnaeidae" & is.na(dt_a_traits$species) & is.na(dt_a_traits$genus))
dt_a_traits[id[2], species := "Peregriana peregra"]

## -- Odhneripisidium moitessierianum -- ## 
# DONE 

# Pisidium moitessierianum is a synonym and included in the trait data. 

dt_a_traits[species == "Pisidium moitessierianum", species := "Odhneripisidium moitessierianum"]

## -- Chelicorophium -- ## 
# DONE 

# Chelicorophium is genus in the family of Corophiidae for which there is an
# entry in the trait data base. Therefore I assign the family levl traits to the
# genus. 
dt_o_traits[str_detect(string = family, pattern = "Corophiidae")]
dt_a_traits <- rbindlist(list(dt_a_traits, dt_a_traits[is.na(species) & genus == "Corophium"]))
id          <- which(is.na(dt_a_traits$species) & dt_a_traits$genus == "Corophium")
dt_a_traits[id[2], genus := "Chelicorophium"]
rm(id); gc()

## -- Hydraenida -- ##
# DONE  

# Potentially a typo ? Look back at former data. -> Ok so there is a genus
# called Hydaenida. Judging by the data on gbif and papers I found on google
# scholar it seems to be a genus that occurs is South America and South East
# Asia. Miguel Iglesias apparently thinks he has found it in Spain. But since I
# can find no data indicating that the taxon occurs there I go down the
# taxonomic ladder one rung to the family Hydraenidae.

dt_s_ta[taxon == "Hydraenida", taxon := "Hydraenidae"]

## -- Micronectidae -- ## 
# DONE 

# Micronectidae used to be regarded as a subfamily of Corixidae (see
# https://doi.org/10.1016/j.ijbiomac.2018.07.191) but is now regarded as a
# separate family. The two families are differentiated by morphological features
# (see Nieser (2002) in previous reference) but these do not seem pertinent to
# the traits considered here. It is thus fair to assume equal traits at the
# family level.

id <- which(str_detect(string = dt_a_traits$taxon_cp, pattern = "Corixidae"))
dt_a_traits <- rbindlist(list(dt_a_traits, dt_a_traits[id, ]))
id <- which(str_detect(string = dt_a_traits$taxon_cp, pattern = "Corixidae"))
dt_a_traits[id[2], family := "Micronectidae"]

## -- Oligochaeta -- ## 
# DONE

# There was an Oligochaeta gen. sp. entry in taxon_cp
dt_o_traits[str_detect(string = taxon_cp, pattern = "Oligochaeta")]
dt_a_traits[str_detect(string = taxon_cp, pattern = "Oligochaeta"), suborder := "Oligochaeta"]


# recheck availability  ---------------------------------------------------

dt_s_ta[, trait_db := (taxon %in% dt_a_traits$species) + 
                (taxon %in% dt_a_traits$genus) + 
                (taxon %in% dt_a_traits$family) + 
                (taxon %in% dt_a_traits$suborder)]
# none left 
dt_s_ta[trait_db == 0]

dt_ta_species <- dt_s_ta[1:18,]
dt_ta_genus   <- dt_s_ta[19:150,]
dt_ta_family  <- dt_s_ta[151:172,]
dt_ta_suboder <- dt_ta_family[taxon == "Oligochaeta"]
dt_ta_family  <- dt_ta_family[taxon != "Oligochaeta"]

dt_spe_traits <- dt_a_traits[!is.na(species)]
dt_gen_traits <- dt_a_traits[ is.na(species) & !is.na(genus)]
dt_fam_traits <- dt_a_traits[ is.na(species) &  is.na(genus) & !is.na(family)]
dt_sub_traits <- copy(dt_a_traits)

names(dt_spe_traits)[1] <- "taxon"
names(dt_gen_traits)[2] <- "taxon"
names(dt_fam_traits)[4] <- "taxon"
names(dt_sub_traits)[6] <- "taxon"

dt_spe_traits <- dt_spe_traits[dt_ta_species, on = "taxon"]
dt_gen_traits <- dt_gen_traits[dt_ta_genus,   on = "taxon"]
dt_fam_traits <- dt_fam_traits[dt_ta_family,  on = "taxon"]
dt_sub_traits <- dt_sub_traits[dt_ta_suboder, on = "taxon"]

dt_spe_traits[, c(2:9):=NULL]
dt_gen_traits[, c(1, 3:9):=NULL]
dt_fam_traits[, c(1,2,3,5:9):=NULL]
dt_sub_traits[, c(1:5, 7:9):=NULL]

dt_all_traits <- rbindlist(list(dt_spe_traits, dt_gen_traits, dt_fam_traits, dt_sub_traits), fill = T)

# remove traits that only have NAs
rm_id <- which(apply(dt_all_traits, 2, function(x) all(is.na(x))))
dt_all_traits[, (rm_id) := NULL]

# check that this worked 
rm_id <- which(apply(dt_all_traits, 2, function(x) all(is.na(x))))

# now taxa that have no trait data at all 
rm_id <- which(apply(dt_all_traits, 1, function(x) all(is.na(x[-c(1,112,113)]))))

dt_all_traits[1,]
