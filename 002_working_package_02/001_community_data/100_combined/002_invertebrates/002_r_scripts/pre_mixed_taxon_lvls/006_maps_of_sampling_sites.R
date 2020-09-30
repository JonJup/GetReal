#### ---------------------- ####
### --- Map diatoms data --- ###
#### ---------------------- ####


# setup -------------------------------------------------------------------
pacman::p_load(sf, dplyr, data.table, magrittr)
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/003_processed_data/")

# read in data ------------------------------------------------------------
spe_all <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.s.all.RDS")
gen_all <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.g.all.RDS")
fam_all <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.f.all.RDS")
spe_spr <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.s.spr.RDS")
gen_spr <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.g.spr.RDS")
fam_spr <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.f.spr.RDS")
spe_sum <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.s.sum.RDS")
gen_sum <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.g.sum.RDS")
fam_sum <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.f.sum.RDS")
spe_aut <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.s.aut.RDS")
gen_aut <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.g.aut.RDS")
fam_aut <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.f.aut.RDS")
spe_win <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.s.win.RDS")
gen_win <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.g.win.RDS")
fam_win <- readRDS("001_speciesXsites_tables/2020-03-19sites.bio2.f.win.RDS")

mzb <- readRDS("004_2020-03-18_mzb_sites_low.RDS")


# prepare data ------------------------------------------------------------

spe_all_map <- filter(mzb, gr_sample_id %in% spe_all) %>% st_as_sf()
gen_all_map <- filter(mzb, gr_sample_id %in% gen_all) %>% st_as_sf()
fam_all_map <- filter(mzb, gr_sample_id %in% fam_all) %>% st_as_sf()
spe_spr_map <- filter(mzb, gr_sample_id %in% spe_spr) %>% st_as_sf()
gen_spr_map <- filter(mzb, gr_sample_id %in% gen_spr) %>% st_as_sf()
fam_spr_map <- filter(mzb, gr_sample_id %in% fam_spr) %>% st_as_sf()
spe_sum_map <- filter(mzb, gr_sample_id %in% spe_sum) %>% st_as_sf()
gen_sum_map <- filter(mzb, gr_sample_id %in% gen_sum) %>% st_as_sf()
fam_sum_map <- filter(mzb, gr_sample_id %in% fam_sum) %>% st_as_sf()
spe_aut_map <- filter(mzb, gr_sample_id %in% spe_aut) %>% st_as_sf()
gen_aut_map <- filter(mzb, gr_sample_id %in% gen_aut) %>% st_as_sf()
fam_aut_map <- filter(mzb, gr_sample_id %in% fam_aut) %>% st_as_sf()
spe_win_map <- filter(mzb, gr_sample_id %in% spe_win) %>% st_as_sf()
gen_win_map <- filter(mzb, gr_sample_id %in% gen_win) %>% st_as_sf()
fam_win_map <- filter(mzb, gr_sample_id %in% fam_win) %>% st_as_sf()

# save_to_file ------------------------------------------------------------

st_write(obj = species, dsn = "003_processed_data/map_species.gpkg")
st_write(obj = genus, dsn = "003_processed_data/map_genus.gpkg")
st_write(obj = family, dsn = "003_processed_data/map_family.gpkg")

st_write(obj = spe_all_map, dsn = "002_sampling_site_maps/spe_all_map.gpkg")
st_write(obj = gen_all_map, dsn = "002_sampling_site_maps/gen_all_map.gpkg")
st_write(obj = fam_all_map, dsn = "002_sampling_site_maps/fam_all_map.gpkg")
st_write(obj = spe_spr_map, dsn = "002_sampling_site_maps/spe_spr_map.gpkg")
st_write(obj = gen_spr_map, dsn = "002_sampling_site_maps/gen_spr_map.gpkg")
st_write(obj = fam_spr_map, dsn = "002_sampling_site_maps/fam_spr_map.gpkg")
st_write(obj = spe_sum_map, dsn = "002_sampling_site_maps/spe_sum_map.gpkg")
st_write(obj = gen_sum_map, dsn = "002_sampling_site_maps/gen_sum_map.gpkg")
st_write(obj = fam_sum_map, dsn = "002_sampling_site_maps/fam_sum_map.gpkg")
st_write(obj = spe_aut_map, dsn = "002_sampling_site_maps/spe_aut_map.gpkg")
st_write(obj = gen_aut_map, dsn = "002_sampling_site_maps/gen_aut_map.gpkg")
st_write(obj = fam_aut_map, dsn = "002_sampling_site_maps/fam_aut_map.gpkg")
st_write(obj = spe_win_map, dsn = "002_sampling_site_maps/spe_win_map.gpkg")
st_write(obj = gen_win_map, dsn = "002_sampling_site_maps/gen_win_map.gpkg")
st_write(obj = fam_win_map, dsn = "002_sampling_site_maps/fam_win_map.gpkg")
