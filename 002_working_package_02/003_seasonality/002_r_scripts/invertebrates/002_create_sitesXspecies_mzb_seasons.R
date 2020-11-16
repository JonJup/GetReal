# ----------------------------------------- #
### --- Create species X sites tables --- ### 
### --- Macroinvertebrates -------------- ### 
### --- Seasonal changes   -------------- ### 
# ----------------------------------------- #

# date written/modified: 24.08.20
# date used: 24.08.20, 25.08. + 28 + 10.09
# Jonathan Jupke 
# Get Real WP 2
# Seasonal changes? 

# 01. Setup  --------------------------------------------------------------
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(data.table,
       dplyr,
       fuzzySim,
       here,
       magrittr,
       stringr)

dir_mzb = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/")
dir_mzb2= here("002_working_package_02/003_seasonality/003_results/invertebrates/")
dir_re  = here("002_working_package_02/003_seasonality/003_results/invertebrates/003_auxiliary/")

# read in and prepare data ------------------------------------------------
mzb  <- readRDS(file.path(dir_mzb, "004_2020-11-03_mzb_data1585_low_impact.RDS"))

# carpeting  -------------------------------------------------------------
mzb[, final_taxon := str_trim(final_taxon, side = "both")]
mzb[final_taxon == "Stratiomyiidae", final_taxon := "Stratiomyidae"]
mzb[final_taxon == "Notonectidae", final_taxon_level := "family"]

# 02. Subset  --------------------------------------------------------------
mzb$gr_sample_id %<>% as.character()

sub_1 <- mzb[(is.na(year) | year >= 2000) & !is.na(season)]
sub_1[,rt := ls_bd_20]
sub_1[, ls_bd_20 := NULL]
sub_1[rt %in% c("RT8", "RT9", "RT10", "RT11", "RT15", "RT16"),   rt := "RT8_11_15_16"]
sub_1[rt %in% c("RT4", "RT5"), rt := "RT4_5"]
sub_1[rt %in% c("RT2", "RT3"), rt := "RT2_3"]
sub_1[season == "Summer", season := "summer"]
unique(sub_1$season)

for (i in c("1", "2_3", "4_5", "8_11_15_16", "17", "18", "19")) {
        assign(x = paste0("rt", i), 
               value = sub_1[rt == paste0("RT",i)])
}

rt_17_sites    <- readRDS(file.path(dir_re, "rt17_sites.RDS"))

# 03. Drop columns --------------------------------------------------------

# subset sites 
rt1.1 = rt1[data.set %in% c("mzb_Ecosurv")]
rt1.2 = rt1[data.set %in% c("mzb_Naiades", "MZB_LD") & !gr_sample_id %in% c("site_18460_date_05121_mzb_Landau", "site_18459_date_05121_mzb_Landau") & season != "spring"]
rt2_3 = rt2_3[data.set %in% c("MZB_LD") & season != "autumn"]
rt4_5 = rt4_5[data.set %in% c("MZB_LD", "mzb_WISER") & season != "winter"]
rt8_11_15_16 =  rt8_11_15_16[!data.set %in% c("leonard_sandin", "Picos_Pepe") & season != "winter"]
rt17 = rt17[gr_sample_id %in% rt_17_sites]
rt18 = rt18[!season %in% c("spring", "winter")]
rt19 = rt19[!season %in% c("spring", "winter")]

files_vec <- c("rt1.1","rt1.2","rt2_3","rt4_5","rt8_11_15_16","rt17","rt18","rt19")

# drop columns 
for (i in files_vec) {
        ld <- get(i)
        ld <- ld[,.(gr_sample_id, final_taxon, final_taxon_level, season)] 
        assign(x = i, value = ld)
}

rm(sub_1, mzb, i , ld, rt_17_sites,rt1);gc()

## -- different levels
for (i in c("spe", "gen", "foh")) {
        taxon_var <- ifelse(i == "spe", "species", ifelse(i == "gen", "genus", c("family", "order", "subclass")))
        for (k in files_vec){
                assign(x     = paste0(k, "_", i), 
                       value = get(k)[final_taxon_level %in% taxon_var] 
                )
        }

}
rm (list = files_vec)
rm (i, k, taxon_var, files_vec);gc()
files <- setdiff(ls(),c("dir_mzb", "dir_re", "dir_mzb2"))

# 04. Turn to site X species matrix --------------------------------------------------------

for (i in files) {
        ld <- get(i)
        ld[, final_taxon_level := NULL]
        ldj <- copy(ld)
        ldj[, final_taxon  := NULL]
        ldj <- unique(ldj, by = "gr_sample_id")
        assign( x = i,
                value = splist2presabs(data = ld, sites.col = 1, sp.col = 2)
                )
         ld2 <- get(i)
         setDT(ld2)
         
         ld3 <- ldj[ld2, on = "gr_sample_id"]

        assign(x = i,
               value = ld3)
        if (any(duplicated(ld3$gr_sample_id))) break()
        
        rm(ld,ld2,ld3,i);gc()

}

# 05. remove rare species/ sites --------------------------------------------------------
# i.e. < 5 taxa per site or < 5 occurrences in data set 

# -- rare taxa -- #

ls_rare = readRDS(file.path(dir_mzb, "0x_rare_taxa_list.RDS"))

for (i in 1:length(files)) {
        dt_loop = get(files[i])
        if (str_detect(string = files[i], pattern = "spe")) {
                in_loop_id = which(names(dt_loop) %in% ls_rare[[1]])
                dt_loop = dt_loop[, -in_loop_id, with = FALSE]
        } else if (str_detect(string = files[i], pattern = "gen")) {
                in_loop_id = which(names(dt_loop) %in% ls_rare[[2]])
                dt_loop = dt_loop[, -in_loop_id, with = FALSE]
        } else if (str_detect(string = files[i], pattern = "foh")) {
                in_loop_id = which(names(dt_loop) %in% ls_rare[[3]])
                dt_loop = dt_loop[, -in_loop_id, with = FALSE]
        }
        assign(x = files[i],
               value = dt_loop)
        rm(i)
        gc()
}
        
# Remove all entries with only two columns i.e. no taxa 
for (i in files) {
        ld <- get(i)
        if (ncol(ld) < 3){
                rm(list = i)
                print(paste("removed", i))
                id <- which(files == i)
                files <- files[-id]
        } 
        
}

# save data to file -------------------------------------------------------
for (i in seq_along(files)) {
        save.name = paste0(files[i], ".RDS")
        save.file = get(files[i])
        saveRDS(object = save.file,
                file = file.path(
                        dir_mzb2,
                        paste0(
                                "001_speciesXsites_tables/",
                                Sys.Date(),
                                "_",
                                save.name
                        )
                ))
} 
## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 
