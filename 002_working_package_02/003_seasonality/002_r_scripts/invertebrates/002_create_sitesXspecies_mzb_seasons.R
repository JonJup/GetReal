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

pacman::p_load(data.table,
               fuzzySim,
               dplyr,
               magrittr,
               stringr,
               here
               )

setwd(here())

# read in and prepare data ------------------------------------------------
mzb  <- readRDS("../001_Community Data/100_Combined/002_invertebrates/003_processed_data/004_2020-07-03_mzb_data1585_low_impact.RDS")

# carpeting  -------------------------------------------------------------
mzb[, final_taxon := str_trim(final_taxon, side = "both")]
mzb[final_taxon == "Stratiomyiidae", final_taxon := "Stratiomyidae"]
mzb[final_taxon == "Notonectidae", final_taxon_level := "family"]

# 02. Subset  --------------------------------------------------------------
mzb$gr_sample_id %<>% as.character()

sub_1 <- mzb[(is.na(year) | year >= 2000) & !is.na(season)]
sub_1[,rt := ls_bd_20]
sub_1[, ls_bd_20 := NULL]
sub_1[rt %in% paste0("RT", 2:3),   rt := "RT2_3"]
sub_1[rt %in% paste0("RT", 4:5),   rt := "RT4_5"]
sub_1[rt %in% paste0("RT", 8:9),   rt := "RT8_9"]
sub_1[rt %in% paste0("RT", 10:11), rt := "RT10_11"]
sub_1[rt %in% paste0("RT", 15:16), rt := "RT15_16"]
sub_1[season == "Summer", season := "summer"]
unique(sub_1$season)

for (i in c("1", "2_3", "4_5", "8_9", "10_11", "15_16", "17", "18", "19")) {
        assign(x = paste0("rt", i), 
               value = sub_1[rt == paste0("RT",i)])
}

rt_15_16_sites <- readRDS("003_results/invertebrates/003_auxiliary/rt_15_16_sites.RDS")
rt_17_sites    <- readRDS("003_results/invertebrates/003_auxiliary/rt17_sites.RDS")

# 03. Drop columns --------------------------------------------------------

# subset sites 
rt1.1   <- rt1[data.set %in% c("mzb_Ecosurv")]
rt1.2   <- rt1[data.set %in% c("mzb_Naiades", "MZB_LD") & !gr_sample_id %in% c("site_18460_date_05121_mzb_Landau", "site_18459_date_05121_mzb_Landau") & season != "spring"]
rt2_3   <- rt2_3[data.set %in% c("MZB_LD") & season != "autumn"]
rt4_5   <- rt4_5[data.set %in% c("MZB_LD", "mzb_WISER") & season != "winter"]
rt8_9   <- rt8_9[!data.set %in% c("leonard_sandin", "Picos_Pepe") & season != "winter"]
rt15_16 <- rt15_16[gr_sample_id %in% rt_15_16_sites]
rt17    <- rt17[gr_sample_id %in% rt_17_sites]
rt18    <- rt18[!season %in% c("spring", "winter")]
rt19    <- rt19[!season %in% c("spring", "winter")]


files_vec <- c("rt1.1","rt1.2","rt2_3","rt4_5","rt8_9", "rt10_11", "rt15_16","rt17","rt18","rt19")

# drop columns 
for (i in files_vec) {
        ld <- get(i)
        ld <- ld[,.(gr_sample_id, final_taxon, final_taxon_level, season)] 
        assign(x = i, value = ld)
}


rm(sub_1, mzb, i , ld, rt_15_16_sites, rt_17_sites,rt1);gc()

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
files <- ls()

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

for (i in files) {
        
        ld <- get(i)
        rare_taxon_id <- which(colSums(x = ld[,-c(1,2)]) < 3) + 1
        rare_names    <- names(rare_taxon_id)
        if (length(rare_names) != 0) {
                assign(x     = i,
                       value = ld[, -rare_names, with = FALSE])
                rm(rare_taxon_id, rare_names, ld);gc()
        } else {
                rm(rare_names, rare_taxon_id, ld);gc()
        }
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
setwd("003_results/invertebrates/")

for (i in seq_along(files)) {
        save.name = paste0(files[i],".RDS")
        save.file = get(files[i])
        saveRDS(
                object = save.file,
                file = paste0("001_speciesXsites_tables/",
                              Sys.Date(),
                              "_",
                              save.name)
        )
} 
## -- ## 
if (readline("remove all? ") == "yes") rm(list = ls()) 
