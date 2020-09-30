# -------------------------------- #
### --- Typical assemblages  --- ###
### --- Macroinvertebrates ----- ###
### --- Seasonality ------------ ### 
# -------------------------------- #
 
# date written\modified : 11.09.20
# date used: 11.09.20
# Jonathan Jupke
# GetReal WP2
# Seasonality Macroinvertebrates   

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, 
               dplyr,  
               here, 
               indicspecies, 
               magrittr, 
               stringr)
setwd(here("003_results/invertebrates/001_speciesXsites_tables"))

# load data  --------------------------------------------------------------
rt10_11g <- readRDS("2020-09-10_rt10_11_gen.RDS")
rt10_11s <- readRDS("2020-09-10_rt10_11_spe.RDS")
rt10_11f <- readRDS("2020-09-10_rt10_11_foh.RDS")

rt15_16g <- readRDS("2020-09-10_rt15_16_gen.RDS") 
rt15_16s <- readRDS("2020-09-10_rt15_16_spe.RDS")
rt15_16f <- readRDS("2020-09-10_rt15_16_foh.RDS")

n_spe_10_11 <- ncol(rt10_11s) - 2
n_gen_10_11 <- ncol(rt10_11g) - 2
n_fol_10_11 <- ncol(rt10_11f) - 2
n_spe_15_16 <- ncol(rt15_16s) - 2
n_gen_15_16 <- ncol(rt15_16g) - 2
n_fol_15_16 <- ncol(rt15_16f) - 2

rt10_11_all <- rt10_11s[rt10_11g, on = "gr_sample_id"]
rt10_11_all <- rt10_11_all[rt10_11f, on = "gr_sample_id"]
rt15_16_all <- rt15_16s[rt15_16g, on = "gr_sample_id"]
rt15_16_all <- rt15_16_all[rt15_16f, on = "gr_sample_id"]

rt10_11_all[is.na(season) & !is.na(i.season), season := i.season]
rt10_11_all[is.na(season) & !is.na(i.season.1), season := i.season.1]
rt10_11_all[, c("i.season", "i.season.1") := NULL]
rt15_16_all[is.na(season) & !is.na(i.season), season := i.season]
rt15_16_all[is.na(season) & !is.na(i.season.1), season := i.season.1]
rt15_16_all[, c("i.season", "i.season.1") := NULL]

# Remove NAs 
for (j in seq_len(ncol(rt10_11_all))) set(rt10_11_all, which(is.na(rt10_11_all[[j]])), j, 0)
for (j in seq_len(ncol(rt15_16_all))) set(rt15_16_all, which(is.na(rt15_16_all[[j]])), j, 0)

# test that taxa vector will work 
ncol(rt10_11_all) == 2 + n_spe_10_11 + n_gen_10_11 + n_fol_10_11
ncol(rt15_16_all) == 2 + n_spe_15_16 + n_gen_15_16 + n_fol_15_16

rm(rt10_11f, rt10_11g, rt10_11s, rt15_16f, rt15_16g, rt15_16s, j);gc()

rt10_11_all[season == "spring", .N]
rt10_11_all[season == "summer", .N]
rt10_11_all[season == "autumn", .N]
rt10_11_all[season == "winter", .N]
rt15_16_all[season == "spring", .N]
rt15_16_all[season == "summer", .N]
rt15_16_all[season == "autumn", .N]
rt15_16_all[season == "winter", .N]

rowSums(rt10_11_all[,-c(1:2)]) %>% hist()
rowSums(rt10_11_all[,-c(1:2)]) %>% summary()
rowSums(rt10_11_all[,-c(1:2)]) %>% sd()
rowSums(rt15_16_all[,-c(1:2)]) %>% hist()
rowSums(rt15_16_all[,-c(1:2)]) %>% summary()
rowSums(rt15_16_all[,-c(1:2)]) %>% sd()

# compute indval ----------------------------------------------------------

files <- c("rt10_11_all", "rt15_16_all")

# increased number of permutations to get smaller p-values 

for (k in 1:length(files)) {
        print(paste("K", k, "start @", Sys.time()))
        loop_data   <- get(files[k])
        loop_data   <- loop_data[!is.na(season)]
        seasons_var <- unique(loop_data$season) 
        for (l in seasons_var) {
                 
                assign(
                        x     = paste0(files[k], "_ta_",l),
                        value = indicators(
                                X = loop_data[,-c(1:2)],
                                cluster = loop_data$season,
                                group = l,
                                max.order = 1,
                                verbose = FALSE,
                                At = 0,
                                Bt = 0,
                                func = "IndVal.g",
                                control = how(nperm = 999)
                        )
                )
                print(paste("l", l, "end @", Sys.time()))
        }
        
        
        print(paste("K", k, "end @", Sys.time()))
}; beepr::beep()

rm(loop_data, seasons_var, files, 
   rt10_11_all, rt15_16_all);gc()

f_10_11   <- ls()[grep(x = ls(), pattern = "rt10_11")]
f_15_16   <- ls()[grep(x = ls(), pattern = "rt15_16")]

ll <- paste0("ll_", c("10_11", "15_16"))

for (i in seq_along(ll)) assign(x = ll[i], value = list())

lists_vector <- ls()[grep(pattern = "^ll_", x = ls())]
files_vector <- ls()[grep(pattern = "^f_", x = ls())]
# rm(ll);gc()

for (i in seq_along(lists_vector)) {
        if (i == 1) ool <- list()
        ool[[i]] <- get(lists_vector[i])
}

for (k in seq_along(files_vector)) {
        
        llf <- files_vector[k] 

        for (i in seq_along(get(llf))) {
                
                fl  <- llf %>% get() %>% .[i] %>% get
                fl2 <- data.table( taxon   = row.names(fl[[7]]), 
                                           A         = fl[[9]],
                                           B         = fl[[10]], 
                                           sqrtIV    = fl[[11]],
                                           p_value   = fl[[12]])
                
                
                
                fl2[, season := llf %>% get() %>% .[i] %>% str_extract("[:alpha:]+$")]
                
                ool[[k]][[i]] <- fl2
        }
}

for (i in seq_along(files_vector)) {
      assign(lists_vector[i],
             rbindlist(ool[[i]])
             )
         
}

rt1011s <- ll_10_11[1:n_spe_10_11,]
rt1011g <- ll_10_11[(n_spe_10_11+1):(n_spe_10_11+n_gen_10_11),]
rt1011f <- ll_10_11[(n_spe_10_11+n_gen_10_11+1):nrow(ll_10_11)]
rt1516s <- ll_15_16[1:n_spe_15_16,]
rt1516g <- ll_15_16[(n_spe_15_16+1):(n_spe_15_16+n_gen_15_16),]
rt1516f <- ll_15_16[(n_spe_15_16+n_gen_15_16+1):nrow(ll_15_16)]

files_to_save <- c("rt1011s",
                   "rt1011g",
                   "rt1011f",
                   "rt1516s",
                   "rt1516g",
                   "rt1516f")

# save to file ------------------------------------------------------------
for (i in files_to_save) {
        
        ld <- get(i)
        # tl <- ifelse(str_detect(i, "spe"), "s", ifelse(str_detect(i, "gen"), "g", "f"))
        # rt <- str_extract(file, "[0-9].*")
        save_name <- paste0("../002_", Sys.Date(), "_indicator_", i, ".RDS")
        saveRDS(object = ld, 
                file   = save_name)
        rm(ld, save_name, i);gc()
}

if (readline("Delete all? ") == "yes") rm(list = ls())
