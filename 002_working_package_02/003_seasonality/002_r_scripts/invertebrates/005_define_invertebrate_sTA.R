# -------------------------------- #
### --- Typical assemblages  --- ###
### --- Macroinvertebrates ----- ###
### --- Seasonality ------------ ### 
# -------------------------------- #
 
# date written\modified : 11.09.20
# date used: 11.09.20, 13.11.20
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

dir_sxs = here("002_working_package_02/003_seasonality/003_results/invertebrates/001_speciesXsites_tables")
dir_sav = here("002_working_package_02/003_seasonality/003_results/invertebrates/")
# load data  --------------------------------------------------------------
rt2_3g <- readRDS(file.path(dir_sxs, "2020-11-09_rt1.2_gen.RDS"))
rt2_3s <- readRDS(file.path(dir_sxs, "2020-11-09_rt1.2_spe.RDS"))
rt2_3f <- readRDS(file.path(dir_sxs, "2020-11-09_rt1.2_foh.RDS"))

rt_large_g <- readRDS(file.path(dir_sxs, "2020-11-09_rt8_11_15_16_gen.RDS")) 
rt_large_s <- readRDS(file.path(dir_sxs, "2020-11-09_rt8_11_15_16_spe.RDS"))
rt_large_f <- readRDS(file.path(dir_sxs, "2020-11-09_rt8_11_15_16_foh.RDS"))

n_spe_2_3 <- ncol(rt2_3s) - 2
n_gen_2_3 <- ncol(rt2_3g) - 2
n_fol_2_3 <- ncol(rt2_3f) - 2
n_spe_large <- ncol(rt_large_s) - 2
n_gen_large <- ncol(rt_large_g) - 2
n_fol_large <- ncol(rt_large_f) - 2

rt2_3_all <- rt2_3s[rt2_3g, on = "gr_sample_id"]
rt2_3_all <- rt2_3_all[rt2_3f, on = "gr_sample_id"]
rt_large_all <- rt_large_s[rt_large_g, on = "gr_sample_id"]
rt_large_all <- rt_large_all[rt_large_f, on = "gr_sample_id"]

rt2_3_all[is.na(season) & !is.na(i.season), season := i.season]
rt2_3_all[is.na(season) & !is.na(i.season.1), season := i.season.1]
rt2_3_all[, c("i.season", "i.season.1") := NULL]
rt_large_all[is.na(season) & !is.na(i.season), season := i.season]
rt_large_all[is.na(season) & !is.na(i.season.1), season := i.season.1]
rt_large_all[, c("i.season", "i.season.1") := NULL]

# Remove NAs 
for (j in seq_len(ncol(rt2_3_all))) set(rt2_3_all, which(is.na(rt2_3_all[[j]])), j, 0)
for (j in seq_len(ncol(rt_large_all))) set(rt_large_all, which(is.na(rt_large_all[[j]])), j, 0)

# test that taxa vector will work 
ncol(rt2_3_all) == 2 + n_spe_2_3 + n_gen_2_3 + n_fol_2_3
ncol(rt_large_all) == 2 + n_spe_large + n_gen_large + n_fol_large

rm(rt2_3f, rt2_3g, rt2_3s, rt_large_f, rt_large_g, rt_large_s, j);gc()

rt2_3_all[season == "spring", .N]
rt2_3_all[season == "summer", .N]
rt2_3_all[season == "autumn", .N]
rt2_3_all[season == "winter", .N]
rt_large_all[season == "spring", .N]
rt_large_all[season == "summer", .N]
rt_large_all[season == "autumn", .N]
rt_large_all[season == "winter", .N]

rowSums(rt2_3_all[,-c(1:2)]) %>% hist()
rowSums(rt2_3_all[,-c(1:2)]) %>% summary()
rowSums(rt2_3_all[,-c(1:2)]) %>% sd()
rowSums(rt_large_all[,-c(1:2)]) %>% hist()
rowSums(rt_large_all[,-c(1:2)]) %>% summary()
rowSums(rt_large_all[,-c(1:2)]) %>% sd()

# compute indval ----------------------------------------------------------

files <- c("rt2_3_all", "rt_large_all")

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
                                control = how(nperm = 1)
                        )
                )
                print(paste("l", l, "end @", Sys.time()))
        }
        
        
        print(paste("K", k, "end @", Sys.time()))
}; beepr::beep()

rm(loop_data, seasons_var, files, 
   rt2_3_all, rt_large_all);gc()

f_2_3   <- ls()[grep(x = ls(), pattern = "rt2_3")]
f_large   <- ls()[grep(x = ls(), pattern = "rt_large")]

ll <- paste0("ll_", c("2_3", "large"))

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

rt23s <- ll_2_3[1:n_spe_2_3,]
rt23g <- ll_2_3[(n_spe_2_3+1):(n_spe_2_3+n_gen_2_3),]
rt23f <- ll_2_3[(n_spe_2_3+n_gen_2_3+1):nrow(ll_2_3)]
rtlarges <- ll_large[1:n_spe_large,]
rtlargeg <- ll_large[(n_spe_large+1):(n_spe_large+n_gen_large),]
rtlargef <- ll_large[(n_spe_large+n_gen_large+1):nrow(ll_large)]

files_to_save <- c("rt23s",
                   "rt23g",
                   "rt23f",
                   "rtlarges",
                   "rtlargeg",
                   "rtlargef")

# save to file ------------------------------------------------------------
for (i in files_to_save) {
        
        ld <- get(i)
        save_name <- file.path(dir_sav, paste0("002_indicator_", i, ".RDS"))
        saveRDS(object = ld, 
                file   = save_name)
        rm(ld, save_name, i);gc()
}

if (readline("Delete all? ") == "yes") rm(list = ls())
