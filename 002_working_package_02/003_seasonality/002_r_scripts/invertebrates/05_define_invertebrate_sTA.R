# -------------------------------- #
### --- Typical assemblages  --- ###
### --- Macroinvertebrates ----- ###
### --- Seasonality ------------ ### 
# -------------------------------- #
 
# date written\modified : 11.09.20
# date used: 11.09.20, 13.11.20 + 27.11

# Jonathan Jupke
# GetReal WP2
# Seasonality 
# Macroinvertebrates   

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, 
               dplyr,  
               here, 
               indicspecies, 
               magrittr, 
               stringr)

dir_re = here("002_working_package_02/003_seasonality/003_results/invertebrates")

# load data  --------------------------------------------------------------
ls_all <- readRDS(file.path(dir_re, "sxs_list.RDS"))
rt10s = ls_all[[1]]$RT10_sub1
rt10g = ls_all[[2]]$RT10_sub1
rt10f = ls_all[[3]]$RT10_sub1

rt16s = ls_all[[1]]$RT16_sub1
rt16g = ls_all[[2]]$RT16_sub1
rt16f = ls_all[[3]]$RT16_sub1

n_spe_10 <- ncol(rt10s) - 2
n_gen_10 <- ncol(rt10g) - 2
n_fol_10 <- ncol(rt10f) - 2
n_spe_16 <- ncol(rt16s) - 2
n_gen_16 <- ncol(rt16g) - 2
n_fol_16 <- ncol(rt16f) - 2

rt10_all <- rt10s[rt10g, on = "gr_sample_id"]
rt10_all <- rt10_all[rt10f, on = "gr_sample_id"]
rt16all <- rt16s[rt16g, on = "gr_sample_id"]
rt16all <- rt16all[rt16f, on = "gr_sample_id"]

rt10_all[is.na(season) & !is.na(i.season), season := i.season]
rt10_all[is.na(season) & !is.na(i.season.1), season := i.season.1]
rt10_all[, c("i.season", "i.season.1") := NULL]
rt16all[is.na(season) & !is.na(i.season), season := i.season]
rt16all[is.na(season) & !is.na(i.season.1), season := i.season.1]
rt16all[, c("i.season", "i.season.1") := NULL]

# Remove NAs 
for (j in seq_len(ncol(rt10_all))) set(rt10_all, which(is.na(rt10_all[[j]])), j, 0)
for (j in seq_len(ncol(rt16all))) set(rt16all, which(is.na(rt16all[[j]])), j, 0)

# test that taxa vector will work 
ncol(rt10_all) == 2 + n_spe_10 + n_gen_10 + n_fol_10
ncol(rt16all) == 2 + n_spe_16 + n_gen_16 + n_fol_16

rm(rt10f, rt10g, rt10s, rt16f, rt16g, rt16s, j);gc()

rt10_all[season == "spring", .N]
rt10_all[season == "summer", .N]
rt10_all[season == "autumn", .N]
rt10_all[season == "winter", .N]
rt16all[season == "spring", .N]
rt16all[season == "summer", .N]
rt16all[season == "autumn", .N]
rt16all[season == "winter", .N]

rowSums(rt10_all[,-c(1:2)]) %>% hist()
rowSums(rt10_all[,-c(1:2)]) %>% summary()
rowSums(rt10_all[,-c(1:2)]) %>% sd()
rowSums(rt16all[,-c(1:2)]) %>% hist()
rowSums(rt16all[,-c(1:2)]) %>% summary()
rowSums(rt16all[,-c(1:2)]) %>% sd()

rt10_all$season %<>% droplevels()
rt16all$season %<>% droplevels()

# compute indval ----------------------------------------------------------

files <- c("rt10_all", "rt16all")

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
   rt10_all, rt16all);gc()

f_10   <- ls()[grep(x = ls(), pattern = "rt10")]
f_16   <- ls()[grep(x = ls(), pattern = "rt16")]

ll <- paste0("ll_", c("10", "16"))

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

rt10s <- ll_10[1:n_spe_10,]
rt10g <- ll_10[(n_spe_10+1):(n_spe_10+n_gen_10),]
rt10f <- ll_10[(n_spe_10+n_gen_10+1):nrow(ll_10)]
rt16s <- ll_16[1:n_spe_16,]
rt16g <- ll_16[(n_spe_16+1):(n_spe_16+n_gen_16),]
rt16f <- ll_16[(n_spe_16+n_gen_16+1):nrow(ll_16)]

ls_final = list(
  "spe10" = rt10s,
  "gen10" = rt10g,
  "fam10" = rt10f,
  "spe16" = rt16s,
  "gen16" = rt16g,
  "fam16" = rt16f
)

saveRDS(file.path(dir_re, "sta_list.RDS"), object = ls_final)

if (readline("Delete all? ") == "yes") rm(list = ls())
