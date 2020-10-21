# -------------------------------------------------- #
### --- Typical assemblages Macroinvertebrates --- ###
### --- Seasonality ------------------------------ ### 
# -------------------------------------------------- #
 
# date written\modified : 24.08.20+ 31
# date used: 24.08.20, 25, 31
# Jonathan Jupke
# GetReal WP2
# Seasonality Invertebrates 

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, magrittr, dplyr, indicspecies, stringr, here)
setwd(here("003_results/"))

# load data  --------------------------------------------------------------
# load  speciesXsites with ls broad types 
files <- dir(path = "001_speciesXsites_tables/")

for (i in seq_along(files)) {
        lv <- files[i]
        obj_name <- lv %>% 
                str_extract("rt[0-9]*_[a-z]*")
        assign(x     = obj_name,
               value = readRDS(paste0("001_speciesXsites_tables/",lv)))
        
}
rm(i, lv, obj_name, files);gc()
# clean data --------------------------------------------------------------

files <- ls()

# compute indval ----------------------------------------------------------

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

rm(loop_data, seasons_var,
   rt01_gen, rt01_foh, 
   rt10_spe, rt10_gen, rt10_foh,
   rt15_gen, rt15_foh,
   rt18_gen, rt18_foh,
   k, l, files);gc()

files <- ls()

#f_s_01   <- files[grep(x = files, pattern = "rt01_spe")]
f_s_10   <- files[grep(x = files, pattern = "rt10_spe")]
#f_s_15   <- files[grep(x = files, pattern = "rt15_spe")]
#f_s_18   <- files[grep(x = files, pattern = "rt18_spe")]
f_g_01   <- files[grep(x = files, pattern = "rt01_gen")]
f_g_10   <- files[grep(x = files, pattern = "rt10_gen")]
f_g_15   <- files[grep(x = files, pattern = "rt15_gen")]
f_g_18   <- files[grep(x = files, pattern = "rt18_gen")]
f_f_01   <- files[grep(x = files, pattern = "rt01_foh")]
f_f_10   <- files[grep(x = files, pattern = "rt10_foh")]
f_f_15   <- files[grep(x = files, pattern = "rt15_foh")]
f_f_18   <- files[grep(x = files, pattern = "rt18_foh")]

ll <- paste0("ll_",
             rep(c("spe", "gen", "foh")       , each = 4),
            "_",
            rep(c("01", "10", "15", "18"), times = 3))

for (i in seq_along(ll)){
        assign(x = ll[i], 
               value = list())
}

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


# save to file ------------------------------------------------------------
for (i in lists_vector) {
        
        file <- i
        ld <- get(file)
        tl <- ifelse(str_detect(file, "spe"), "s", ifelse(str_detect(file, "gen"), "g", "f"))
        rt <- str_extract(file, "[0-9].*")
        save_name <- paste0("002_", Sys.Date(), "_indicator_", tl, "_", rt, ".RDS")
        saveRDS(object = ld, 
                file   = save_name)
        
}


