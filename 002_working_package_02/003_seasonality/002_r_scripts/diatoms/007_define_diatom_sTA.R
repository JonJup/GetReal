# --------------------------------------- #
### --- Typical assemblages Diatoms --- ###
### --- Seasonality ------------------- ### 
# --------------------------------------- #
 
# date written\modified : 09.09.20
# date used: 09.09.20
# Jonathan Jupke
# GetReal WP2
# Seasonality Diatoms  

# Setup -------------------------------------------------------------------
pacman::p_load(data.table, magrittr, dplyr, indicspecies, stringr, here)
setwd(here("003_results/diatoms/001_speciesXsites_tables"))

# load data  --------------------------------------------------------------
rt11g <- readRDS("2020-09-15_rt11_gen.RDS") 
rt11s <- readRDS("2020-09-15_rt11_spe.RDS")
rt15g <- readRDS("2020-09-15_rt15_gen.RDS")
rt15s <- readRDS("2020-09-15_rt15_spe.RDS")

# all(rt11g$gr_sample_id %in% rt11s$gr_sample_id)
# all(rt15g$gr_sample_id %in% rt15s$gr_sample_id)

# join data ---------------------------------------------------------------
names_rt11_g<- names(rt11g)[-c(1:2)] 
names_rt11_s<- names(rt11s)[-c(1:2)] 
names_rt15_g<- names(rt15g)[-c(1:2)] 
names_rt15_s<- names(rt15s)[-c(1:2)] 

rt11all <- rt11s[rt11g, on = "gr_sample_id"]
rt15all <- rt15s[rt15g, on = "gr_sample_id"]

rt11all[is.na(season) & !is.na(i.season), season := i.season]
rt11all[, c("i.season") := NULL]
rt15all[is.na(season) & !is.na(i.season), season := i.season]
rt15all[, c("i.season") := NULL]

# Remove NAs 
for (j in seq_len(ncol(rt11all))) set(rt11all, which(is.na(rt11all[[j]])), j, 0)
for (j in seq_len(ncol(rt15all))) set(rt15all, which(is.na(rt15all[[j]])), j, 0)

rm(rt15g, rt15s, rt11g, rt11s, j);gc()

rowSums(rt11all[,-c(1:2)]) %>% hist()
rowSums(rt11all[,-c(1:2)]) %>% summary()
rowSums(rt11all[,-c(1:2)]) %>% sd()
rowSums(rt15all[,-c(1:2)]) %>% summary()
rowSums(rt15all[,-c(1:2)]) %>% hist()
rowSums(rt15all[,-c(1:2)]) %>% sd()


rt11all[season == "summer", .N]
rt11all[season == "autumn", .N]
rt11all[season == "winter", .N]
rt15all[season == "summer", .N]
rt15all[season == "autumn", .N]
rt15all[season == "winter", .N]

# clean data --------------------------------------------------------------
files <- ls()[grepl(pattern = "^rt", x = ls())]

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
   rt11all, 
   rt15all, 
   k, l, files);gc()

files <- ls()[grepl(pattern = "^rt", x = ls())]

f_11   <- files[grep(x = files, pattern = "rt11")]
f_15   <- files[grep(x = files, pattern = "rt15")]

ll <- paste0("ll_",
             rep(c("all"), each = 2),
            "_",
            rep(c("11", "15"), times = 1))

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

# split taxa level 
spe11 <- ll_all_11[taxon %in% names_rt11_s]
gen11 <- ll_all_11[taxon %in% names_rt11_g]
spe15 <- ll_all_15[taxon %in% names_rt15_s]
gen15 <- ll_all_15[taxon %in% names_rt15_g]

files <- paste0(rep(c("spe", "gen"), times = 3),rep(c("11", "15"), each = 2))

# save to file ------------------------------------------------------------

setwd("../")

for (i in files) {
        
        file <- i
        ld <- get(file)
        tl <- ifelse(str_detect(file, "s"), "s", "g")
        rt <- str_extract(file, "[0-9].*")
        save_name <- paste0("002_", Sys.Date(), "_indicator_", tl, "_", rt, ".RDS")
        saveRDS(object = ld, 
                file   = save_name)
        
}
getwd()
if (readline("delete all? ") == "yes") rm(list = ls())
