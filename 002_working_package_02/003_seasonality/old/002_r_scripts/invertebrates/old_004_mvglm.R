# ------------------------------------- #
### --- MVGLM of different seasons --- ### 
### --- Macroinvertebrates ---------- ### 
# ------------------------------------- #

# date: 07.09.20
# GetReal WP 2 MZB


# setup -------------------------------------------------------------------

pacman::p_load(here, data.table, dplyr, ggplot2, magrittr, mvabund, stringr, vegan)
setwd(here())


# load data ---------------------------------------------------------------

files <- dir(path = "003_results/invertebrates/001_speciesXsites_tables/")

for (i in seq_along(files)) {
        lv <- files[i]
        obj_name <- lv %>% 
                str_extract("rt[0-9]*_[a-z]*")
        assign(x     = obj_name,
               value = readRDS(paste0("003_results/invertebrates/001_speciesXsites_tables/",lv)))
        
}
rm(i, lv, obj_name, files);gc()
file_l <- ls()
# join tables 
for (i in c("01", "10", "15", "18")) {
        
        file_pre <- paste0("rt", i, "_")
        files    <- ls()[grepl(pattern = file_pre, x = ls())]
        if (length(files) == 2){
                ld1 <- get(files[1]) 
                ld2 <- get(files[2])
                ldj <- ld2[ld1, on = "gr_sample_id"]
                if (any(duplicated(ldj$gr_sample_id))) {print(paste(i, "after 1"))}
                rm(ld1, ld2, ld3);gc()
                
        } else if (length(files) == 3){
                ld1 <- get(files[1]) 
                ld2 <- get(files[2]) 
                ld3 <- get(files[3])  
                ldj <- ld3[ld2, on = "gr_sample_id"]
                if (any(duplicated(ldj$gr_sample_id))) {print(paste(i, "after 2"))}
                #ldf[, i.season := NULL]
                ldj <- ld1[ldj, on = "gr_sample_id"]
                if (any(duplicated(ldj$gr_sample_id))) {print(paste(i, "after 3"))}
                rm(ld1, ld2, ld3);gc()
        }
        assign(x = paste0(file_pre, "all"), 
               value = ldj)
        print(i)
        rm(ldj, files, file_pre, i);gc()
        
}
rm(list = file_l)
rm(file_l)

## -- distance matrix -- ##
files <- ls()

if (nrow(rt01_all[is.na(season)]) != 0 |
    nrow(rt10_all[is.na(season)]) != 0 |
    nrow(rt15_all[is.na(season)]) != 0 |
    nrow(rt18_all[is.na(season)]) != 0) {
        print("Quality Check Failed")
} else {
        print("Quality Check Passed")
}
# remove i.season column that was created during the join
for (i in files){
        ld <- get(i)
        ld[,i.season := NULL]
        assign(x = i, 
               value = ld)
        rm(ld)
}
# rt10 special 

rt10_all[is.na(Alainites.muticus)  , Alainites.muticus := 0]
rt10_all[is.na(Peregriana.peregra) , Peregriana.peregra := 0]
rt10_all[is.na(Torleya.major)      , Torleya.major := 0]
rt10_all[is.na(Nilotanypus.dubius) , Nilotanypus.dubius := 0]
rt10_all[is.na(Oreodytes.sanmarkii), Oreodytes.sanmarkii := 0]
rt10_all[is.na(Propappus.volki)    , Propappus.volki := 0]
rt10_all[is.na(Serratella.ignita)  , Serratella.ignita := 0]
rt10_all[, i.season.1 := NULL]

# fit model  --------------------------------------------------------------

## -- model1 -- ## 
mva_data <- mvabund(rt01_all[,3:71])
model <- manyglm(mva_data ~ season,
                 data = rt01_all, 
                 family = "binomial")

plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)

rt1_model_out <- anova.manyglm(
        model,
        p.uni = "adjusted",
        test = "LR",
        resamp = "pit.trap",
        nBoot = 999, 
        show.time = "all"
)
rt1_model_out$table
rt1_model_out$uni.test[2,] %>% sort %>%  plot

saveRDS(rt1_model_out, paste0("003_results/invertebrates//002_mvglms/", Sys.Date(), "_rt1.RDS"))


## -- model2 -- ## 
mva_data <- mvabund(rt10_all[,3:198])
model <- manyglm(mva_data ~ season,
                 data = rt10_all, 
                 family = "binomial")

# plot(model, which = 1)
# plot(model, which = 2)
# plot(model, which = 3)

rt10_model_out <- anova.manyglm(
        model,
        p.uni = "adjusted",
        test = "LR",
        resamp = "pit.trap",
        nBoot = 999, 
        show.time = "all"
)
rt10_model_out$table
rt10_model_out$uni.test[2,] %>% sort %>%  plot
saveRDS(rt10_model_out, paste0("003_results/invertebrates/002_mvglms/", Sys.Date(), "_rt10.RDS"))

## -- model3 -- ## 
mva_data <- mvabund(rt15_all[,3:102])
model <- manyglm(mva_data ~ season,
                 data = rt15_all, 
                 family = "binomial")

plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)

rt15_model_out <- anova.manyglm(
        model,
        p.uni = "adjusted",
        test = "LR",
        resamp = "pit.trap",
        nBoot = 999, 
        show.time = "all"
)
rt15_model_out$table
rt15_model_out$uni.test[2,] %>% sort %>%  plot
saveRDS(rt15_model_out, paste0("003_results/invertebrates/002_mvglms/", Sys.Date(), "_rt15.RDS"))

## -- model4 -- ## 
mva_data <- mvabund(rt18_all[,3:128])

rt18_all$season %<>% as.factor()

model <- manyglm(mva_data ~ season,
                 data = rt18_all, 
                 family = "binomial")



plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)

rt18_model_out <- anova.manyglm(
        model,
        p.uni = "adjusted",
        test = "LR",
        resamp = "pit.trap",
        nBoot = 999, 
        show.time = "all"
)
rt18_model_out$table
rt18_model_out$uni.test[2,] %>% sort %>%  plot
saveRDS(rt18_model_out, paste0("003_results/invertebrates/002_mvglms/", Sys.Date(), "_rt18.RDS"))
