# ------------------------------------- #
### --- mvGLM of different seasons --- ### 
### --- Diatoms ---------- ---------- ### 
# ------------------------------------- #

# date: 07.09.20 + 08. 
# GetReal WP 2 
# Diatoms 

# Look for differences between seasons using mvGLM

pacman::p_load(here, data.table, dplyr, ggplot2, mvabund, stringr, vegan)
setwd(here())

# load data ---------------------------------------------------------------
files <- dir(path = "003_results/diatoms/001_speciesXsites_tables/")
files <- files[grep(pattern = "2020-09-08", x = files)]

for (i in seq_along(files)) {
        lv <- files[i]
        obj_name <- lv %>% 
                str_extract("rt[0-9]*_[a-z]*")
        assign(x     = obj_name,
               value = readRDS(paste0("003_results/diatoms/001_speciesXsites_tables/",lv)))
        
}
rm(i, lv, obj_name, files);gc()
file_l <- ls()
# join tables 

loop_over_var <- c(1,2,5,10,11,14:17,19) %>% as.character()
for (i in loop_over_var) {
        
        file_pre <- paste0("rt", i, "_")
        files    <- ls()[grepl(pattern = file_pre, x = ls())]
        ld1 <- get(files[1]) 
        ld2 <- get(files[2])
        ldj <- ld2[ld1, on = "gr_sample_id"]
        if (any(duplicated(ldj$gr_sample_id))) {print(paste(i, "after 1"))}
        rm(ld1, ld2);gc()
        
        assign(x = paste0(file_pre, "all"), 
               value = ldj)
        print(i)
        rm(ldj, files, file_pre, i);gc()
        
}
rm(list = file_l)
rm(file_l, loop_over_var)

files <- ls()

if (nrow(rt1_all[is.na(season)]) != 0 |
    nrow(rt2_all[is.na(season)]) != 0 |
    nrow(rt17_all[is.na(season)]) != 0 |
    nrow(rt19_all[is.na(season)]) != 0) {
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
        rm(ld, i);gc()
}

# fit model  --------------------------------------------------------------

## -- model1 -- ## 
mva_data <- mvabund(rt1_all[,3:271])
model <- manyglm(mva_data ~ season,
                 data = rt1_all, 
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

rt1_model_out$uni.test[2,] %>% sort %>%  plot

saveRDS(rt1_model_out, paste0("003_results/diatoms/002_mvglms/", Sys.Date(), "_rt1.RDS"))


## -- model2 -- ## 
mva_data <- mvabund(rt2_all[,3:127])
model <- manyglm(mva_data ~ season,
                 data = rt2_all, 
                 family = "binomial")

plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)

rt2_model_out <- anova.manyglm(
        model,
        p.uni = "adjusted",
        test = "LR",
        resamp = "pit.trap",
        nBoot = 999, 
        show.time = "all"
)
rt2_model_out$table
rt2_model_out$uni.test[2,] %>% sort %>%  plot
saveRDS(rt2_model_out, paste0("003_results/diatoms/002_mvglms/", Sys.Date(), "_rt2.RDS"))
## -- model3 -- ## 
mva_data <- mvabund(rt17_all[,3:225])
model <- manyglm(mva_data ~ season,
                 data = rt17_all, 
                 family = "binomial")

plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)

rt17_model_out <- anova.manyglm(
        model,
        p.uni = "adjusted",
        test = "LR",
        resamp = "pit.trap",
        nBoot = 999, 
        show.time = "all"
)
rt17_model_out$table
rt17_model_out$uni.test[2,] %>% sort %>%  plot
saveRDS(rt17_model_out, paste0("003_results/diatoms/002_mvglms/", Sys.Date(), "_rt17.RDS"))

## -- model4 -- ## 
mva_data <- mvabund(rt19_all[,3:164])
model <- manyglm(mva_data ~ season,
                 data = rt19_all, 
                 family = "binomial")

plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)

rt19_model_out <- anova.manyglm(
        model,
        p.uni = "adjusted",
        test = "LR",
        resamp = "pit.trap",
        nBoot = 999, 
        show.time = "all"
)
rt19_model_out$table
rt19_model_out$uni.test[2,] %>% sort %>%  plot
saveRDS(rt19_model_out, paste0("003_results/diatoms/002_mvglms/", Sys.Date(), "_rt19.RDS"))
