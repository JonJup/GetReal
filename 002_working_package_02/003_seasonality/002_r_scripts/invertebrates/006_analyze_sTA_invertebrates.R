# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### --------- Macroinvertebrates ----- ###
### --------- Seasons ---------------- ###
# -------------------------------------- #

# date written/ modified: 11.09.20 + 17.09.
# date used: 11.09.20 + 17. + 21.10. 
# Jonathan Jupke 
# Get Real WP2 
# Macroinvertebrates - Seasonality 

# setup -------------------------------------------------------------------
pacman::p_load(data.table,
               dplyr,
               fuzzySim,
               ggplot2,
               here,
               magrittr,
               purrr,
               stringr,
               vegan,
               viridis)

dir_da = here("002_working_package_02/003_seasonality/003_results/invertebrates/")

# load data ---------------------------------------------------------------
files <- dir(dir_da)
files <- files[grep(pattern = "002_indicator", x = files)]
for (i in seq_along(files)) {
    obj_name <- files[i]
    # obj_name %<>% str_remove("[:number:]+_+[:number:]+-+[:number:]+-+[:number:]+_[:alpha:]+_") %>% 
    #     str_remove(".RDS$")
    obj_name %<>% str_remove(".RDS$") %>% 
        str_remove("002_indicator_")
    assign(x = obj_name,
           value = readRDS(file.path(dir_da, files[i])))
    #if (length(get(obj_name)) == 0) rm(list = obj_name)
    rm(obj_name, i)
}
# create typical communities ----------------------------------------------

ta_rt23s <- rt23s[B > 0.20]
ta_rtlarges <- rtlarges[B > 0.20]
ta_rt23g <- rt23g[B > 0.33]
ta_rtlargeg <- rtlargeg[B > 0.33]
ta_rt23f <- rt23f[B > 0.55]
ta_rtlargef <- rtlargef[B > 0.55]

# rt23s %>% ggplot(aes(x = B)) + geom_histogram(binwidth = .01) + xlim(0,1)
# rt23s %>% ggplot(aes(x = A)) + geom_histogram(binwidth = .01) + xlim(0,1)
# rtlarges %>% ggplot(aes(x = B)) + geom_histogram(binwidth = .01) + xlim(0,1)
# rtlarges %>% ggplot(aes(x = A)) + geom_histogram(binwidth = .01) + xlim(0,1)
# rt23g %>% ggplot(aes(x = B)) + geom_histogram(binwidth = .01) + xlim(0,1) + geom_vline(xintercept = 0.5, col = "red")
# rt23g %>% ggplot(aes(x = A)) + geom_histogram(binwidth = .01) + xlim(0,1)
# rtlargeg %>% ggplot(aes(x = B)) + geom_histogram(binwidth = .01) + xlim(0,1) + geom_vline(xintercept = 0.5, col = "red")
# rtlargeg %>% ggplot(aes(x = A)) + geom_histogram(binwidth = .01) + xlim(0,1)

ta_2_3 <- rbind(ta_rt23s, ta_rt23g, ta_rt23f)
ta_large <- rbind(ta_rtlarges, ta_rtlargeg, ta_rtlargef)

rm_files <- paste0("rt", rep(c("23","large"), each = 3), rep(c("s","g","f"), times = 2))
rm(list = rm_files)
rm_files <- paste0("ta_", rm_files)
rm(list = rm_files)
rm(rm_files)
gc()

#### How similar are the seasonal assemblages to each other #### 

ta_2_3$season <- factor(ta_2_3$season, levels = c("spring", "summer", "autumn", "winter"))
ta_large$season <- factor(ta_large$season, levels = c("spring", "summer", "autumn", "winter"))

rv <- c("2_3","large")
ll <- list()
for (l in 1:2){
    ld <- get(paste0("ta_",rv[l]))
    ld$season <- droplevels(ld$season)
    sv <- levels(ld$season)
    om <- data.frame(matrix(data = 0, ncol =  length(sv), nrow = length(sv)))
    names(om) <- rownames(om) <- sv
    for (i in sv) {
        ls <- ld[season == i, unique(taxon)]
        om_row <- which(row.names(om) == i)
        for (k in sv) {
            om_col <- which(names(om) == k)
            sls <- ld[season == k, unique(taxon)]
            om[om_row, om_col] <- round(sum(ls %in% sls)/length(ls) * 100,1)
            rm(om_col, sls, k);gc()
        }
        rm(om_row, ls, i);gc()
    }
    ll[[l]] <- om
}

om1 <- ll[[1]]
om2 <- ll[[2]]

om1$N <- table(droplevels(ta_2_3$season))
om2$N <- table(droplevels(ta_large$season))

rm(sv, ll, ld, l, om, rv)


saveRDS(om1, file.path(dir_da, "seasonal_overlap_RT_2_3.RDS"))
saveRDS(om2, file.path(dir_da, "seasonal_overlap_RT_large.RDS"))

# A closer Look  ----------------------------------------------------------

### --- RT10 + 11 --- ### 
ta_2_3_spring <- ta_2_3[season == "spring"]
ta_2_3_summer <- ta_2_3[season == "summer"]
ta_2_3_autumn <- ta_2_3[season == "autumn"]
ta_2_3_winter <- ta_2_3[season == "winter"]

paste(ta_2_3_spring$taxon %in% ta_2_3_summer$taxon %>% sum(), "of", length(ta_2_3_spring$taxon), "|", length(ta_2_3_summer$taxon))
paste(ta_2_3_spring$taxon %in% ta_2_3_autumn$taxon %>% sum(), "of", length(ta_2_3_spring$taxon), "|", length(ta_2_3_autumn$taxon))
paste(ta_2_3_spring$taxon %in% ta_2_3_winter$taxon %>% sum(), "of", length(ta_2_3_spring$taxon), "|", length(ta_2_3_winter$taxon))
paste(ta_2_3_summer$taxon %in% ta_2_3_autumn$taxon %>% sum(), "of", length(ta_2_3_summer$taxon), "|", length(ta_2_3_autumn$taxon))
paste(ta_2_3_summer$taxon %in% ta_2_3_winter$taxon %>% sum(), "of", length(ta_2_3_summer$taxon), "|", length(ta_2_3_winter$taxon))
paste(ta_2_3_autumn$taxon %in% ta_2_3_winter$taxon %>% sum(), "of", length(ta_2_3_autumn$taxon), "|", length(ta_2_3_winter$taxon))

ta_2_3_spring$taxon[which(ta_2_3_spring$taxon %in% ta_2_3_summer$taxon)]
ta_2_3_spring$taxon[which(ta_2_3_spring$taxon %in% ta_2_3_autumn$taxon)]
ta_2_3_spring$taxon[which(ta_2_3_spring$taxon %in% ta_2_3_winter$taxon)]
ta_2_3_summer$taxon[which(ta_2_3_summer$taxon %in% ta_2_3_autumn$taxon)]
ta_2_3_summer$taxon[which(ta_2_3_summer$taxon %in% ta_2_3_winter$taxon)]
ta_2_3_autumn$taxon[which(ta_2_3_autumn$taxon %in% ta_2_3_winter$taxon)]

ta_2_3_summer$taxon
ta_2_3_autumn$taxon
ta_2_3_winter$taxon


# overlap all 0 taxa are in all seasons. 
all_23_id <- which(ta_2_3_winter$taxon %in% ta_2_3_autumn$taxon & 
                         ta_2_3_winter$taxon %in% ta_2_3_summer$taxon &
                         ta_2_3_winter$taxon %in% ta_2_3_spring$taxon)
length(all_23_id)


### --- RT15 + 16 --- ### 
ta_large_spring <- ta_large[season == "spring"]
ta_large_summer <- ta_large[season == "summer"]
ta_large_autumn <- ta_large[season == "autumn"]
ta_large_winter <- ta_large[season == "winter"]

paste(ta_large_spring$taxon %in% ta_large_summer$taxon %>% sum(), "of", length(ta_large_spring$taxon), "|", length(ta_large_summer$taxon))
paste(ta_large_spring$taxon %in% ta_large_autumn$taxon %>% sum(), "of", length(ta_large_spring$taxon), "|", length(ta_large_autumn$taxon))
paste(ta_large_spring$taxon %in% ta_large_winter$taxon %>% sum(), "of", length(ta_large_spring$taxon), "|", length(ta_large_winter$taxon))
paste(ta_large_summer$taxon %in% ta_large_autumn$taxon %>% sum(), "of", length(ta_large_summer$taxon), "|", length(ta_large_autumn$taxon))
paste(ta_large_summer$taxon %in% ta_large_winter$taxon %>% sum(), "of", length(ta_large_summer$taxon), "|", length(ta_large_winter$taxon))
paste(ta_large_autumn$taxon %in% ta_large_winter$taxon %>% sum(), "of", length(ta_large_autumn$taxon), "|", length(ta_large_winter$taxon))

ta_large_spring$taxon[which(ta_large_spring$taxon %in% ta_large_summer$taxon)]
ta_large_spring$taxon[which(ta_large_spring$taxon %in% ta_large_autumn$taxon)]
ta_large_spring$taxon[which(ta_large_spring$taxon %in% ta_large_winter$taxon)]
ta_large_summer$taxon[which(ta_large_summer$taxon %in% ta_large_autumn$taxon)]
ta_large_summer$taxon[which(ta_large_summer$taxon %in% ta_large_winter$taxon)]
ta_large_autumn$taxon[which(ta_large_autumn$taxon %in% ta_large_winter$taxon)]

ta_large_spring$taxon
ta_large_summer$taxon
ta_large_autumn$taxon
ta_large_winter$taxon


# overlap all 0 taxa are in all seasons. 
all_large_id <- which(ta_large_winter$taxon %in% ta_large_autumn$taxon & 
                         ta_large_winter$taxon %in% ta_large_summer$taxon &
                         ta_large_winter$taxon %in% ta_large_spring$taxon)


### --- ### 
ta_2_3[season == "autumn", unique(taxon)] %>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()

ta_large[season == "winter", unique(taxon)]%>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()


