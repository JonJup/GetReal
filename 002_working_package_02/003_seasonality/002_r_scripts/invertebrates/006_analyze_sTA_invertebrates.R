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

setwd(here("002_working_package_02/003_seasonality/003_results/invertebrates/"))

# load data ---------------------------------------------------------------
files <- dir()
files <- files[grep(pattern = "002_2020", x = files)]
for (i in seq_along(files)) {
    obj_name <- files[i]
    obj_name %<>% str_remove("[:number:]+_+[:number:]+-+[:number:]+-+[:number:]+_[:alpha:]+_") %>% 
        str_remove(".RDS$")
    assign(x = obj_name,
           value = readRDS(files[i]))
    #if (length(get(obj_name)) == 0) rm(list = obj_name)
    rm(obj_name, i)
}
# create typical communities ----------------------------------------------

ta_rt1011s <- rt1011s[B > 0.25 | (B > 0.20 & p_value <= 0.05)]
ta_rt1516s <- rt1516s[B > 0.25 | (B > 0.20 & p_value <= 0.05)]
ta_rt1011g <- rt1011g[B > 0.50 | (B > 0.33 & p_value <= 0.05)]
ta_rt1516g <- rt1516g[B > 0.50 | (B > 0.33 & p_value <= 0.05)]
ta_rt1011f <- rt1011f[B > 0.95 | (B > 0.80 & p_value <= 0.05)]
ta_rt1516f <- rt1516f[B > 0.95 | (B > 0.80 & p_value <= 0.05)]

# rt1011s %>% ggplot(aes(x = B)) + geom_histogram(binwidth = .01) + xlim(0,1)
# rt1011s %>% ggplot(aes(x = A)) + geom_histogram(binwidth = .01) + xlim(0,1)
# rt1516s %>% ggplot(aes(x = B)) + geom_histogram(binwidth = .01) + xlim(0,1)
# rt1516s %>% ggplot(aes(x = A)) + geom_histogram(binwidth = .01) + xlim(0,1)
# rt1011g %>% ggplot(aes(x = B)) + geom_histogram(binwidth = .01) + xlim(0,1) + geom_vline(xintercept = 0.5, col = "red")
# rt1011g %>% ggplot(aes(x = A)) + geom_histogram(binwidth = .01) + xlim(0,1)
# rt1516g %>% ggplot(aes(x = B)) + geom_histogram(binwidth = .01) + xlim(0,1) + geom_vline(xintercept = 0.5, col = "red")
# rt1516g %>% ggplot(aes(x = A)) + geom_histogram(binwidth = .01) + xlim(0,1)

ta_10_11 <- rbind(ta_rt1011s, ta_rt1011g, ta_rt1011f)
ta_15_16 <- rbind(ta_rt1516s, ta_rt1516g, ta_rt1516f)

rm_files <- paste0("rt", rep(c("1011","1516"), each = 3), rep(c("s","g","f"), times = 2))
rm(list = rm_files)
rm_files <- paste0("ta_", rm_files)
rm(list = rm_files)
rm(rm_files)
gc()

#### How similar are the seasonal assemblages to each other #### 

ta_10_11$season <- factor(ta_10_11$season, levels = c("spring", "summer", "autumn", "winter"))
ta_15_16$season <- factor(ta_15_16$season, levels = c("spring", "summer", "autumn", "winter"))

rv <- c("10_11","15_16")
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

rm(sv, ll, ld, l, om, rv)

om1$N <- table(ta_10_11$season) 
om2$N <- table(ta_15_16$season)

saveRDS(om1,"../../../../005_documents/2020_10_Zwischenbericht an RLT/r/tbl_sta_mzb_11.RDS")
saveRDS(om2,"../../../../005_documents/2020_10_Zwischenbericht an RLT/r/tbl_sta_mzb_15.RDS")

# A closer Look  ----------------------------------------------------------

### --- RT10 + 11 --- ### 
ta_10_11_spring <- ta_10_11[season == "spring"]
ta_10_11_summer <- ta_10_11[season == "summer"]
ta_10_11_autumn <- ta_10_11[season == "autumn"]
ta_10_11_winter <- ta_10_11[season == "winter"]

paste(ta_10_11_spring$taxon %in% ta_10_11_summer$taxon %>% sum(), "of", length(ta_10_11_spring$taxon), "|", length(ta_10_11_summer$taxon))
paste(ta_10_11_spring$taxon %in% ta_10_11_autumn$taxon %>% sum(), "of", length(ta_10_11_spring$taxon), "|", length(ta_10_11_autumn$taxon))
paste(ta_10_11_spring$taxon %in% ta_10_11_winter$taxon %>% sum(), "of", length(ta_10_11_spring$taxon), "|", length(ta_10_11_winter$taxon))
paste(ta_10_11_summer$taxon %in% ta_10_11_autumn$taxon %>% sum(), "of", length(ta_10_11_summer$taxon), "|", length(ta_10_11_autumn$taxon))
paste(ta_10_11_summer$taxon %in% ta_10_11_winter$taxon %>% sum(), "of", length(ta_10_11_summer$taxon), "|", length(ta_10_11_winter$taxon))
paste(ta_10_11_autumn$taxon %in% ta_10_11_winter$taxon %>% sum(), "of", length(ta_10_11_autumn$taxon), "|", length(ta_10_11_winter$taxon))

ta_10_11_spring$taxon[which(ta_10_11_spring$taxon %in% ta_10_11_summer$taxon)]
ta_10_11_spring$taxon[which(ta_10_11_spring$taxon %in% ta_10_11_autumn$taxon)]
ta_10_11_spring$taxon[which(ta_10_11_spring$taxon %in% ta_10_11_winter$taxon)]
ta_10_11_summer$taxon[which(ta_10_11_summer$taxon %in% ta_10_11_autumn$taxon)]
ta_10_11_summer$taxon[which(ta_10_11_summer$taxon %in% ta_10_11_winter$taxon)]
ta_10_11_autumn$taxon[which(ta_10_11_autumn$taxon %in% ta_10_11_winter$taxon)]

ta_10_11_summer$taxon
ta_10_11_autumn$taxon
ta_10_11_winter$taxon


# overlap all 0 taxa are in all seasons. 
all_1011_id <- which(ta_10_11_winter$taxon %in% ta_10_11_autumn$taxon & 
                         ta_10_11_winter$taxon %in% ta_10_11_summer$taxon &
                         ta_10_11_winter$taxon %in% ta_10_11_spring$taxon)
length(all_1011_id)


### --- RT15 + 16 --- ### 
ta_15_16_spring <- ta_15_16[season == "spring"]
ta_15_16_summer <- ta_15_16[season == "summer"]
ta_15_16_autumn <- ta_15_16[season == "autumn"]
ta_15_16_winter <- ta_15_16[season == "winter"]

paste(ta_15_16_spring$taxon %in% ta_15_16_summer$taxon %>% sum(), "of", length(ta_15_16_spring$taxon), "|", length(ta_15_16_summer$taxon))
paste(ta_15_16_spring$taxon %in% ta_15_16_autumn$taxon %>% sum(), "of", length(ta_15_16_spring$taxon), "|", length(ta_15_16_autumn$taxon))
paste(ta_15_16_spring$taxon %in% ta_15_16_winter$taxon %>% sum(), "of", length(ta_15_16_spring$taxon), "|", length(ta_15_16_winter$taxon))
paste(ta_15_16_summer$taxon %in% ta_15_16_autumn$taxon %>% sum(), "of", length(ta_15_16_summer$taxon), "|", length(ta_15_16_autumn$taxon))
paste(ta_15_16_summer$taxon %in% ta_15_16_winter$taxon %>% sum(), "of", length(ta_15_16_summer$taxon), "|", length(ta_15_16_winter$taxon))
paste(ta_15_16_autumn$taxon %in% ta_15_16_winter$taxon %>% sum(), "of", length(ta_15_16_autumn$taxon), "|", length(ta_15_16_winter$taxon))

ta_15_16_spring$taxon[which(ta_15_16_spring$taxon %in% ta_15_16_summer$taxon)]
ta_15_16_spring$taxon[which(ta_15_16_spring$taxon %in% ta_15_16_autumn$taxon)]
ta_15_16_spring$taxon[which(ta_15_16_spring$taxon %in% ta_15_16_winter$taxon)]
ta_15_16_summer$taxon[which(ta_15_16_summer$taxon %in% ta_15_16_autumn$taxon)]
ta_15_16_summer$taxon[which(ta_15_16_summer$taxon %in% ta_15_16_winter$taxon)]
ta_15_16_autumn$taxon[which(ta_15_16_autumn$taxon %in% ta_15_16_winter$taxon)]

ta_15_16_spring$taxon
ta_15_16_summer$taxon
ta_15_16_autumn$taxon
ta_15_16_winter$taxon


# overlap all 0 taxa are in all seasons. 
all_1516_id <- which(ta_15_16_winter$taxon %in% ta_15_16_autumn$taxon & 
                         ta_15_16_winter$taxon %in% ta_15_16_summer$taxon &
                         ta_15_16_winter$taxon %in% ta_15_16_spring$taxon)


### --- ### 
ta_10_11[season == "winter", unique(taxon)] %>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()

ta_15_16[season == "winter", unique(taxon)]%>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()

ta15[, unique(taxon)] %>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()
