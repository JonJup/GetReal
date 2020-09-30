# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### --------- Diatoms ---------------- ###
### --------- Seasons ---------------- ###
# -------------------------------------- #

# date written/ modified: 09.09.20 + 16. + 17. 
# date used: 09.09.20 + 16. + 17.  
# Jonathan Jupke 
# Get Real WP2 
# Diatoms - Seasonality 

# setup -------------------------------------------------------------------
pacman::p_load(dplyr, magrittr, data.table, stringr, taxize, purrr, ggplot2, here, viridis, fuzzySim, vegan, ggplot2, faraway)
setwd(here("003_results/diatoms"))

# load data ---------------------------------------------------------------
files <- dir()
files <- files[grep(pattern = "002_2020", x = files)]
for (i in seq_along(files)) {
    obj_name <- files[i]
    obj_name %<>% str_remove("[:number:]+_+[:number:]+-+[:number:]+-+[:number:]+_[:alpha:]+_") %>% 
        str_remove(".RDS$")
    assign(x = obj_name,
           value = readRDS(files[i]))
    if (length(get(obj_name)) == 0) rm(list = obj_name)
}
rm(obj_name, i, files);gc()

# create typical communities ----------------------------------------------
files <- ls()

ta_s_11 <- s_11[B > 0.4 | (B>0.3 & p_value <= 0.05) | A > 0.70]
ta_s_15 <- s_15[B > 0.4 | (B>0.3 & p_value <= 0.05) | A > 0.70]
ta_g_11 <- g_11[B > 0.8 | (B>0.6 & p_value <= 0.05) | A > 0.95]
ta_g_15 <- g_15[B > 0.8 | (B>0.6 & p_value <= 0.05) | A > 0.95]

ta_11 <- rbind(ta_s_11, ta_g_11)
ta_15 <- rbind(ta_s_15, ta_g_15)

rm_files <- paste0(c("s", "g"), "_", rep(c("11","15"), each = 2))
rm(list = rm_files)
rm_files <- paste0("ta_", c("s", "g"), "_", rep(c("11","15"), each = 2))
rm(list = rm_files)
rm(rm_files)
gc()



#### How similar are the seasonal assemblages to each other #### 

rv <- c("11","15")
ll <- list()
for (l in 1:2){
    ld <- get(paste0("ta_",rv[l]))
    sv <- unique(ld$season)
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

om11 <- ll[[1]]
om15 <- ll[[2]]

rm(sv, ll, ld, l)

om11
om15

# A closer Look  ----------------------------------------------------------

### --- RT11 --- ### 
ta_11_summer <- ta_11[season == "summer"]
ta_11_autumn <- ta_11[season == "autumn"]
ta_11_winter <- ta_11[season == "winter"]


paste(ta_11_summer$taxon %in% ta_11_autumn$taxon %>% sum(), "of", length(ta_11_summer$taxon), "|", length(ta_11_autumn$taxon))
paste(ta_11_summer$taxon %in% ta_11_winter$taxon %>% sum(), "of", length(ta_11_summer$taxon), "|", length(ta_11_winter$taxon))
paste(ta_11_autumn$taxon %in% ta_11_winter$taxon %>% sum(), "of", length(ta_11_autumn$taxon), "|", length(ta_11_winter$taxon))

ta_11_summer$taxon[which(ta_11_summer$taxon %in% ta_11_autumn$taxon)]
ta_11_summer$taxon[which(ta_11_summer$taxon %in% ta_11_winter$taxon)]
ta_11_summer$taxon
ta_11_autumn$taxon
ta_11_winter$taxon



# overlap all 8 taxa are in all seasons. 
all_11_id <- which(ta_11_winter$taxon %in% ta_11_autumn$taxon & ta_11_winter$taxon %in% ta_11_summer$taxon )
length(all_11_id)
ta_11_winter$taxon[all_11_id]
### --- RT15 --- ### 
ta_15_summer <- ta_15[season == "summer"]
ta_15_winter <- ta_15[season == "winter"]
ta_15_autumn <- ta_15[season == "autumn"]

paste(ta_15_summer$taxon %in% ta_15_autumn$taxon %>% sum(), "of", length(ta_15_summer$taxon), "|", length(ta_15_autumn$taxon))
paste(ta_15_summer$taxon %in% ta_15_winter$taxon %>% sum(), "of", length(ta_15_summer$taxon), "|", length(ta_15_winter$taxon))
paste(ta_15_autumn$taxon %in% ta_15_winter$taxon %>% sum(), "of", length(ta_15_autumn$taxon), "|", length(ta_15_winter$taxon))

ta_15_summer$taxon[which(ta_15_summer$taxon %in% ta_15_autumn$taxon)]
ta_15_summer$taxon[which(ta_15_summer$taxon %in% ta_15_winter$taxon)]
ta_15_autumn$taxon[which(ta_15_summer$taxon %in% ta_15_winter$taxon)]
ta_15_summer$taxon
ta_15_autumn$taxon
ta_15_winter$taxon

i1 <- intersect(ta_15_summer$taxon, ta_15_autumn$taxon)
i2 <- intersect(i1,ta_15_winter$taxon)
i3 <- intersect(i2,ta_11_summer$taxon)
i4 <- intersect(i3,ta_11_autumn$taxon)
i5 <- intersect(i4,ta_11_winter$taxon)

i15sa <- intersect(ta_15_summer$taxon, ta_15_autumn$taxon)
i15sw <- intersect(ta_15_summer$taxon, ta_15_winter$taxon)
i15aw <- intersect(ta_15_autumn$taxon, ta_15_winter$taxon)

i11sa <- intersect(ta_11_summer$taxon, ta_11_autumn$taxon)
i11sw <- intersect(ta_11_summer$taxon, ta_11_winter$taxon)
i11aw <- intersect(ta_11_autumn$taxon, ta_11_winter$taxon)
i15sa <- intersect(ta_15_summer$taxon, ta_15_autumn$taxon)
i15sw <- intersect(ta_15_summer$taxon, ta_15_winter$taxon)
i15aw <- intersect(ta_15_autumn$taxon, ta_15_winter$taxon)

i_all <- intersect(i15sa, i15aw)
setdiff(i11aw ,i_all)
setdiff(ta_11_summer$taxon ,i11aw)

i15aw %>% str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", \n") %>%
    writeClipboard()
    


setdiff(i15aw, i15sw)
"Nitzschia.palea.paleacea" %in% ta_15_winter$taxon


sum(!ta_15_winter$taxon %in% ta_15_autumn$taxon & !ta_15_winter$taxon %in% ta_15_autumn$taxon )



    # overlap all: 6 taxa are in all seasons. 
all_15_id <- which(ta_15_winter$taxon %in% ta_15_autumn$taxon & ta_15_winter$taxon %in% ta_15_summer$taxon )
length(all_15_id)
ta_15_winter$taxon[all_15_id]

all1115 <- which(ta_15_winter$taxon[all_15_id] %in% ta_11_winter$taxon[all_11_id])
length(all1115)
ta_15_winter$taxon[all_15_id][all1115]

### --- ### 
ta_11[season == "autumn", unique(taxon)][both_id]%>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()
ta_15_summer[, unique(taxon)] %>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", \n") %>%
    writeClipboard()
