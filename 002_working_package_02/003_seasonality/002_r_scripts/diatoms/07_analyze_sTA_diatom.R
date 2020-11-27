# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### --------- Diatoms ---------------- ###
### --------- Seasons ---------------- ###
# -------------------------------------- #

# date written/ modified: 09.09.20 + 16. + 17. 
# date used: 09.09.20 + 16. + 17. + 
#            07.10. + 21. + 
#            16.11 + 27.
# Jonathan Jupke 
# Get Real WP2 
# Diatoms - Seasonality 

# SETUP -------------------------------------------------------------------
pacman::p_load(
    data.table,
    dplyr,
    faraway,
    fuzzySim,
    ggplot2,
    here,
    magrittr,
    purrr,
    stringr,
    taxize,
    vegan,
    viridis
)

## DIRECTORIES 
DIR = list(
        data = here("002_working_package_02/003_seasonality/003_results/diatoms/"),
        rs   = here("002_working_package_02/001_community_data/002_combined/001_diatoms/002_r_scripts/")
)


# LOAD ---------------------------------------------------------------
ls_sta = readRDS(file.path(DIR$data, "sta_list.RDS"))

# COMPUTE sTA ----------------------------------------------
# load thresholds
source(textConnection(readLines(file.path(DIR$rs, "10_a_ta_master.R"))[c(28,29)]))

ls_sta2 = list()
ls_sta2$s16 = ls_sta$spe_16[B > x_ls_thesholds$spe$b | (A > x_ls_thesholds$spe$a & B > x_ls_thesholds$spe$b2)]
ls_sta2$g16 = ls_sta$gen_16[B > x_ls_thesholds$gen$b | (A > x_ls_thesholds$gen$a & B > x_ls_thesholds$gen$b2)]
ls_sta2$s17_18 = ls_sta$spe_17_18[B > x_ls_thesholds$spe$b | (A > x_ls_thesholds$spe$a & B > x_ls_thesholds$spe$b2)]
ls_sta2$g17_18 = ls_sta$gen_17_18[B > x_ls_thesholds$gen$b | (A > x_ls_thesholds$gen$a & B > x_ls_thesholds$gen$b2)]

ls_sta3 = list(
    "sixteen" = rbind(ls_sta2$s16, ls_sta2$g16),
    "seventeen" = rbind(ls_sta2$s17_18, ls_sta2$g17_18)
)

ls_sta3$sixteen$season   %<>% factor(levels=c("spring", "summer", "autumn", "winter"))
ls_sta3$seventeen$season %<>% factor(levels=c("spring", "summer", "autumn", "winter"))

#### How similar are the seasonal assemblages to each other #### 

ll <- list()
for (l in seq_along(ls_sta3)){
    ld <- ls_sta3[[l]]
    ld$season=droplevels(ld$season)
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
            # a   = length(intersect(ls, sls))
            # bc  = length(setdiff(ls, sls))
            # abc = a + bc
            # jcd = a / abc
            # om[om_row, om_col] <- round(jcd,2)
            # rm(om_col, sls, k);gc()
        }
        rm(om_row, ls, i);gc()
    }
    ll[[l]] <- om
}

om16 <- ll[[1]]
om17 <- ll[[2]]

om16$N <- table(droplevels(ls_sta3$sixteen$season))
om17$N <- table(droplevels(ls_sta3$seventeen$season))

# SAVE --------------------------------------------------------------------
saveRDS(om16, file.path(DIR$data, "tbl_sta_dia_16.RDS"))
saveRDS(om17, file.path(DIR$data, "tbl_sta_dia_17.RDS"))

# MAKE sTA lists  ---------------------------------------------------------

ls_sta3

ch_river_types = c("RT16", "RT17_18")
copy_list = list()
counter = 1
for (i in seq_along(ch_river_types)){
    loop_var    = ls_sta3[[i]]
    seasons_var = uniqueN(loop_var$season)
    for (j in 1:seasons_var){
       ch_temp =  loop_var[season == unique(season)[j], taxon] %>% 
            str_replace_all(pattern = "\\.", "\\ ") %>%
            paste(sep = ",", collapse = ", ") 
       copy_list[[counter]] = paste(paste0("## ",ch_river_types[i]," ", unique(loop_var$season)[j]), ch_temp, "   ") 
       counter = counter + 1
    }
}

copy_list %>% 
    unlist %>% 
    writeClipboard()

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
ta_11[season == "winter", unique(taxon)] %>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()

ta_15[season=="winter", unique(taxon)] %>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()
