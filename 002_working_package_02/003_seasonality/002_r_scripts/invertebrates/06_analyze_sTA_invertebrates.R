# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### --------- Macroinvertebrates ----- ###
### --------- Seasons ---------------- ###
# -------------------------------------- #

# date written/ modified: 11.09.20 + 17.09.
# date used: 11.09.20 + 17. + 21.10. + 27.11
# Jonathan Jupke 
# Get Real WP2 
# Macroinvertebrates - Seasonality 

# SETUP -------------------------------------------------------------------
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

DIR = list(data = here("002_working_package_02/003_seasonality/003_results/invertebrates/"), 
           rs   = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/002_r_scripts/"))

# LOAD --------------------------------------------------------------------
ls_sta = readRDS(file.path(DIR$data, "sta_list.RDS"))

# COMPUTE sTA ----------------------------------------------
# load thresholds
source(textConnection(readLines(file.path(DIR$rs, "08_a_ta_master.R"))[c(25:27)]))

ls_sta2 = list()
ls_sta2$s10 = ls_sta$spe10[B > x_ls_thesholds$spe$b | (A > x_ls_thesholds$spe$a & B > x_ls_thesholds$spe$b2)]
ls_sta2$g10 = ls_sta$gen10[B > x_ls_thesholds$gen$b | (A > x_ls_thesholds$gen$a & B > x_ls_thesholds$gen$b2)]
ls_sta2$f10 = ls_sta$fam10[B > x_ls_thesholds$foh$b | (A > x_ls_thesholds$foh$a & B > x_ls_thesholds$foh$b2)]
ls_sta2$s16 = ls_sta$spe16[B > x_ls_thesholds$spe$b | (A > x_ls_thesholds$spe$a & B > x_ls_thesholds$spe$b2)]
ls_sta2$g16 = ls_sta$gen16[B > x_ls_thesholds$gen$b | (A > x_ls_thesholds$gen$a & B > x_ls_thesholds$gen$b2)]
ls_sta2$f16 = ls_sta$fam16[B > x_ls_thesholds$foh$b | (A > x_ls_thesholds$foh$a & B > x_ls_thesholds$foh$b2)]

ls_sta3 = list(
    "ten"     = rbind(ls_sta2$s10, ls_sta2$g10, ls_sta2$f10),
    "sixteen" = rbind(ls_sta2$s16, ls_sta2$g16, ls_sta2$f16)
)

ls_sta3$ten$season     %<>% factor(levels=c("spring", "summer", "autumn", "winter"))
ls_sta3$sixteen$season %<>% factor(levels=c("spring", "summer", "autumn", "winter"))

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

om10 <- ll[[1]]
om16 <- ll[[2]]

om10$N <- table(droplevels(ls_sta3$ten$season))
om16$N <- table(droplevels(ls_sta3$sixteen$season))

# save tables for report 
saveRDS(om10, file.path(DIR$data, "tbl_sta_mzb_10.RDS"))
saveRDS(om16, file.path(DIR$data, "tbl_sta_mzb_16.RDS"))

ch_river_types = c("RT10", "RT16")
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


