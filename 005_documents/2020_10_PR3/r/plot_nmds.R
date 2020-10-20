# ------------------------------------- #
### --- NMDS of different seasons --- ### 
### --- Macroinvertebrates ---------- ###
### --- Presentation 
###     GET REAL 
###     Progress Review 3 ----------- ###
# ------------------------------------- #

# date written: 19.10.20 

# NMDS of seasonal invertebrate assemblages. For Progress Review 3 presentation. 

# setup -------------------------------------------------------------------
options(warn=-1)
pacman::p_load(data.table, 
               dplyr, 
               fuzzySim, 
               ggplot2, 
               here, 
               stringr, 
               vegan)
setwd(here())

# load data ---------------------------------------------------------------

files <- dir(path = "002_working_package_02/003_seasonality/003_results/invertebrates/001_speciesXsites_tables/")
files <- files[grepl(pattern = "2020-09-10", x = files)]
files <- files[grepl(pattern = "rt10", x = files)]

for (i in seq_along(files)) {
        lv <- files[i]
        obj_name <- lv %>% 
                str_extract("rt.*") %>% 
                str_remove(pattern = "\\.RDS")
        assign(x     = obj_name,
               value = readRDS(paste0("002_working_package_02/003_seasonality/003_results/invertebrates/001_speciesXsites_tables/",lv)))
        
}
rm(i,lv)
rt1011_all <- rt10_11_foh[rt10_11_gen, on = "gr_sample_id"]
rt1011_all <- rt10_11_spe[rt1011_all, on = "gr_sample_id"]


seas_id <- which(str_detect(string = names(rt1011_all), pattern = "season"))
rt1011_all[is.na(season) & !is.na(i.season), season := i.season]
rt1011_all[, i.season := NULL]
if (length(seas_id) > 2){
        rt1011_all[is.na(season) & !is.na(i.season.1), season := i.season.1]
        rt1011_all[, i.season.1 := NULL]
}

# fix NAs with code from Matt Dowle (https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table)
for (j in seq_len(ncol(rt1011_all))){
                set(rt1011_all, which(is.na(rt1011_all[[j]])), j, 0)
}

nmds10_11   <- readRDS("002_working_package_02/003_seasonality/003_results/invertebrates/004_nmds/2020-09-10_nmds_object_invertebrates_rt10_11.RDS") 
nmds10_11_data <- data.table(NMDS1 = scores(nmds10_11)[,1],
                             NMDS2 = scores(nmds10_11)[,2], 
                             season = factor(rt1011_all$season, levels = c("spring", "summer", "autumn", "winter"))
                             )
hull_10_11 <- nmds10_11_data %>% 
        group_by(season) %>% 
        slice(chull(NMDS1, NMDS2))
nmds_plot = nmds10_11_data %>% 
        ggplot(aes(x = NMDS1, 
                   y = NMDS2)) +  
        geom_polygon(data = hull_10_11, 
                     alpha = 0.5, 
                     aes(fill = season, col=season)) + 
        geom_point(aes(col = season)) + 
        labs(fil = "Season")  +
        theme(text=element_text(size=20)) + 
        theme_xaringan(background_color = base_color_code2) +
        scale_xaringan_color_discrete() + 
        scale_xaringan_fill_discrete() 
