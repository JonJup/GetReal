# ------------------------------------- #
### --- NMDS of different seasons --- ### 
### --- Macroinvertebrates ---------- ### 
# ------------------------------------- #

# date written: 08.10.20 
# short version of 003_nmds.R in seasonal -> r_scripts -> invertebrates for rlt report 


# setup -------------------------------------------------------------------

pacman::p_load(data.table, dplyr, fuzzySim, ggplot2, here, stringr, vegan)
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
nmds10_11_data <- data.table(NMDS1 = scores(nmds10_11)[,1],  NMDS2 = scores(nmds10_11)[,2], season = factor( rt1011_all$season, levels = c("spring", "summer", "autumn", "winter")))
hull_10_11 <- nmds10_11_data %>% group_by(season) %>% slice(chull(NMDS1, NMDS2))
my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")
nmds_plot <- ggplot(data = nmds10_11_data, aes(x = NMDS1, y = NMDS2)) +  geom_polygon(data = hull_10_11, alpha = 0.5, aes(fill = season)) + geom_point(aes(fill = season), shape = 21) + ggtitle(paste0("NMDS of macroinvertebrate communities in RT10_11", "- Stress: ", round(nmds10_11$stress, 2))) + labs(fil = "Season") + scale_fill_manual(values = my_color_palette[c(1,2,4,6)])
