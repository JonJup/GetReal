# -- Plot GDM for RLT Report 

# date: 08.10.20

# setup -------------------------------------------------------------------
pacman::p_load(cowplot,
               gdm, 
               here,
               magrittr,
               sf,
               stringr
)
setwd(here())
options(warn=-1)
# load and prepare data  --------------------------------------------------
files <- dir(path = "002_working_package_02/003_seasonality/003_results/invertebrates/001_speciesXsites_tables/")
files <- files[grep(pattern = "2020-09-10", x = files)]
files <- files[grep(pattern = "rt10", x = files)]

for (i in seq_along(files)) {
        lv <- files[i]
        obj_name <- lv %>% 
                str_extract("rt.*") %>% 
                str_remove(pattern = "\\.RDS")
        assign(x     = obj_name,
               value = readRDS(paste0("002_working_package_02/003_seasonality/003_results/invertebrates/001_speciesXsites_tables/",lv)))
        
}
rm(i, lv, obj_name, files);gc()
file_l <- ls()

rt1011_all <- rt10_11_foh[rt10_11_gen, on = "gr_sample_id"]
rt1011_all <- rt10_11_spe[rt1011_all, on = "gr_sample_id"]

seas_id <- which(str_detect(string = names(rt1011_all), pattern = "season"))
rt1011_all[is.na(season) & !is.na(i.season), season := i.season]
rt1011_all[, i.season := NULL]
if (length(seas_id) > 2){
        rt1011_all[is.na(season) & !is.na(i.season.1), season := i.season.1]
        rt1011_all[, i.season.1 := NULL]}


# fix NAs with code from Matt Dowle (https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table)
for (j in seq_len(ncol(rt1011_all))) set(rt1011_all, which(is.na(rt1011_all[[j]])), j, 0)


## -- site locations -- ## 
data <- readRDS("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/004_2020-07-03_mzb_data1585_low_impact.RDS")
data <- data[,c("gr_sample_id", "geom")]
data <- unique(data, by = "gr_sample_id")
data %<>% st_as_sf()
data$x <- st_coordinates(data)[,1] %>%  as.numeric()
data$y <- st_coordinates(data)[,2] %>%  as.numeric()
data %<>% st_drop_geometry()
# - 
# Fitting models ----------------------------------------------------------
loop_data       <- rt1011_all
n_col           <- ncol(loop_data) - 1
env_data        <- loop_data[,1:2]
env_data        <- data[env_data, on = "gr_sample_id"]
input_data      <- as.data.frame(loop_data[,c(1,3:n_col), with =F])
env_data        %<>% as.data.frame
env_data$season %<>% factor(levels = c("spring", "summer", "autumn", "winter"))
        
gdmTab <- formatsitepair(bioData = input_data, 
                                 bioFormat = 1,
                                 dist = "jaccard",
                                 siteColumn = "gr_sample_id",
                                 predData = env_data,
                                 XColumn = "x",
                                 YColumn = "y",
                                 sampleSites = 1
)
fit_model  <- gdm(gdmTab, geo = T)
splineDat  <- isplineExtract(fit_model)
splineDat_x <- splineDat$x %>% as.data.frame
splineDat_y <- splineDat$y %>% as.data.frame
names(splineDat_x) <- paste0("x_", names(splineDat_x))
names(splineDat_y) <- paste0("y_", names(splineDat_y))
splineDat <- cbind(splineDat_x, splineDat_y)
max_geo    <- max(splineDat$y_Geographic)
max_season <- max(splineDat$y_season)
y_max <- max(max_geo, max_season)

p1 <- ggplot(data = splineDat, 
             aes(x = x_Geographic,
                 y = y_Geographic)
             ) + 
        geom_line(size=2) + 
        ylab("partial ecological distance") + 
        xlab("Geographic Distance") + 
        ylim(0, y_max) + 
        theme(text=element_text(size=20)) + 
        theme_xaringan(background_color = "#1F4257") +
        scale_xaringan_color_discrete() 

p2 <- ggplot(data = splineDat, 
             aes(x = x_season, 
                 y = y_season)
             ) + 
        geom_line(size=2) + 
        ylab("partial ecological distance") + 
        xlab("Season") + ylim(0, y_max) + 
        theme(text=element_text(size=20)) + 
        theme_xaringan(background_color = "#1F4257") +
        scale_xaringan_color_discrete() 

gdm_plot <- plot_grid(p1, p2, label_size = 12)


