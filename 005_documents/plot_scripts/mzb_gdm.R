## -- GetREAL
## -- Macroinvertebrates 
## -- GDM plot for documents 


# setup --------------------------------------------------------------------
pacman::p_load(cowplot, dplyr, gdm, ggplot2, here)
dir_da = here("002_working_package_02/003_seasonality/003_results/invertebrates/005_gdms/")

# data --------------------------------------------------------------------
gdm_data = readRDS(file.path(dir_da, "2020-11-13_gdm_rt4_5.RDS"))
splineDat  <- isplineExtract(gdm_data)
splineDat_x <- splineDat$x %>% as.data.frame
splineDat_y <- splineDat$y %>% as.data.frame
names(splineDat_x) <- paste0("x_", names(splineDat_x))
names(splineDat_y) <- paste0("y_", names(splineDat_y))
splineDat <- cbind(splineDat_x, splineDat_y)

max_geo    <- max(splineDat$y_Geographic)
max_season <- max(splineDat$y_season)

y_max <- max(max_geo, max_season)


# plot --------------------------------------------------------------------
p1 <-
        ggplot(data = splineDat, aes(x = x_Geographic, y = y_Geographic)) +
        geom_line() +
        ylab("partial ecological distance") +
        xlab("Geographic Distance") +
        ylim(0, y_max) + theme_minimal_hgrid() +
        ggtitle(paste0("Macroinvertebrates")) +
        labs(subtitle = "RT 4_5")
p2 <- ggplot(data = splineDat, aes(x = x_season, y = y_season)) +
        geom_line() +
        ylab("partial ecological distance") +
        xlab("Season") +
        ylim(0, y_max) +
        theme_minimal_hgrid()

p3 <- plot_grid(p1, p2, label_size = 12)

# clean -------------------------------------------------------------------
rm(gdm_data, dir_da, splineDat, splineDat_x,splineDat_y,max_geo,max_season,y_max, p1,p2);gc()


