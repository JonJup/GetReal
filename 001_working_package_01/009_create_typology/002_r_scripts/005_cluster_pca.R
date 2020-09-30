# -------------------------------------------------- #
### --- PCA of diatom weighted stream typology --- ### 
# -------------------------------------------------- #

# date: 
# what: 

# - OVERVIEW - #

# ------------ #


# setup -------------------------------------------------------------------
setwd("~/01_Uni/03_GetReal/001_WP_01/009_create_typology/")
pacman::p_load(data.table, magrittr, dplyr, sf, PCAmixdata, 
               RColorBrewer, ggplot2, ggrepel, qualpalr)

# data input  -------------------------------------------------------------
species <- readRDS("003_data_processed/2020-02-14_base_data_no_corr.RDS")
cluster_id <- readRDS("003_data_processed/2020-03-02_typology_15_noxy_nobio_river.RDS")

# alternative input 
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/")
data = readRDS("003_processed_data/005_gdm2020-03-25_species.RDS")
data_old <- readRDS("../../../../001_WP_01/009_create_typology/003_data_processed/2020-02-14_base_data_for_typology.RDS")

data_old <- data_old[-which(duplicated(data_old$WSO1_ID))]

(zero_id <- names(which(colSums(data) == 0)))
data[,(zero_id) := NULL]

data_old <- data_old[,.(WSO1_ID, lgm_name, dominant_geology)]

data2 <- left_join(data, 
                   data_old,
                   by = "WSO1_ID") %>% 
        setDT

data2[,c("x_Coords", "y_Coords") := NULL]
species <- data2
cluster_id <- readRDS("003_processed_data/008_2020-03-25_typology_11_noxy_inv_river.RDS")

# prepare data ------------------------------------------------------------
data2 <- left_join(species, 
          cluster_id,
          by = "WSO1_ID")

setDT(data2)
# plot  -------------------------------------------------------------------
# for normal 
names(data2) <- c("WSO1_ID", "Catchment Area", "Elevation", "Slope Range", 
                  "Precipitation", "Precipitation Range", "Temperature", 
                  "Segment Length", "Distance to Source", "Proportion in Lakes", 
                  "Amplitude of Seasonal Signal", "Mean discharge", "Phase of Seasonal Signal", 
                  "Kurtosis of discharge", "Dominant Geology", "Sinuosity", "cluster", "geom")

x.qunat = scale(data2[,.SD, .SDcols = c(2:14, 16)])
x.qual = data2[,.SD, .SDcols = c(15)]

# for invertebrates 
names(data2) <- c("WSO1_ID", "Catchment Area", "Elevation", "Slope Range", 
                  "Precipitation", "Precipitation Range", "Temperature", 
                  "Segment Length", "Distance to Source", "Proportion in Lakes", 
                  "Amplitude of Seasonal Signal", "Mean discharge", "Phase of Seasonal Signal", 
                  "Kurtosis of discharge", "Sinuosity", "State at Last Glacial Maximum","Dominant Geology", "cluster", "geom")


x.qunat = data2[,.SD, .SDcols = c(2:15)]
x.qual = data2[,.SD, .SDcols = c(16,17)]


# both 
res.pca <- PCAmix(X.quanti = x.qunat, 
                  X.quali = x.qual, 
                  ndim = 5, 
                  rename.level = FALSE,
                  weight.col.quanti = NULL, 
                  weight.col.quali = NULL, 
                  graph = F)

# explained fraction per axis 
round(res.pca$eig[1:5,2],1)[1]

# how important are the variables ? 
res.pca$quanti$contrib.pct %>% 
        apply(1,sum) %>% 
        sort(decreasing = T)
res.pca$quanti$contrib %>% 
        apply(1,sum) %>% 
        sort()


lvl = factor(data2$cluster)


## -- color palette -- ## 
n <- 15           
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
## -- -- ## 

## -- prepare data sets for PCA -- ## 
# quantitative varibles 
ggquant = as.data.frame(res.pca$quanti$coord)
names(ggquant) = paste0("dim",1:5)
# individual points (catchments)
ggtest = res.pca$ind$coord %>% 
        as.data.frame
names(ggtest) = paste0("dim",1:5)
# add cluser column
ggtest %<>% 
        tibble::add_column(cluster = lvl)
# find medioid coordinates 
ggtest %>% 
        group_by(cluster) %>% 
        summarise_all(mean) -> medioids
ggtest %>% 
        group_by(cluster) %>% 
        summarise_all(sd) -> medioid_sd
# quantitative variables
ggqual = as.data.frame(res.pca$categ.coord)
names(ggqual) = paste0("dim",1:5)

## -- -- ## 

## -- some statistics -- ## 

ggtest %>% 
        group_by(cluster) %>% 
        count() %>% 
        ggplot(aes(x = cluster, y = 1:11, size = n/1000)) + 
        geom_point()

## ----------------------- ##
## -- how many axes? -- ## 

#- scree plot 
plot(res.pca$eig[,1], type = "b")

#- sum criterion 
alpha <- 0.7
res.pca$eig[,1 ] > alpha

## -- -- ## 
# sizes 
ggtest %>% 
        group_by(cluster) %>% 
        count %>% .[,2] %>% 
        pull() -> size_vec

# take a sample of the rows 
plotsample = sample(nrow(ggtest), 10000)

## -- plots -- ## 

#- dimemsions 1 and 2 

ggtest[plotsample,] %>% 
        ggplot(aes(x = dim1, y = dim2)) + 
        geom_point(aes(color = cluster), alpha = 0.2)

mult <- 4

ggtest %>%
        ggplot(aes(x = dim1, y = dim2)) +
        #geom_point(aes(color = cluster), alpha = 0.2) +
        #geom_label_repel(data = medioids,
        #aes(label = cluster, col = cluster)) +
        # geom_errorbarh(aes(xmin = dim1 - medioid_sd$dim1,
        #                                     xmax = dim1 + medioid_sd$dim1,
        #                                     col = cluster), size = 2) +
        # geom_errorbar(aes(ymin = dim2 - medioid_sd$dim2,
        #                                    ymax = dim2 + medioid_sd$dim2,
        #                                    col = cluster), size = 2) +
        geom_segment(
                data = ggquant,
                x = 0,
                y = 0,
                aes(xend = (dim1 * mult),  yend = (dim2 * mult)),
                arrow = arrow(length = unit(0.1, "inches")),
                size = 1
        ) +
        geom_label_repel(data = ggquant,
                         label = row.names(ggquant),
                         aes(x = dim1 * mult, y = dim2 * mult)) +
        geom_point(
                data = ggqual,
                aes(x = (dim1 * mult), y = (dim2 * mult)),
                shape = 23,
                size = 3,
                fill = "black"
        ) +
        geom_label_repel(data = ggqual,
                         aes(x = dim1 * mult, y = dim2 * mult),
                         label = rownames(ggqual)) +
        theme_minimal() +
        ggtitle("PCA of weighted Stream Typology - Variables") +
        xlab(paste("dim1;", round(res.pca$eig[1:5, 2], 1)[1], "% explained")) +
        ylab(paste("dim2;", round(res.pca$eig[1:5, 2], 1)[2], "% explained"))
# xlim(c(-10,11)) +
# ylim(c(-3, 7))

## -- plot of segements 

ggtest %>%
        ggplot(aes(x = dim1, y = dim2)) +
        geom_point(aes(color = cluster), alpha = 0.2) +
        geom_label_repel(data = medioids,
        aes(label = cluster, col = cluster)) +
        # geom_errorbarh(aes(xmin = dim1 - medioid_sd$dim1,
        #                                     xmax = dim1 + medioid_sd$dim1,
        #                                     col = cluster), size = 2) +
        # geom_errorbar(aes(ymin = dim2 - medioid_sd$dim2,
        #                                    ymax = dim2 + medioid_sd$dim2,
        #                                    col = cluster), size = 2) +
        # geom_segment(
        #         data = ggquant,
        #         x = 0,
        #         y = 0,
        #         aes(xend = (dim1 * 5),  yend = (dim2 * 5)),
        #         arrow = arrow(length = unit(0.1, "inches")),
        #         size = 1
        # ) +
        # geom_label_repel(data = ggquant,
        #                  label = row.names(ggquant),
        #                  aes(x = dim1 * 5, y = dim2 * 5)) +
        # geom_point(
        #         data = ggqual,
        #         aes(x = (dim1 * 5), y = (dim2 * 5)),
        #         shape = 23,
        #         size = 3,
        #         fill = "black"
        # ) +
        # geom_label_repel(data = ggqual,
        #                  aes(x = dim1 * 5, y = dim2 * 5),
        #                  label = rownames(ggqual)) +
        theme_minimal() +
        ggtitle("PCA of weighted Stream Typology - Observations") +
        xlab(paste("dim1;", round(res.pca$eig[1:5, 2], 1)[1], "% explained")) +
        ylab(paste("dim2;", round(res.pca$eig[1:5, 2], 1)[2], "% explained")) + 
        theme(legend.position = "none") 
# xlim(c(-10,11)) +
# ylim(c(-3, 7))


# --- dimemsions 1 and 3

medioids %>% 
        ggplot(aes(x = dim1, y = dim3)) + 
        geom_label_repel(aes(label = cluster, col = cluster)) + 
        # geom_errorbarh(aes(xmin = dim1 - medioid_sd$dim1,
        #                    xmax = dim1 + medioid_sd$dim1,
        #                    col = cluster), size = 2) + 
        # geom_errorbar(aes(ymin = dim3 - medioid_sd$dim3,
        #                   ymax = dim3 + medioid_sd$dim3,
        #                   col = cluster), size = 2) + 
        geom_segment(data = ggquant, 
                     x = 0, y = 0, 
                     aes(xend = (dim1 * 2),  yend = (dim3 * 2)), 
                     arrow = arrow(length = unit(0.1, "inches")),
                     size = 1) + 
        geom_label_repel(data = ggquant, label = row.names(ggquant),
                         aes(x = dim1 * 2, y = dim3 * 2)) + 
        geom_point(data = ggqual,
                   aes(x = (dim1*2), y = (dim3*2)),
                   shape = 23, size = 3, fill = "black") + 
        geom_label_repel(data = ggqual, 
                         aes(x = dim1 * 2, y = dim3 * 2),
                         label = rownames(ggqual))

# --- dimensions 2 and 3 

medioids %>% 
        ggplot(aes(x = dim3, y = dim2)) + 
        geom_label_repel(aes(label = cluster, col = cluster)) + 
        # geom_errorbarh(aes(xmin = dim3 - medioid_sd$dim3,
        #                    xmax = dim3 + medioid_sd$dim3,
        #                    col = cluster), size = 2) + 
        # geom_errorbar(aes(ymin = dim2 - medioid_sd$dim2,
        #                   ymax = dim2 + medioid_sd$dim2,
        #                   col = cluster), size = 2) + 
        geom_segment(data = ggquant, 
                     x = 0, y = 0, 
                     aes(xend = (dim3 * 2),  yend = (dim2 * 2)), 
                     arrow = arrow(length = unit(0.1, "inches")),
                     size = 1) + 
        geom_label_repel(data = ggquant, label = row.names(ggquant),
                         aes(x = dim3 * 2, y = dim2 * 2)) + 
        geom_point(data = ggqual,
                   aes(x = (dim3*2), y = (dim2*2)),
                   shape = 23, size = 3, fill = "black") + 
        geom_label_repel(data = ggqual, 
                         aes(x = dim3 * 2, y = dim2 * 2),
                         label = rownames(ggqual))
