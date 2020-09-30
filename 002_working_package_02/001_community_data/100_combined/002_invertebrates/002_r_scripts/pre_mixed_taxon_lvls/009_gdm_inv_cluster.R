#-----------------------------#
### --- Create Clusters --- ###
#-----------------------------#

# date: 25.02.20

## -- OVERVIEW -- ## 
# 01. Setup
# 02. Remove correlated variables
## -------------- ##


# 01. Setup -------------------------------------------------------------------


pacman::p_load(data.table,
               dplyr,
               magrittr,
               treeClust)

# also required: here, beepr, tibble
setwd(here::here())
# or 
setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/")

# Input 
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
# Tree based Clustering - Optimal number of Clusters  -----------------------------------------------------------

sil.width = c()
for (i in 2:30) {
        tc = treeClust(dfx = data2,
                       d.num = 4, 
                       col.range = 2:17,
                       final.algorithm = "clara",
                       k = i ,
                       verbose = T,
                       control = treeClust.control("parallelnodes" = 12))
        
        sil.width[i-1] = tc$final.clust$silinfo$avg.width
        
        print(paste(i, "@", Sys.time()))
        
};beepr::beep()

## -- plot -- ## 
sort(sil.width, decreasing = T) %>% 
        .[1] -> top_three
tt_index = which(sil.width %in% top_three)
plot(sil.width, type = "b", xlab = "Cluster", ylab = "Average Silhouette width", main = "Size class <= 4")
points(x = tt_index, y = sil.width[tt_index], col = "red", pch = 20)
text(x = tt_index, y = sil.width[tt_index] - sd(sil.width), paste(tt_index))

## -- 5 percent threshold 
names(sil.width) <- paste(1:length(sil.width))
top_value <- sort(sil.width, decreasing = T) %>%  .[1]
five_percent <- sil.width[sil.width >= top_value - top_value * 0.05]
ten_percent <- sil.width[sil.width >= top_value - top_value * 0.10]
tt_index_5 = which(sil.width %in% five_percent)
tt_index_10 = which(sil.width %in% ten_percent)
plot(sil.width, type = "b", xlab = "Cluster", ylab = "Average Silhouette width")
points(x = tt_index_10, y = sil.width[tt_index_10], col = "orange", pch = 20)
points(x = tt_index_5, y = sil.width[tt_index_5], col = "red", pch = 20)
text(x = tt_index_10, y = sil.width[tt_index_10] - sd(sil.width), paste(tt_index_10), col = "orange")
text(x = tt_index_5, y = sil.width[tt_index_5] - sd(sil.width), paste(tt_index_5), col = "red")

## -- save plot -- ## 
# 1. Open jpeg file
pdf(paste0("004_plots/", Sys.Date(), "_silhouette_gdm_mzb_noxy.pdf"))
# 2. Create the plot
names(sil.width) <- paste(1:length(sil.width))
top_value <- sort(sil.width, decreasing = T) %>%  .[1]
five_percent <- sil.width[sil.width >= top_value - top_value * 0.05]
ten_percent <- sil.width[sil.width >= top_value - top_value * 0.10]
tt_index_5 = which(sil.width %in% five_percent)
tt_index_10 = which(sil.width %in% ten_percent)
plot(sil.width, type = "b", xlab = "Cluster", ylab = "Average Silhouette width")
points(x = tt_index_10, y = sil.width[tt_index_10], col = "orange", pch = 20)
points(x = tt_index_5, y = sil.width[tt_index_5], col = "red", pch = 20)
text(x = tt_index_10, y = sil.width[tt_index_10] - sd(sil.width), paste(tt_index_10), col = "orange")
text(x = tt_index_5, y = sil.width[tt_index_5] - sd(sil.width), paste(tt_index_5), col = "red")
# 3. Close the file
dev.off()



# Optimal clusters --------------------------------------------------------

typologie_11 = treeClust(dfx = data2,
                     d.num = 4, 
                     col.range = 2:17,
                     final.algorithm = "clara",
                     k = 11,
                     verbose = T,
                     control = treeClust.control("parallelnodes" = 12));beepr::beep()

typologie_26 = treeClust(dfx = data,
                     d.num = 4, 
                     col.range = 2:16,
                     final.algorithm = "clara",
                     k = 26,
                     verbose = T,
                     control = treeClust.control("parallelnodes" = 10))



# qualtiy checks 
unique(typologie_11$final.clust$clustering)


table(typologie_11$final.clust$clustering) %>% plot

# save to file ------------------------------------------------------------

save_name_1 = paste0("007_",Sys.Date(), "_typology11_gdm_inv_noxy.RDS")
save_name_2 = paste0(Sys.Date(), "_typology26_gdm_inv_noxy.RDS")



data %>% 
        bind_cols(cluster = typologie_11$final.clust$clustering)  %>% 
        dplyr::select(WSO1_ID, cluster) %>% 
        saveRDS(paste0("003_processed_data/",save_name_1))
data %>% 
        bind_cols(cluster = typologie_26$final.clust$clustering)  %>% 
        dplyr::select(WSO1_ID, cluster) %>% 
        saveRDS(paste0("003_processed_data/",save_name_2))

