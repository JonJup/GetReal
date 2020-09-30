#-----------------------------#
### --- Create Clusters --- ###
   ### --- weighted --- ### 
#-----------------------------#



## -- OVERVIEW -- ## 
# 01. Setup
# 02. Remove correlated variables
## -------------- ##


# 01. Setup -------------------------------------------------------------------


pacman::p_load(data.table,
               dplyr,
               treeClust)


# also required: here, beepr, tibble
setwd(here::here())
# or 
setwd("~/01_Uni/GetReal/01_WP_01/06_create_typology/")

# load my cluster function that enable cost in trees 
source("02_r_scripts/my_tree_clust.R")
source("02_r_scripts/tree_clust_r_part2.R")

# Input 
data = readRDS("03_data_processed/base_data_for_typology_SC4.RDS")
weights = readRDS("../../../03_GetReal/02_WP_02/Community Data/100_Combined/Diatoms/random forest/200204_variable_importance.RDS")


# 02. Remove correlated variables -----------------------------------------

data$size_class = factor(data$size_class)

# standardize numerical variables 
data[, c("Area", "Elevation", "Slope", "Temperature", "Y_Coords","X_Coords", "prop_Lakes", "amplitude","ar1_correlation", "phase", "tau2","tau3","tau4","sinuosity") := 
             .(scale(Area), scale(Elevation), scale(Slope), scale(Temperature), scale(Y_Coords), scale(X_Coords), scale(prop_Lakes),
               scale(amplitude), scale(ar1_correlation), scale(phase), scale(tau2), scale(tau3), scale(tau4),  scale(sinuosity))]

data = as.data.frame(data)
# Tree based Clustering - Optimal number of Clusters  -----------------------------------------------------------

## --  Size Class <= 4 -- ##
sil.width1 = c()
sil.width2 = c()
sil.width3 = c()

# 1/var.imp

for (i in 1:29) {

        tc = mytreeClust(dfx = data,
                       d.num = 4, 
                       col.range = 2:18,
                       final.algorithm = "clara",
                       k = i + 1,
                       verbose = T,
                       control = treeClust.control("parallelnodes" = 1),
                       weights = 1/weights
                       )

        sil.width1[i] = tc$final.clust$silinfo$avg.width
        print(paste(i, "@", Sys.time()))
        
};beepr::beep()

## -- plot -- ## 
names(sil.width1) <- paste(1:length(sil.width1))
top_value <- sort(sil.width1, decreasing = T) %>%  .[1]
ten_percent <- sil.width1[sil.width1 >= top_value - top_value * 0.05]
tt_index = which(sil.width1 %in% ten_percent)
plot(sil.width1, type = "b", xlab = "Cluster", ylab = "Average Silhouette width", main = "Size class <= 4")
points(x = tt_index, y = sil.width1[tt_index], col = "red", pch = 20)
text(x = tt_index, y = sil.width1[tt_index] - sd(sil.width1), paste(tt_index))

# weight option 2
for (i in 1:29) {
        
        tc = mytreeClust(dfx = data,
                         d.num = 4, 
                         col.range = 2:18,
                         final.algorithm = "clara",
                         k = i + 1,
                         verbose = T,
                         control = treeClust.control("parallelnodes" = 1),
                         weights = 1/(weights^2)
        )
        
        sil.width2[i] = tc$final.clust$silinfo$avg.width
        print(paste(i, "@", Sys.time()))
        
};beepr::beep()

## -- plot -- ## 
names(sil.width2) <- paste(1:length(sil.width2))
top_value <- sort(sil.width2, decreasing = T) %>%  .[1]
ten_percent <- sil.width2[sil.width2 >= top_value - top_value * 0.1]
tt_index = which(sil.width2 %in% ten_percent)
plot(sil.width2, type = "b", xlab = "Cluster", ylab = "Average Silhouette width", main = "Size class <= 4")
points(x = tt_index, y = sil.width2[tt_index], col = "red", pch = 20)
text(x = tt_index, y = sil.width2[tt_index] - sd(sil.width2), paste(tt_index))


# weight option 3
for (i in 1:29) {
        
        tc = mytreeClust(dfx = data,
                         d.num = 4, 
                         col.range = 2:18,
                         final.algorithm = "clara",
                         k = i + 1,
                         verbose = T,
                         control = treeClust.control("parallelnodes" = 1),
                         weights = 1/(weights/2)
        )
        
        sil.width3[i] = tc$final.clust$silinfo$avg.width
        print(paste(i, "@", Sys.time()))
        
};beepr::beep()

## -- plot -- ## 
names(sil.width3) <- paste(1:length(sil.width3))
top_value <- sort(sil.width3, decreasing = T) %>%  .[1]
ten_percent <- sil.width3[sil.width3 >= top_value - top_value * 0.1]
tt_index = which(sil.width3 %in% ten_percent)
plot(sil.width3, type = "b", xlab = "Cluster", ylab = "Average Silhouette width", main = "Size class <= 4")
points(x = tt_index, y = sil.width3[tt_index], col = "red", pch = 20)
text(x = tt_index, y = sil.width3[tt_index] - sd(sil.width3), paste(tt_index))

## -- save plot -- ## 
# 1. Open jpeg file
# pdf("04_plots/200114_silhouette_sc4_withxy.pdf")
# # 2. Create the plot
# plot(sil.width, type = "b", xlab = "Cluster", ylab = "Average Silhouette width", main = "Size class <= 4")
# points(x = tt_index, y = sil.width[tt_index], col = "red", pch = 20)
# text(x = tt_index, y = sil.width[tt_index] - sd(sil.width), paste(tt_index))
# # 3. Close the file
# dev.off()

# Optimal clusters --------------------------------------------------------

# create cluster with optial number of clusters 
tc_sc4_12_after_weight = mytreeClust(dfx = data,
                                 d.num = 4, 
                                 col.range = 2:18,
                                 final.algorithm = "clara",
                                  k = 12,
                                  verbose = T,
                                  control = treeClust.control("parallelnodes" = 1),
                                  weights = 1/weights
)
# weight 2 
tc_sc4_12_w2 = mytreeClust(dfx = data,
                           d.num = 4, 
                           col.range = 2:18,
                           final.algorithm = "clara",
                           k = 12,
                           verbose = T,
                           control = treeClust.control("parallelnodes" = 1),
                           weights = 1/(weights^2)
)
# weight 3
tc_sc4_w3 = mytreeClust(dfx = data,
                 d.num = 4, 
                 col.range = 2:18,
                 final.algorithm = "clara",
                 k = 12,
                 verbose = T,
                 control = treeClust.control("parallelnodes" = 1),
                 weights = 1/(weights/2)
)


# check cluster id values 
unique(tc_sc4_12_after_weight$final.clust$clustering)
unique(tc_sc4_12_w2$final.clust$clustering)
unique(tc_sc4_w3$final.clust$clustering)

# check distribution between groups 
plot(table(tc_sc4_12_after_weight$final.clust$clustering))
plot(table(tc_sc4_12_w2$final.clust$clustering))
plot(table(tc_sc4_w3$final.clust$clustering))

# Save to File ------------------------------------------------------------

# create save name with date       
(save_name = paste0(Sys.Date(), "_tc_sc4_12_after_weight.RDS"))
(save_name2 = paste0(Sys.Date(), "_tc_sc4_12_w2.RDS"))
(save_name3 = paste0(Sys.Date(), "_tc_sc4_12_w3.RDS"))

# save to file 
data %>% 
        bind_cols(cluster = tc_sc4_12_after_weight$final.clust$clustering)  %>% 
        select(WSO1_ID, cluster) %>% 
        saveRDS(paste0("03_data_processed/",save_name))
data %>% 
        bind_cols(cluster = tc_sc4_12_w2$final.clust$clustering)  %>% 
        select(WSO1_ID, cluster) %>% 
        saveRDS(paste0("03_data_processed/",save_name2))
data %>% 
        bind_cols(cluster = tc_sc4_w3$final.clust$clustering)  %>% 
        select(WSO1_ID, cluster) %>% 
        saveRDS(paste0("03_data_processed/",save_name3))
