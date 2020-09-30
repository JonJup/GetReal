#-----------------------------#
### --- Create Clusters --- ###
#-----------------------------#

# date: 09.09.19 - 23.09.19

# Here I use the base data sets created in "01_compile_data_sets.R" to build clusters. 

## -- OVERVIEW -- ## 
# 01. Setup
# 02. Remove correlated variables
## -------------- ##


# 01. Setup -------------------------------------------------------------------


pacman::p_load(data.table,
               dplyr,
               magrittr,
               treeClust,
               PCAmixdata,
               ggplot2,
               factoextra,
               FactoMineR,
               PCAmixdata,
               RColorBrewer,
               ggrepel, # for repelling text labels
               ggforce,  # for creating a hull around observations  
                metR        )

# also required: here, beepr, tibble
setwd(here::here())
# or 
setwd("~/01_Uni/GetReal/01_WP_01/06_create_typology/")

# Input 
data = readRDS("03_data_processed/base_data_for_typology_SC4.RDS")

data#remove x and y for test 
#data = data[,c("Y_Coords", "X_Coords") := .(NULL, NULL)]

# 02. Remove correlated variables -----------------------------------------

# based on PCA loadings: Elevation > Slope 
data[,Slope := NULL]
# based on PCA loadings: tau2 > ar1 
data[,ar1_correlation := NULL]
# no removal just turn size class into factor 
data$size_class = factor(data$size_class)
# Tree based Clustering - Optimal number of Clusters  -----------------------------------------------------------



## --  Size Class <= 4 -- ##
sil.width = c()
for (i in 1:30) {
        tc = treeClust(dfx = data,
                       d.num = 4, 
                       col.range = 2:16,
                       final.algorithm = "clara",
                       k = i + 1,
                       verbose = T,
                       control = treeClust.control("parallelnodes" = 10))
        sil.width[i] = tc$final.clust$silinfo$avg.width
        print(i)
        
};beepr::beep()

## -- plot -- ## 
sort(sil.width, decreasing = T) %>% 
        .[1] -> top_three

tt_index = which(sil.width %in% top_three)
plot(sil.width, type = "b", xlab = "Cluster", ylab = "Average Silhouette width", main = "Size class <= 4")
points(x = tt_index, y = sil.width[tt_index], col = "red", pch = 20)
text(x = tt_index, y = sil.width[tt_index] - sd(sil.width), paste(tt_index))

## -- save plot -- ## 
# 1. Open jpeg file
pdf("04_plots/200114_silhouette_sc4_withxy.pdf")
# 2. Create the plot
plot(sil.width, type = "b", xlab = "Cluster", ylab = "Average Silhouette width", main = "Size class <= 4")
points(x = tt_index, y = sil.width[tt_index], col = "red", pch = 20)
text(x = tt_index, y = sil.width[tt_index] - sd(sil.width), paste(tt_index))
# 3. Close the file
dev.off()



# Optimal clusters --------------------------------------------------------

tc_sc4_19 = treeClust(dfx = data,
                     d.num = 4, 
                     col.range = 2:16,
                     final.algorithm = "clara",
                     k = 19,
                     verbose = T,
                     control = treeClust.control("parallelnodes" = 10))
unique(tc_sc4_19$final.clust$clustering)


save_name = paste0(Sys.Date(), "_tc_sc4_xy_19.RDS")
data %>% 
        bind_cols(cluster = tc_sc4_19$final.clust$clustering)  %>% 
        select(WSO1_ID, cluster) %>% 
        saveRDS(paste0("03_data_processed/",save_name))

pca.data = data
        
pca.data[,cluster := tc_sc4_25$final.clust$clustering]

# PCA ---------------------------------------------------------------------

x.qunat = scale(pca.data[,.SD, .SDcols = c(2:5, 7:11,13)]) 
x.qual = pca.data[,.SD, .SDcols = c(6,12,14)]

res.pca <- PCAmix(X.quanti = x.qunat, 
                  X.quali = x.qual, 
                  ndim = 5, 
                  rename.level = FALSE,
                  weight.col.quanti = NULL, 
                  weight.col.quali = NULL, 
                  graph = F)

# explained fraction per axis 
round(res.pca$eig[1:5,2],1)

# how important are the variables ? 
res.pca$quanti$contrib.pct %>% 
        apply(1,sum) %>% 
        sort(decreasing = T)
res.pca$quanti$contrib %>% 
        apply(1,sum) %>% 
        sort()


lvl = factor(pca.data$cluster)


## -- color palette -- ## 
n <- 60           
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
        ggplot(aes(x = cluster, y = 1:25, size = n/1000)) + 
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
        ggtest %>% group_by(cluster) %>% count %>% .[,2] %>% pull() -> size_vec
        # take a sample of the rows 
        plotsample = sample(nrow(ggtest), 1000)
## -- plots -- ## 

        #- dimemsions 1 and 2 
        ggtest %>%
                ggplot(aes(x = dim1, y = dim2)) + 
                # color scale 
                scale_colour_manual(values = col_vector[-c(4,6,16)]) +
                scale_fill_manual(values = col_vector[-c(4,6,16)]) +
                
                # individual catchments 
                geom_point(aes(fill = cluster, col = cluster), alpha = 0.01) +
                # # cluster medioids
                geom_errorbarh(data = medioids, aes(xmin = dim1 - medioid_sd$dim1,
                                                    xmax = dim1 + medioid_sd$dim1,
                                                    col = cluster),
                               size = 2) + 
                geom_errorbar(data = medioids, aes(ymin = dim2 - medioid_sd$dim2,
                                                   ymax = dim2 + medioid_sd$dim2,
                                                   col = cluster),
                              size = 2
                ) +
                geom_point(data = medioids, aes(x = dim1, y = dim2, fill = cluster),
                           shape = 21, size = 5) + 
                # # quantitative variables 
                geom_segment(data = ggquant, 
                             x = 0, y = 0, 
                             aes(xend = (dim1 * 2),  yend = (dim2 * 2)), 
                             arrow = arrow(length = unit(0.1, "inches")),
                             size = 1) +
                

                # # qualitative variables
                geom_point(data = ggqual,
                           aes(x = (dim1*2),
                               y = (dim2*2)),
                           shape = 23,
                           size = 3,
                           fill = "black") +

                geom_text(data = ggqual,
                          aes(x = ((dim1 +
                                            c(-.2,.3,.07,-.1,-.15,0,-.15,.15,.05,.05,.07,-.08)) * 2),

                              y = ((dim2 +
                                            c(0,.07,.15,-.17,0,-.1,0,0,-.08,.05,0,-.07)) * 2)
                                      ),

                          label = row.names(ggqual)) +

                # # # axis labels
                xlab(paste("Dim 1 (", round(res_pca$eig[1,2],1), "%)")) +
                ylab(paste("Dim 2 (", round(res_pca$eig[2,2],1), "%)")) +
                # # extend
                #ylim(c(-8.9, 5)) +
                #xlim(c(-5.8, 4.4)) +
                # theme
                theme_classic()

        
         #dim 1 3 -- needs update 
        ggtest %>%         
                ggplot(aes(x = dim1 + 4, y = dim3 + 1)) + 
                scale_colour_manual(values = col_vector) +
                scale_fill_manual(values = col_vector) +
                geom_point(aes(fill = lvl, col = lvl), shape = 21, alpha = 0.2) +
                geom_segment(data = ggquant, x = 0, y = 0, aes(xend = dim1 * 3, yend = dim3 * 3)) +
                geom_text_repel(data = ggquant, aes( x = dim1 * 4, y = dim3 * 4), label = row.names(ggquant)) + 
                geom_point(data = ggqual, aes(x = dim1 * 3, y = dim3 * 3)) + 
                geom_text_repel(data = ggqual, aes(x = dim1 * 3, y = dim3 * 3), label = row.names(ggqual), position = "dodge")
        #dim 2 3 -- needs update
        ggtest %>%         
                ggplot(aes(x = dim2 + 4, y = dim3 + 1)) + 
                scale_colour_manual(values = col_vector) +
                scale_fill_manual(values = col_vector) +
                geom_point(aes(fill = lvl, col = lvl), shape = 21, alpha = 0.2) +
                geom_segment(data = ggquant, x = 0, y = 0, aes(xend = dim2 * 3, yend = dim3 * 3)) +
                geom_text(data = ggquant, aes( x = dim2 * 4, y = dim3 * 4), label = row.names(ggquant)) + 
                geom_point(data = ggqual, aes(x = dim2 * 3, y = dim3 * 3)) + 
                geom_text(data = ggqual, aes(x = dim2 * 3, y = dim3 * 3), label = row.names(ggqual), position = "dodge")

## -- -- ##
# old ---------------------------------------------------------------------


#get_eig(res.pca)
#fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
#var <- get_pca_var(res.pca)
# fviz_pca_var(res.pca, col.var = "contrib",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE # Avoid text overlapping
# )
fviz_pca_biplot(
        res.pca,
        label = "none",
        habillage = cluster
        # col.var = "contrib",
        # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
        #      repel = TRUE, # Avoid text overlapping,
        #      label = "var",
        #      habillage = data4$cluster[1:100],
        #      palette = c("#00AFBB", "#E7B800", "#FC4E07")
)

fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             habillage = data4$cluster[1:100],
             label = "none",
             addEllipses = T
)

## old stuff mostly correlation analysis 

# glimpse(data3)
# gower_dist <- daisy(data3[1:10000,], metric = "gower")
# gower_mat <- as.matrix(gower_dist)
# sil_width <- c(NA)
# for (i in 2:8) {  
#         pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
#         sil_width[i] <- pam_fit$silinfo$avg.width  
# }
# plot(1:8, sil_width,
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(1:8, sil_width)
# 
# 
# 
# 
# 
# cor_mat = mixedCor(data = data2, 
#          c = c(3:10,12:18,20),
#          p = c(19),
#          use = "complete.obs",
#          method = "spearman")
# cor_mat = mixedCor(data = data3, 
#                    c = c(3:10,12:15,17),
#                    p = c(16),
#                    use = "complete.obs",
#                    method = "spearman")
# 
# cormat = cor_mat[["rho"]]



# reorder_cormat <- function(cormat) {
#         # Use correlation between variables as distance
#         dd <- as.dist((1 - cormat)/2)
#         hc <- hclust(dd)
#         cormat <- cormat[hc$order, hc$order]
# }
# cormat <- reorder_cormat(cormat)
# cormat[upper.tri(cormat)] <- NA
# cormat[abs(cormat) < 0.7] <- NA
# cormat[cormat == 1] <- NA 
# # Melt the correlation matrix
# melted_cormat <- melt(cormat, na.rm = TRUE)
# # Create a ggheatmap
# ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
#         geom_tile(color = "white") +
#         scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                              midpoint = 0, limit = c(-1,1), space = "Lab", 
#                              name = "Pearson\nCorrelation") +
#         theme_minimal() + # minimal theme
#         theme(axis.text.x = element_text(angle = 45, vjust = 1, 
#                                          size = 12, hjust = 1)) +
#         coord_fixed()
# # Print the heatmap
# print(ggheatmap)


# removal of statistics
