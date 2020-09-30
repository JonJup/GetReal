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
               treeClust)

# also required: here, beepr, tibble
setwd(here::here())
# or 
setwd("~/01_Uni/03_GetReal/001_WP_01/009_create_typology/")

# Input 
data = readRDS("003_data_processed/2020-02-14_base_data_no_corr.RDS")

data[, c("area", "elevation", "slope_range", "precipitation", "precipitation_range",
         "temperature", "Segement_length", "distance_to_source","prop_Lakes", 
         "amplitude", "discharge", "phase", "tau4", "sinuosity") := 
             .(scale(area), scale(elevation), scale(slope_range), scale(precipitation), scale(precipitation_range),
               scale(temperature), scale(Segement_length), scale(distance_to_source), scale(prop_Lakes), 
               scale(amplitude), scale(discharge), scale(phase), scale(tau4), 
               scale(sinuosity))]

# Tree based Clustering - Optimal number of Clusters  -----------------------------------------------------------

sil.width = c()
for (i in 2:30) {
        tc = treeClust(dfx = data,
                       d.num = 4, 
                       col.range = 2:16,
                       final.algorithm = "clara",
                       k = i ,
                       verbose = T,
                       control = treeClust.control("parallelnodes" = 5))
        
        sil.width[i] = tc$final.clust$silinfo$avg.width
        
        print(paste(i, "@", Sys.time()))
        
};beepr::beep()

## -- plot -- ## 
names(sil.width) <- paste(1:length(sil.width))
top_value <- sort(sil.width, decreasing = T) %>%  .[1]
five_percent <- sil.width[sil.width >= top_value - top_value * 0.05]
ten_percent <- sil.width[sil.width >= top_value - top_value * 0.10]
tt_index_5 = which(sil.width %in% five_percent)
tt_index_10 = which(sil.width %in% ten_percent)
plot(sil.width, type = "b", xlab = "Cluster", ylab = "Average Silhouette width")
points(x = tt_index_10, y = sil.width[tt_index_10], col = "orange", pch = 20)
points(x = tt_index_5, y = sil.width[tt_index_5], col = "red", pch = 20)
text(x = tt_index_10, y = sil.width[tt_index_10] - 0.05, paste(tt_index_10), col = "orange")
text(x = tt_index_5, y = sil.width[tt_index_5] - 0.05, paste(tt_index_5), col = "red")


## -- save plot -- ## 
# 1. Open jpeg file
pdf(paste0("004_plots/",
           Sys.Date(),
           "_Silhouetten.pdf")
    )
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
text(x = tt_index_10, y = sil.width[tt_index_10] - 0.05, paste(tt_index_10), col = "orange")
text(x = tt_index_5, y = sil.width[tt_index_5] - 0.05, paste(tt_index_5), col = "red")
# 3. Close the file
dev.off()

# Optimal clusters --------------------------------------------------------

typologie = treeClust(dfx = data,
                     d.num = 4, 
                     col.range = 2:16,
                     final.algorithm = "clara",
                     k = 15,
                     verbose = T,
                     control = treeClust.control("parallelnodes" = 5))


# qualtiy checks 
unique(typologie$final.clust$clustering)

table(typologie$final.clust$clustering) %>% plot



save_name_15 = paste0(Sys.Date(), "_typology_noxy_nobio.RDS")



data %>% 
        bind_cols(cluster = typologie$final.clust$clustering)  %>% 
        dplyr::select(WSO1_ID, cluster) %>% 
        saveRDS(paste0("003_data_processed/",save_name_15))

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
                geom_point(aes(fill = cluster, col = cluster), alpha = 0.01) 
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
                # [1] "Area"        "Elevation"   "Temperature" "Y_Coords"    "X_Coords"    "prop_Lakes"  "amplitude"  
                # [8] "phase"       "tau2"        "tau3"        "tau4"        "sinuosity" 
                annotate("rect", 
                         xmin = ((
                                 ggquant$dim1 +
                                         #1   #2  #3    #4  #5     #6   #7   #8   #9  #10 #11  #12     
                                         c(-.14, .15, 0, -.09, .1,  0.05,-.15, .12, .1, .2,-.1,-0.08)) * 2) +
                                 #1    #2     #3   #4   #5  #6   #7   #8   #9  #10  #11  #12      
                                 c(-.2, -.25, -.35, -.3, -.3, -.3, -.3, -.2, -.15, -.15, -.15, -.3),
                         xmax = ((
                                 ggquant$dim1 + 
                                         #1   #2  #3   #4  #5    #6   #7   #8   #9  #10 #11  #12
                                         c(-.14, .15, 0, -.09, .1, 0.05, -.15, .12, .1, .2, -.1, -0.08)) * 2) +
                                 #1    #2    #3   #4   #5  #6   #7   #8   #9  #10  #11  #12                           
                                 c(.2, .25, .35,.3, .3, .3, .3, .2, .2, .15, .15, .3), 
                         ymin = ((
                                 ggquant$dim2 + 
                                         #1   #2    #3   #4    #5    #6   #7   #8   #9  #10  #11  #12    
                                         c(0.01, -.05, -.12, 0.12, .17, 0.15, -.15, .15, -0.05, 0, .1, -0.16)) * 2) - 
                                 #1    #2    #3   #4   #5      #6   #7   #8   #9      #10  #11  #12 
                                 c(.15, .15, .15, .15, .15, .15, .15, .15, .15, .15, .15, .15),
                         ymax = ((
                                 ggquant$dim2 + 
                                         #1   #2     #3   #4   #5     #6   #7   #8   #9   #10 #11    #12    
                                         c(0.01, -.05, -.12, 0.12, .17, 0.15, -.15, .15, -0.05, 0, .1, -0.16)) * 2) -
                                 #1    #2    #3     #4    #5    #6     #7   #8    #9    #10   #11  #12 
                                 c(-.15, -.15, -.15, -.15, -.15, -.15, -.15, -.15, -.15, -.15, -.15, -.15),
                         
                         fill = "white", col = "black", alpha = .7) + 
                
                annotate("text", 
                         
                         x = ((
                                 ggquant$dim1 + 
                                         #1   #2   #3    #4  #5   #6    #7   #8   #9   #10 #11    #12    
                                         c(-.14, .15, 0.0, -.09, .1, 0.05, -.15, .12, .1, .2, -.1, -0.08)) * 2), 
                         
                         y = ((
                                 ggquant$dim2 + 
                                         #1   #2   #3    #4   #5   #6    #7   #8    #9   #10 #11   #12      
                                         c(0.01, -.05, -.12, 0.12, .17, 0.15, -.15, .15, -0.05, 0, .1, -0.16)) * 2), 
                         label =   row.names(ggquant)) + 
                
                
                # # # qualitative variables
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
                xlab(paste("Dim 1 (", round(res.pca$eig[1,2],1), "%)")) +
                ylab(paste("Dim 2 (", round(res.pca$eig[2,2],1), "%)")) +
                # # extend 
                ylim(c(-8.9, 5)) +
                xlim(c(-5.8, 4.4)) +
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
