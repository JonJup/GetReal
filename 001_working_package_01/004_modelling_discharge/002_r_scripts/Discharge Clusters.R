##############################################
##### ---- Clustering flow regimes ----- ##### 
##############################################


# setup -------------------------------------------------------------------
pacman::p_load(data.table, dplyr)

setwd(here::here())
mag7 = readRDS("magnificent7_final.RDS") 

ids = which(mag7 == "Only NAs")
mag7 = mag7[-ids]
mag7 =  rbindlist(mag7)
saveRDS(mag7, "mag7_dt.RDS")
# WSO1ID 440198 is duplicated 
mag7[which(duplicated(mag7$WSO1_ID) == TRUE),]
# both are equal so I can remove one 
mag7[WSO1_ID == 440198]
mag7 = mag7[-which(duplicated(mag7$WSO1_ID) == TRUE),]

# subset to small rivers 
mag7 = mag7[WSO1_ID %in% small_id]

# add WSO1_ID as rowname 
rownames(mag7) = as.character(mag7$WSO1_ID)
mag7 = mag7[,-"WSO1_ID"]

mag72 = scale(mag7)

library(GGally)
class1 = kmeans(mag72, centers = 3, iter.max = 10, nstart = 1)
mag7[, "class" := class1$cluster]

library(cluster)
silhouette(mag7$class)

clus1 = clara(x = mag72, k = 1, metric = "euclidean", stand = T, 
              samples = 1000, pamLike = T)
clus2 = clara(x = mag72, k = 2, metric = "euclidean", stand = T, 
      samples = 1000, pamLike = T)
clus3 = clara(x = mag72, k = 3, metric = "euclidean", stand = T, 
              samples = 1000, pamLike = T)
clus4 = clara(x = mag72, k = 4, metric = "euclidean", stand = T, 
              samples = 1000, pamLike = T)
clus5 = clara(x = mag72, k = 5, metric = "euclidean", stand = T, 
              samples = 1000, pamLike = T)
clus6 = clara(x = mag72, k = 6, metric = "euclidean", stand = T, 
              samples = 1000, pamLike = T)
clus7 = clara(x = mag72, k = 7, metric = "euclidean", stand = T, 
              samples = 1000, pamLike = T)
clus8 = clara(x = mag72, k = 8, metric = "euclidean", stand = T, 
              samples = 1000, pamLike = T)
clus9 = clara(x = mag72, k = 9, metric = "euclidean", stand = T, 
              samples = 1000, pamLike = T)
clus10 = clara(x = mag72, k = 10, metric = "euclidean", stand = T, 
              samples = 1000, pamLike = T)


sil_wi = c()
clna = paste0("clus",1:10)
for (i in 2:10){
      tester = get(clna[i])
      x = silhouette(tester)
      sil_wi[i] = x[1,3]
}

plot(sil_wi, type = "b")


mag7[,"class" := clus2$clustering]


mag7 %>% 
      group_by(class) %>% 
      summarize(
            amplitude = mean(amplitude),
            ar1 = mean(ar1),
            lam1 = mean(lam1),
            phase = mean(phase),
            tau2 = mean(tau2),
            tau3 = mean(tau3),
            tau4 = mean(tau4)
      ) %>% 
      ggparcoord(columns = 2:8, groupColumn = 'class', scale = 'globalminmax')

dist(mag72)
### --- hierarchical clustering --- ### 
hclust(d)