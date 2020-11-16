### ---------------------- ###
### -- script for pcoa --- ###
### ---- in RLT Report --- ###
### ---------------------- ###

# date written : 03.10.20 
# date changed : 09.10.20
# date used    : 03.10.20

source(here("005_documents/2020_10_Zwischenbericht an RLT/r/f_quiet_sim_mat.R"))

#tm[,group:=str_replace(group, pattern="_",replacement="+")]

mepad <- t(splist2presabs(data = bty, 
                         sites.col = 7,
                         sp.col = 1))
mepam <- t(splist2presabs(data = tm, 
                         sites.col = 6,
                         sp.col = 1))


colnames(mepad) <- mepad[1,]
colnames(mepam) <- mepam[1,]

mepad <- mepad[-1,]
mepam <- mepam[-1,]

taxa_namesd <- rownames(mepad)
taxa_namesm <- rownames(mepam)

mepad <- apply(mepad, 2, as.numeric)
mepam <- apply(mepam, 2, as.numeric)

rownames(mepad) <- taxa_namesd
rownames(mepam) <- taxa_namesm

d_me_d <- 1 - quiet_sim_mat(mepad, method = "Jaccard")
d_me_m <- 1 - quiet_sim_mat(mepam, method = "Jaccard")

pcoa_objd <- pcoa(D= d_me_d)
pcoa_objm <- pcoa(D= d_me_m)

bid <- pcoa_objd$vectors %>%
        as.data.frame() %>%         
        ggplot(aes(x=Axis.1, y=Axis.2)) + 
        geom_label(label = rownames(pcoa_objd$vectors)) +
        xlab(paste("Axis 1")) + 
        ylab(paste("Axis 2"))
bim <- pcoa_objm$vectors %>%
        as.data.frame() %>%         
        ggplot(aes(x=Axis.1, y=Axis.2)) + 
        geom_label(label = rownames(pcoa_objm$vectors)) +
        xlab(paste("Axis 1")) + 
        ylab(paste("Axis 2"))

#rm(setdiff(ls(), c("bim", "bid")))
