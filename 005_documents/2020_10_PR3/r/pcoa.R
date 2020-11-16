### -------------------------- ###
### -- script for PCoA ------- ###
### ---- presentation
###      GET REAL 
###      Progress Review 3 --- ###
### -------------------------- ###

# date written : 19.10.20 
# date changed :
# date used    : 19.10.20

source(here("005_documents/2020_10_Zwischenbericht an RLT/r/f_quiet_sim_mat.R"))

tm[,group:=str_replace(group, pattern="_",replacement="+")]

mepam <- t(splist2presabs(data = tm, 
                         sites.col = 6,
                         sp.col = 1))

colnames(mepam) <- mepam[1,]
mepam <- mepam[-1,]
taxa_namesm <- rownames(mepam)
mepam <- apply(mepam, 2, as.numeric)
rownames(mepam) <- taxa_namesm
d_me_m <- 1 - quiet_sim_mat(mepam, method = "Jaccard")
pcoa_objm <- pcoa(D= d_me_m)
bim <- pcoa_objm$vectors %>%
        as.data.frame() %>%
        mutate("river_type"=rownames(as.data.frame(pcoa_objm$vectors))) %>% 
        ggplot(aes(x=Axis.1, y=Axis.2)) + 
        geom_text(aes(label=river_type, col = river_type), size = 10) + 
        xlab(paste("PCoA Axis 1")) + 
        ylab(paste("PCoA Axis 2")) + 
        xlim(-0.5,.6) + 
        theme(text=element_text(size=20), legend.position = "none") + 
        theme_xaringan(background_color = base_color_code2) +
        scale_xaringan_color_discrete()


