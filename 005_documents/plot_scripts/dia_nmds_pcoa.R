# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------------ Diatoms ------------- ###
### ----------- NMDS & PCOA ---------- ###
# -------------------------------------- #

# 06.11.20
# GetReal
# Working Package 2 
# Diatoms


# setup -------------------------------------------------------------------
options(warn=-1)
if(!require(pacman))install.packages("pacman")
p_load(ape,
       data.table,
       dplyr,
       fuzzySim,
       ggplot2,
       here,
       vegan)
## DIRECTORIES 
dir_rs = here("002_working_package_02/001_community_data/002_combined/001_diatoms/002_r_scripts/")
dir_fu = here("005_documents/plot_scripts/")   
## FUNCTIONS 
call_to_quiet_distance = file.path(dir_fu, "f_quiet_sim_mat.R")
source(call_to_quiet_distance)
## PREVIOUS SCRIPTS 
call_ta_setup = file.path(dir_rs, "11_a_setup_ta_analysis.R")
source(call_ta_setup)
# COLOR PALETTE 
my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")
#OPTIONS 
NMDS = FALSE 
PCOA = TRUE 
REMOVE = TRUE

# Compute distance matrix  ------------------------------------------------

mepa <- t(splist2presabs(data = dt_bty, 
                         sites.col = 6,
                         sp.col = 1))
colnames(mepa) <- mepa[1,]
mepa           <- mepa[-1,]
taxa_names     <- rownames(mepa)
mepa           <- apply(mepa, 2, as.numeric)
rownames(mepa) <- taxa_names
d_me           <- 1 - quiet_sim_mat(mepa, method = "Jaccard")

if (PCOA) {
        pcoa_obj = pcoa(D= d_me)
        plot_dia_pcoa = pcoa_obj$vectors %>%
                as.data.frame() %>%         
                ggplot(aes(x=Axis.1, y=Axis.2)) + 
                geom_label(label = rownames(pcoa_obj$vectors)) +
                xlab(paste("Axis 1")) + 
                ylab(paste("Axis 2")) + 
                ggtitle("PCoA of typical diatoms assemblages") + 
                labs(subtitle = paste("cumulative eigenvalues of first two axes: ", round(pcoa_obj$values$Cumul_eig[2],2)))
}
if (NMDS) {
        me_NMDS   = metaMDS(comm = d_me,
                            try = 1000,
                            k = 2)
        dt_nmds   = data.table(
                NMDS1 = scores(me_NMDS)[, 1],
                NMDS2 = scores(me_NMDS)[, 2],
                river_type = unique(dt_bty$group)
        )
        plot_dia_nmds = ggplot(data = dt_nmds, aes(x = NMDS1, y = NMDS2)) +
                geom_label(aes(label = river_type)) +
                ggtitle("NMDS of typical diatoms assemblages") +
                labs(subtitle = paste("Stress: ", round(me_NMDS$stress, 2)))
}
if (REMOVE) {
        remove = rm(
                call_ta_setup,
                dir_pd,
                dir_rs,
                dt_bta,
                dt_bty,
                dt_fol,
                dt_fta,
                dt_fty,
                dt_gen,
                dt_gta,
                ch_river_types,
                dt_gty,
                dt_spe,
                dt_sta,
                dt_sty,
                i,
                river_type_var,
                dt_nmds,
                d_me,
                me_NMDS,
                mepa,
                my_color_palette,
                pcoa_obj,
                taxa_names,
                NMDS,
                PCOA,
                REMOVE,
                dir_fu,
                quiet_sim_mat,
                call_to_quiet_distance
        )
        rm(list = remove)
        rm(remove)
}