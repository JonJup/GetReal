# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### -----  Macroinvertebrates -------- ###
### -------- NMDS & PCOA ------------- ###
# -------------------------------------- #

# 09.11.20
# GetReal
# Working Package 2 
# Macroinvertebrates


# setup -------------------------------------------------------------------
if(!require(pacman))install.packages("pacman")
p_load(ape,
       data.table,
       dplyr,
       fuzzySim,
       ggplot2,
       ggrepel,
       here,
       vegan)
## DIRECTORIES 
DIR = list(rs = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/002_r_scripts/"),
           pd = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data//"),
           fu = here("005_documents/plot_scripts/") )

##FUNCTIONS 
source(file.path(DIR$fu, "f_quiet_sim_mat.R"))
## PREVIOUS SCRIPTS 
source(file.path(DIR$rs, "08_c_setup_ta_analysis.R"))
# COLOR PALETTE
my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")
#OPTIONS 
OPT = list(
        compute_distance = TRUE,
        NMDS = TRUE ,
        PCOA = TRUE ,
        REMOVE = TRUE 
)
# compute distance matrix -------------------------------------------------
if (OPT$compute_distance){
        mepa <- t(splist2presabs(data = dt_mzb, 
                                 sites.col = which(names(dt_mzb) == "group"),
                                 sp.col = which(names(dt_mzb) == "taxon")))
        colnames(mepa) = mepa[1,]
        mepa           = mepa[-1,]
        taxa_names     = rownames(mepa)
        mepa           = apply(mepa, 2, as.numeric)
        rownames(mepa) = taxa_names
        d_me           = 1 - quiet_sim_mat(mepa, method = "Jaccard")  
}

if (OPT$PCOA) {
        pcoa_obj = pcoa(D= d_me)
        plot_mzb_pcoa = pcoa_obj$vectors %>%
                as.data.frame() %>%         
                ggplot(aes(x=Axis.1, y=Axis.2)) + 
                geom_label_repel(label = rownames(pcoa_obj$vectors)) +
                xlab(paste("Axis 1")) + 
                ylab(paste("Axis 2")) + 
                ggtitle("PCoA of typical macroinvertebrate assemblages") + 
                labs(subtitle = paste("cumulative eigenvalues of first two axes: ", round(pcoa_obj$values$Cumul_eig[2],2)))
        }
if (OPT$NMDS) {
        me_NMDS   = metaMDS(comm = d_me,
                            try = 1000,
                            k = 2)
        dt_nmds   = data.table(
                NMDS1 = scores(me_NMDS)[, 1],
                NMDS2 = scores(me_NMDS)[, 2],
                river_type = unique(dt_mzb$group)
        )
        plot_mzb_nmds = ggplot(data = dt_nmds, aes(x = NMDS1, y = NMDS2)) +
                geom_label_repel(aes(label = river_type)) +
                ggtitle("NMDS of typical macroinvertebrate assemblages") +
                labs(subtitle = paste("Stress: ", round(me_NMDS$stress, 2)))
}

if (OPT$REMOVE) {
        remove = rm(
                call_ta_setup,
                dir_pd,
                dir_rs,
                dt_bta,
                dt_mzb,
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
