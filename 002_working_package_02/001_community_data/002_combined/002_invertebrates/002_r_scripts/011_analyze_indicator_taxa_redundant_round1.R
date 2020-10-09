# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### --------- Invertebrates ---------- ###
### -------- Redundant Round I ------- ###   
# -------------------------------------- #

# date written/ modified: 21.08.20 + 14.09 
# date used: 21.08.20 + 14.09 + 30. + 02.10
# Jonathan Jupke 
# Get Real WP2 
# Macroinvertebrates 


# Overview River types  ---------------------------------------------------

#   RT-01 Very large rivers
#   RT-02 Lowland, siliceous, medium-large
#   RT-03 Lowland, siliceous, very small-small
#   RT-04 Lowland, calcareous or mixed, medium-large
#   RT-05 Lowland, calcareous or mixed, very small-small
#   RT-06 Lowland, organic and siliceous, very small-large
#   RT-08 Mid-altitude, siliceous, medium-large
#   RT-09 Mid-altitude, siliceous, very small-small
#   RT-10 Mid-altitude, calcareous or mixed, medium-large
#   RT-11 Mid-altitude, calcareous or mixed, very small-small
#   RT-12 Mid-altitude, organic and siliceous, very small-large 
#   RT-13 Mid-altitude, organic and calcareous/mixed
#   RT-14 Highland (all Europe), siliceous, incl. organic (humic)
#   RT-15 Highland (all Europe), calcareous/mixed
#   RT-16 Glacial rivers (all Europe)
#   RT-17 Mediterranean, lowland, medium-Large, perennial
#   RT-18 Mediterranean, mid altitude, medium-large,
#   RT-19 Mediterranean, very small-small, perennial


# setup -------------------------------------------------------------------
pacman::p_load(ape, dplyr, magrittr, data.table, 
               stringr, taxize, purrr, 
               ggplot2, here, viridis, 
               fuzzySim, vegan, Rtsne,
               ggrepel)
setwd(here(... = "002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/"))
## -- functions -- ##  
source("../002_r_scripts/F_001_Redundnat.R")

# load data ---------------------------------------------------------------
spe <- readRDS("007_2020-09-14_indicator_spe_reduntant_1.RDS")
gen <- readRDS("007_2020-09-14_indicator_gen_reduntant_1.RDS")
foh <- readRDS("007_2020-09-14_indicator_foh_reduntant_1.RDS")

my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")


# create typical communities ----------------------------------------------
spe_t <- spe[B > 0.25 | B > 0.20 & p_value <= 0.05 | A > 0.80]
gen_t <- gen[B > 0.50 | B > 0.33 & p_value <= 0.05 | A > 0.95]
foh_t <- foh[B > 0.95 | B > 0.80 & p_value <= 0.01 | A > 0.99]

spe_t <- spe_t[taxon != "Notonectidae"]

tm <- rbindlist(list(spe_t, 
                     gen_t, 
                     foh_t))
tm[,.N,by=group]
# setwd("../003_processed_data/")
# saveRDS(file = paste0("08_", Sys.Date(),"_macroinvertebrate_typical_assemblages.RDS"),object =  tm)

rt_vector <- unique(tm$group)
(n_types   <- length(rt_vector))

## -- redundancy for medium -- ## 

# No Redundancies! Carry on with sensitivity analysis. 
redundant(x = 1 , data = tm)
redundant(x = 2 , data = tm)
redundant(x = 3 , data = tm)
redundant(x = 4 , data = tm)
redundant(x = 5 , data = tm)
redundant(x = 6 , data = tm)
redundant(x = 7 , data = tm)
redundant(x = 8 , data = tm)
redundant(x = 9 , data = tm)


# Sensitivity analysis  ---------------------------------------------------

n_loop <- 10
a_loop <- round(seq(from = 0.500, to = 1.00, length.out = n_loop), 2)
b_loop <- round(seq(from = 0.100, to = 0.75, length.out = n_loop), 2)

out_list <- list() 

# -- sensitivity analysis loop 
for (i in 1:(n_loop * n_loop)) {
    if (i == 1){
        sen_out <- data.table(as.data.frame(matrix(0, nrow = n_types * n_loop^2, ncol = 6)))
        names(sen_out) <- c("river_type", "A_threshold", "B_threshold", "n_species", "n_genera", "n_foh")
        sen_out[, c("river_type", "A_threshold", "B_threshold")  := 
                    .(rep(rt_vector, each  = n_loop, times = n_loop), 
                      rep(a_loop   , each  = n_types * n_loop), 
                      rep(b_loop   , times = n_types * n_loop)
                    )]
    }
    
    A_ind <- floor((i+9)/10)
    B_ind <- i - 10 * floor(i / 10)
    if (B_ind == 0) B_ind <- 10
    
    spe_typ <- spe[B >  b_loop[B_ind] |
                       B > (b_loop[B_ind] - 0.25 * b_loop[B_ind]) &
                       p_value <= 0.05 |
                       A >  a_loop[A_ind]
    ]
    gen_typ <- gen[B > min((b_loop[B_ind] * 2), 1) |
                       B > (min((b_loop[B_ind] * 2), 1) - 0.25 * min((b_loop[B_ind] * 2), 1)) &
                       p_value <= 0.05 |
                       A >  min(a_loop[A_ind] * 1.25,1)
    ]
    foh_typ <- foh[B > min((b_loop[B_ind] * 3), 1) |
                       B > (min((b_loop[B_ind] * 3), 1) - 0.25 * min((b_loop[B_ind] * 3), 1)) &
                       p_value <= 0.05 |
                       A >  min(a_loop[A_ind] * 1.5,1)
    ]
    spe_typ[, c("A_threshold", "B_threshold", "taxon_level") := .(a_loop[A_ind]               , b_loop[B_ind]           , "species")]
    gen_typ[, c("A_threshold", "B_threshold", "taxon_level") := .(min(a_loop[A_ind] * 1.25, 1), min(b_loop[B_ind] * 2,1), "genus")]
    foh_typ[, c("A_threshold", "B_threshold", "taxon_level") := .(min(a_loop[A_ind] * 1.50, 1), min(b_loop[B_ind] * 3,1), "foh")]
    all_typ <- rbindlist(list(spe_typ, gen_typ, foh_typ))
    
    unspe <- unique(spe_typ$taxon)
    ungen <- unique(gen_typ$taxon)
    unfoh <- unique(foh_typ$taxon)
    unall <- unique(all_typ$taxon)
    for (k in seq_along(unspe)) {
        spe_typ[taxon == unspe[k], score := 1/spe_typ[taxon == unspe[k], .N]]
    }
    for (k in seq_along(ungen)){
        gen_typ[taxon == ungen[k], score := 1/gen_typ[taxon == ungen[k], .N]]
    }
    for (k in seq_along(unfoh)){
        foh_typ[taxon == unfoh[k], score := 1/foh_typ[taxon == unfoh[k], .N]]
    }
    for (k in seq_along(unall)){
        all_typ[taxon == unall[k], score := 1/all_typ[taxon == unall[k], .N]]
    }
    for (l in seq_along(unique(spe_typ$group))) {
        spe_typ[group == unique(spe_typ$group)[l], group_score := sum(score)/.N]
    }
    for (l in seq_along(unique(gen_typ$group))) {
        gen_typ[group == unique(gen_typ$group)[l], group_score := sum(score)/.N]
    }
    for (l in  seq_along(unique(foh_typ$group))) {
        foh_typ[group == unique(foh_typ$group)[l], group_score := sum(score)/.N]
    }
    for (l in  seq_along(unique(all_typ$group))) {
        all_typ[group == unique(all_typ$group)[l], group_score := sum(score)/.N]
    }
    
    for (j in 1:n_types) {
        
        sen_out[river_type  == rt_vector[j]   & 
                    B_threshold == b_loop[B_ind] & 
                    A_threshold == a_loop[A_ind],
                c("n_species", "n_genera", "n_foh", "us_spe", "us_gen", "us_foh", "us_taxa") := 
                    .(
                        spe_typ[group == rt_vector[j], .N],
                        gen_typ[group == rt_vector[j], .N],
                        foh_typ[group == rt_vector[j], .N],
                        ifelse (length(spe_typ[group == rt_vector[j], unique(group_score)]) == 0, 
                                NA,
                                spe_typ[group == rt_vector[j], unique(group_score)]),
                        ifelse (length(gen_typ[group == rt_vector[j], unique(group_score)]) == 0, 
                                NA,
                                gen_typ[group == rt_vector[j], unique(group_score)]),
                        ifelse (length(foh_typ[group == rt_vector[j], unique(group_score)]) == 0, 
                                NA,
                                foh_typ[group == rt_vector[j], unique(group_score)]),
                        ifelse (length(all_typ[group == rt_vector[j], unique(group_score)]) == 0, 
                                NA,
                                all_typ[group == rt_vector[j], unique(group_score)])
                    )]
        
    }
    
    ## -- debug zone -- ## 
    # sen_out[river_type == rt_vector[4] & 
    #             B_threshold == b_loop[B_ind] & 
    #             A_threshold == a_loop[A_ind], 
    #         n_species := ]
    ## --               -- ## 
    
    out_list[[i]] <- rbindlist(list(spe_typ, gen_typ, foh_typ))
    rm(spe_typ, gen_typ, foh_typ, unspe, ungen, unfoh);gc()
    print(i)
}

### -- New data set 
out_comb <- rbindlist(out_list)


### --- Plots --- ### 
sen_out$A_col   <- factor(sen_out$A_threshold)
sen_out$river_type %<>% str_remove_all(pattern = "RT")
sen_out$river_type %<>% factor(levels = c("1", "2_3", "4_5", "8_9", "10_11", "12","14", "15_16", "18"))
sen_out[, n_taxa := n_species + n_genera + n_foh]

#saveRDS(sen_out, "00x_for_plot_mzb_sa.RDS")

(plot_sa_tr_spe     <- sen_out %>% ggplot(aes(x = B_threshold, y = n_species        , col = A_threshold)) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Richness - Total Change"  )      + facet_wrap(.~river_type) + scale_color_viridis() + ylab("species richness") + xlab("B Threshold") + labs(col = "A Threshold"))  
(plot_sa_tr_gen     <- sen_out %>% ggplot(aes(x = B_threshold, y = n_genera         , col = A_threshold)) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Richness - Total Change"  )      + facet_wrap(.~river_type) + scale_color_viridis() + ylab("species richness") + xlab("B Threshold") + labs(col = "A Threshold"))  
(plot_sa_tr_foh     <- sen_out %>% ggplot(aes(x = B_threshold, y = n_foh            , col = A_threshold)) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Richness - Total Change"  )      + facet_wrap(.~river_type) + scale_color_viridis() + ylab("species richness") + xlab("B Threshold") + labs(col = "A Threshold"))  
(plot_sa_tr_all     <- sen_out %>% ggplot(aes(x = B_threshold, y = n_taxa           , col = A_threshold)) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Richness - Total Change"  )      + facet_wrap(.~river_type) + scale_color_viridis() + ylab("species richness") + xlab("B Threshold") + labs(col = "A Threshold"))  

(plot_sa_us_spe   <- sen_out %>% ggplot(aes(x = B_threshold, y = us_spe           , col = A_threshold)) + geom_hline(yintercept = 1/n_types, linetype = 2) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Uniqueness - Total Change")      + facet_wrap(.~river_type) + scale_color_viridis() + ylab("uniqueness score") + xlab("B Threshold") + labs(col = "A Threshold"))
(plot_sa_us_gen   <- sen_out %>% ggplot(aes(x = B_threshold, y = us_gen           , col = A_threshold)) + geom_hline(yintercept = 1/n_types, linetype = 2) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Uniqueness - Total Change")      + facet_wrap(.~river_type) + scale_color_viridis() + ylab("uniqueness score") + xlab("B Threshold") + labs(col = "A Threshold"))
(plot_sa_us_foh   <- sen_out %>% ggplot(aes(x = B_threshold, y = us_foh           , col = A_threshold)) + geom_hline(yintercept = 1/n_types, linetype = 2) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Uniqueness - Total Change")      + facet_wrap(.~river_type) + scale_color_viridis() + ylab("uniqueness score") + xlab("B Threshold") + labs(col = "A Threshold"))
(plot_sa_us_all   <- sen_out %>% ggplot(aes(x = B_threshold, y = us_taxa          , col = A_threshold)) + geom_hline(yintercept = 1/n_types, linetype = 2) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Uniqueness - Total Change")      + facet_wrap(.~river_type) + scale_color_viridis() + ylab("uniqueness score") + xlab("B Threshold") + labs(col = "A Threshold"))

plots      <- c("plot_sa_tr_spe", "plot_sa_tr_gen", "plot_sa_tr_foh", "plot_sa_tr_all", "plot_sa_us_spe", "plot_sa_us_gen", "plot_sa_us_foh", "plot_sa_us_all")
file_names <-  paste0("sa_",
                      rep(c("spe", "gen", "foh", "all"), times = 2),
                      "_",
                      rep(c("tr", "us")  , each = 4))
                
ggthemr("greyscale")
for (i in 1:8) {
    ggsave(
        file = paste0(
            "../004_plots/002_zwischenbericht/00",
            i,
            "_",
            Sys.Date(),
            file_names[i],
            ".jpeg"
        ),
        plot   = get(plots[i]),
        width  = 297,
        height = 210,
        units  = "mm",
        dpi = 400
    )
}

ac <- data.table(
    river_type = character(n_types),
    species    =   integer(n_types),
    genus      =   integer(n_types),
    foh        =   integer(n_types)
)

for (i in 1:n_types) {
    ac[i, river_type := rt_vector[i]]
    ac[i, c("species", "genus", "foh") :=
           .(spe_t[group == rt_vector[i], .N],
             gen_t[group == rt_vector[i], .N],
             foh_t[group == rt_vector[i], .N]
             )
       ]
}

# drop stream types that were not considered
ac      <- ac[!(species == 0 & genus == 0)]
# reshape for plot
acp <- melt(
    ac,
    id.vars = c("river_type"),
    measure.vars = c("species", "genus", "foh")
)

acp$river_type %<>% str_remove_all(pattern = "RT")
acp[value == 0, value := NA]

acp$river_type %<>% factor(levels = c("1", "2_3", "4_5", "8_9", "10_11", "12","14", "15_16", "18"))

(taxa_per_level_plot <-
    ggplot(data = acp, aes(x = river_type, y = value)) +
    geom_point(aes(col = variable), size = 3) +
    scale_color_manual(values = my_color_palette[c(1,2,4)]) +
    ylab("number of taxa") +
    xlab("river type") + 
    labs(col = "taxon level") )

ggsave(filename = paste0("../004_plots/002_zwischenbericht//009_",
                         Sys.Date(),
                         "_number_of_taxa_per_level.jpeg"),
       plot = taxa_per_level_plot,
       width  = 150,
       height = 100,
       units = 'mm',
       dpi = 400)

# How different are the assemblages? --------------------------------------

utm <- sort(unique(tm$taxon))
# compute a "uniqueness score". Each taxon gets a score based on the number of
# TAs its part of. The score is 1 for taxa that only occur in one TA and 1/4 in
# those that occur in four. The score of a stream type is the mean score of its
# taxa.
for (i in seq_along(utm)) {
    tm[taxon == utm[i], score := 1/tm[taxon == utm[i], .N]]
}
for (i in seq_along(unique(tm$group))) {
    tm[group == unique(tm$group)[i], group_score := sum(score)/.N]
}

tm$group %<>% str_remove_all(pattern = "RT")
tm$group %<>% factor(levels = c("1", "2_3", "4_5", "8_9", "10_11", "12","14", "15_16", "18"))

(plot_uniqueness_scores <-
    ggplot(data = tm, aes(x = group, y = group_score))  +
    geom_point(size = 2) +
    ylim(c(0, 1)) +
    geom_hline(
        col = "red",
        yintercept = 1 / length(unique(tm$group)),
        linetype = 2
    ) +
    ylab("Uniqueness score") +
    xlab("River Type"))

ggsave(filename = paste0("../004_plots/002_zwischenbericht//010_",
                         Sys.Date(),
                         "_uniqueness_scores.jpeg"),
       plot = plot_uniqueness_scores,
       dpi = 400,
       height = 100,
       width = 150,
       units = "mm")

# tsne --------------------------------------------------------------------
dfu <- copy(tm)
dfu[,c("A", "B", "sqrtIV", "p_value", "n_taxa", "score", "group_score") := NULL]
dfu$score <- 1
dfu %<>% dcast(taxon ~ group,
               value.var = "score") %>%
    as.matrix()
dfu[which(is.na(dfu))] <- 0
dfu          <- dfu[!duplicated(dfu[,-1]), ]
dfu          <- t(dfu) %>% as.data.frame()
names(dfu)   <- dfu[1,]
dfu          <- dfu[-1,]
dfu2         <- apply(dfu, 2, as.numeric)
tsne_results <- Rtsne(X                = dfu2, 
                      perplexity       = 2,
                      check_duplicates = FALSE, 
                      max_iter         = 5000000,
                      theta            = 00)
plot_data <- tsne_results$Y %>% as.data.frame() %>% setDT
plot_data[, stream_type := factor(rownames(dfu))]
(tsne_plot <- ggplot(data = plot_data,
       aes(
           x = V1,
           y = V2,
           col = stream_type,
           fill = stream_type
       )) +
    scale_color_manual(values = my_color_palette) +
    scale_fill_manual(values = my_color_palette) +
    geom_point(size = 4, shape = 21) +
    geom_text_repel(aes(label = stream_type)) +
    theme_grey() +
    theme(legend.position = "none") )

ggsave(filename = paste0("../004_plots/002_zwischenbericht//011_",
                         Sys.Date(),
                         "_t-SNE.jpeg"),
       plot = tsne_plot,
       dpi = 400)
# NMDS --------------------------------------------------------------------
mepa <- t(splist2presabs(data = tm, 
                         sites.col = 6,
                         sp.col = 1))

colnames(mepa) <- mepa[1,]
mepa <- mepa[-1,]
taxa_names <- rownames(mepa)
mepa <- apply(mepa, 2, as.numeric)
rownames(mepa) <- taxa_names
d_me <- 1 - simMat(mepa, method = "Jaccard")
pcoa_obj <- pcoa(D= d_me)
me_NMDS <- metaMDS(comm = d_me, try = 1000, k = 2)
par(mfrow = c(1,2))
pcoa_biplot <- biplot(pcoa_obj)
plot(me_NMDS, type = "t", main = "NDMS")
stressplot(me_NMDS)

summary(pcoa_obj)

jpeg(filename = paste0("../004_plots/002_zwischenbericht//008_",
                       Sys.Date(),
                       "_NMDS.jpeg"), 
     width = 600, units = "px")
par(mfrow = c(1,1))

plot(me_NMDS, type = "t", main = "NDMS")
dev.off()

# My own NMDS -------------------------------------------------------------

my_own_distance <- data.frame(matrix(0, ncol = n_types, nrow = n_types))
names(my_own_distance) <- rt_vector
rownames(my_own_distance) <- rt_vector
for (i in seq_along(rt_vector)) {
    print(paste("start", i))
    
    riv_x      <- rt_vector[i]
    riv_x_taxa <- tm[group == riv_x, unique(taxon)]
    n_taxa_x   <- length(riv_x_taxa)
    
    for (j in 1:length(rt_vector)){
        riv_y <- rt_vector[j]
        riv_y_taxa <- tm[group == riv_y, unique(taxon)]
        my_own_distance[i,j] <-  round(sum(riv_x_taxa %in% riv_y_taxa)/n_taxa_x * 100,1)
    }
    
    print(paste("end", i))
}

sym_dist <- (my_own_distance[upper.tri(my_own_distance)] + my_own_distance[lower.tri(my_own_distance)]) / 2

m1 <- my_own_distance
m1[lower.tri(m1)] <- sym_dist
m2 <- t(m1)
m2[lower.tri(m2)] <- sym_dist

#my_own_distance2 <- as.dist(my_own_distance) 
my_own_distance2 <- 100 - m2 %>% as.dist()
my_d_NMDS         <- metaMDS(comm = my_own_distance2, try = 1000, k = 2)

    
(plot_nmds <- scores(my_d_NMDS) %>% 
    as.data.frame() %>% 
    ggplot(aes(x = NMDS1, y = NMDS2)) +
    geom_text(label = rt_vector))


ggsave(filename = paste0("../004_plots/002_zwischenbericht//011_",
                         Sys.Date(),
                         "_NMDS.jpeg"),
       plot = plot_nmds,
       dpi = 300)

# Actual lists  -----------------------------------------------------------
tm[group == "18", taxon] %>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()


