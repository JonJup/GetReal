# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------------ Diatoms ------------- ###
# -------------------------------------- #

# date written/ modified: 04.08.20  
# date used: 04.08.20, 18., 19. + 14.09
# Jonathan Jupke 
# Diatoms GetReal WP2

# setup -------------------------------------------------------------------
#devtools::install_github('cttobin/ggthemr')
pacman::p_load(dplyr, magrittr, data.table, stringr, taxize, purrr,
               ggplot2, here, viridis, fuzzySim, vegan, umap, Rtsne,
               ggrepel, ggthemr, ape)
setwd(here("002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/"))

## -- functions 
redundant <- function(x){
    riv_x      <- ifelse(x>9, paste0("RT",x), paste0("RT0", x))
    riv_x_taxa <- bty[group == riv_x, unique(taxon)]
    n_taxa_x   <- length(riv_x_taxa)
    # typical_me[river_type == riv_x, redundancy := sum(typical_me[river_type == riv_x, taxon] %in% typical_me[river_type == riv_y, taxon] /typical_me[river_type == riv_x, .N]) * 100]
    redundancy <- list()
    list_id <- 1
    for (i in c(1,2,3,4,5,6,8,9,12,16,17,18,19)){
        if (i == x) next()
        riv_y <- ifelse(i>9, paste0("RT", i), paste0("RT0", i))
        riv_y_taxa <- bty[group == riv_y, unique(taxon)]
        redundancy[[list_id]] <-  round(sum(riv_x_taxa %in% riv_y_taxa)/n_taxa_x * 100,1)
        names(redundancy[[list_id]]) <- riv_y
        list_id <- list_id + 1
    }
    redundancy <- unlist(redundancy)
    max_red <- max(redundancy)
    max_id  <- which(redundancy == max_red)
    if (length(max_id) > 1) {
        max_id <- paste(names(max_id), collapse = "+")
        print(paste("most similiar is", max_id, "with", max_red))
    } else {
        print(paste("most similiar is", names(max_id), "with", max_red))
    }
    
    
}

# load data ---------------------------------------------------------------
spe <- readRDS("010_2020-09-14_indicator_spe_medi.RDS")
gen <- readRDS("010_2020-09-14_indicator_gen_medi.RDS")
my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")
rt_vector <- paste0("RT", c("01","02","03","04","05","06","08","09","12","16","17","18","19"))
n_types   <- length(rt_vector) 



# Deriving initial TAs ---------------------------------------------------
sty <- spe[B > 0.4 | B > 0.3 & p_value <= 0.05 | A > 0.7]
gty <- gen[B > 0.8 | B > 0.6 & p_value <= 0.05 | A > .95]

bty <- rbindlist(list(sty, gty))

saveRDS(file = paste0("011_", Sys.Date(),"_daitom_typical_assemblages.RDS"),object =  bty)

# Redundancy  -------------------------------------------------------------
redundant(1)
redundant(2)
redundant(3)
redundant(4)
redundant(5)
redundant(6)
redundant(8)
redundant(9)
redundant(12)
redundant(16)
redundant(17)
redundant(18)
redundant(19)


# sensitivity analysis ----------------------------------------------------

# The sensitivity analysis will consists of: 
# Varying the parameters B and A
# Recording changes in species richness and uniqueness score at the level of genus and species. 
# Transform to percent changes
# Preparing a data set to plot all subsequent plots with estimates of uncertainty.  

n_loop <- 10
a_loop <- round(seq(from = 0.500, to = 1.00, length.out = n_loop), 2)
b_loop <- round(seq(from = 0.100, to = 0.75, length.out = n_loop), 2)

out_list <- list() 

# -- sensitivity analysis loop 
for (i in 1:(n_loop * n_loop)) {
    if (i == 1){
        sen_out <- data.table(as.data.frame(matrix(0, nrow = n_types * n_loop^2, ncol = 6)))
        names(sen_out) <- c("river_type", "A_threshold", "B_threshold", "n_species", "n_genera", "us")
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
    
    spe_typ[, c("A_threshold", "B_threshold", "taxon_level") := .(a_loop[A_ind], b_loop[B_ind], "species")]
    gen_typ[, c("A_threshold", "B_threshold", "taxon_level") := .(min(a_loop[A_ind] * 1.25, 1), min(b_loop[B_ind] * 2,1), "genus")]

    
    unspe <- unique(spe_typ$taxon)
    ungen <- unique(gen_typ$taxon)
    for (k in seq_along(unspe)) {
        spe_typ[taxon == unspe[k], score := 1/spe_typ[taxon == unspe[k], .N]]
    }
    for (k in seq_along(ungen)){
        gen_typ[taxon == ungen[k], score := 1/gen_typ[taxon == ungen[k], .N]]
    }
    for (l in seq_along(unique(spe_typ$group))) {
        spe_typ[group == unique(spe_typ$group)[l], group_score := sum(score)/.N]
    }
    for (l in seq_along(unique(gen_typ$group))) {
        gen_typ[group == unique(gen_typ$group)[l], group_score := sum(score)/.N]
    }
    
    
    
    for (j in 1:13) {
        
        sen_out[river_type  == rt_vector[j]   & 
                B_threshold == b_loop[B_ind] & 
                A_threshold == a_loop[A_ind],
                c("n_species", "n_genera", "us_spe", "us_gen") := 
                    .(
                        spe_typ[group == rt_vector[j], .N],
                        gen_typ[group == rt_vector[j], .N],
                        spe_typ[group == rt_vector[j], unique(group_score)],
                        ifelse (length(gen_typ[group == rt_vector[j], unique(group_score)]) == 0, 
                                NA,
                                gen_typ[group == rt_vector[j], unique(group_score)])
                        
                    )]

    }
    
    ## -- debug zone -- ## 
    # sen_out[river_type == rt_vector[4] & 
    #             B_threshold == b_loop[B_ind] & 
    #             A_threshold == a_loop[A_ind], 
    #         n_species := ]
    ## --               -- ## 
    
    out_list[[i]] <- rbindlist(list(spe_typ, gen_typ))
    rm(spe_typ, utm);gc()
    print(i)
}

# -- Percent change 
for (j in 1:n_loop) {
    for (i in 2:n_loop) {
        for (k in 1:13) {
           b_new     <- sen_out[river_type  == rt_vector[k] & A_threshold == a_loop[j] & B_threshold == b_loop[i]    , n_species]
           b_from    <- sen_out[river_type  == rt_vector[k] & A_threshold == a_loop[j] & B_threshold == b_loop[i - 1], n_species]
           b_change  <- ((b_new - b_from)/ b_from) 
           us_new    <- sen_out[river_type  == rt_vector[k] & A_threshold == a_loop[j] & B_threshold == b_loop[i]    , us_spe]
           us_from   <- sen_out[river_type  == rt_vector[k] & A_threshold == a_loop[j] & B_threshold == b_loop[i - 1], us_spe]
           us_change <- ((us_new - us_from)/ us_from)
           
           sen_out[river_type  == rt_vector[k] & A_threshold == a_loop[j] & B_threshold == b_loop[i], 
                   c("percent_change_b", "percent_change_us") := . (b_change, us_change)]
    
           
}}
    print(j)
}


# -- New data sets 
out_comb <- rbindlist(out_list)


# Plots from Sensitivity Analysis -----------------------------------------
sen_out$A_col <- factor(sen_out$A_threshold)
sen_out$river_type %<>% str_remove_all(pattern = "RT")

saveRDS(sen_out, "00x_plotdata_sensitivity_analysis.RDS")

sensitivity_richness_total_plot     <- sen_out %>% ggplot(aes(x = B_threshold, y = n_species        , col = A_threshold)) +                                               geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Richness - Total Change"  )      + facet_wrap(.~river_type) + scale_color_viridis() + ylab("species richness") + xlab("B Threshold") + labs(col = "A Threshold")  
sensitivity_uniqueness_total_plot   <- sen_out %>% ggplot(aes(x = B_threshold, y = us_spe           , col = A_threshold)) + geom_hline(yintercept = 1/13, linetype = 2) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Uniqueness - Total Change")      + facet_wrap(.~river_type) + scale_color_viridis() + ylab("uniqueness score") + xlab("B Threshold") + labs(col = "A Threshold")
sensitivity_richness_percent_plot   <- sen_out %>% ggplot(aes(x = B_threshold, y = percent_change_b , col = A_threshold)) + geom_hline(yintercept = 0   , linetype = 2) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Richness - Change in Percent")   + facet_wrap(.~river_type) + scale_color_viridis() + ylab("species richness") + xlab("B Threshold") + labs(col = "A Threshold")
sensitivity_uniqueness_percent_plot <- sen_out %>% ggplot(aes(x = B_threshold, y = percent_change_us, col = A_threshold)) + geom_hline(yintercept = 0   , linetype = 2) + geom_line(size = 1.5, alpha = 0.8, aes(group = A_col)) + ggtitle("Uniqueness - Change in Percent") + facet_wrap(.~river_type) + scale_color_viridis() + ylab("uniqueness score") + xlab("B Threshold") + labs(col = "A Threshold")




plots      <- c("sensitivity_richness_total_plot", "sensitivity_uniqueness_total_plot", "sensitivity_richness_percent_plot", "sensitivity_uniqueness_percent_plot")
file_names <- c("_sensitivity_analysis_species_richness_total", "_sensitivity_analysis_uniqueness_total", "_sensitivity_analysis_species_richness_percent", "_sensitivity_analysis_uniqueness_percent")
ggthemr("greyscale")
for (i in 1:4) {
    ggsave(
        file = paste0(
            "../004_plots/Zwischenbericht Diatom TAs/00",
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

# I did not use these two plots in the past. But I think they are quite nice. 
# sen_out %>%  ggplot(aes(x = river_type,  y = us_spe           , col = B_threshold)) + geom_jitter(width = .2, size = 2)           + geom_violin(col = alpha("black", 0.2), fill=alpha("black", 0.2), draw_quantiles = 0.5) + ylim(c(0,1)) +     geom_hline( col = "red",  yintercept = 1/length(unique(sen_out$river_type)),  linetype = 2) + ylab("Uniqueness score") + xlab("River Type")# sen_out %>%  ggplot(aes(x = river_type,  y = us_spe           , col = B_threshold)) + geom_jitter(width = .2, size = 2)           + geom_violin(col = alpha("black", 0.2), fill=alpha("black", 0.2), draw_quantiles = 0.5) + ylim(c(0,1)) +     geom_hline( col = "red",  yintercept = 1/length(unique(sen_out$river_type)),  linetype = 2) + ylab("Uniqueness score") + xlab("River Type")
# sen_out %>%  ggplot(aes(x = river_type,  y = us_spe           , col = A_threshold)) + geom_jitter(width = .2, size = 2)           + geom_violin(col = alpha("black", 0.2), fill=alpha("black", 0.2), draw_quantiles = 0.5) + ylim(c(0,1)) +     geom_hline( col = "red",  yintercept = 1/length(unique(sen_out$river_type)),  linetype = 2) + ylab("Uniqueness score") + xlab("River Type")# sen_out %>%  ggplot(aes(x = river_type,  y = us_spe           , col = A_threshold)) + geom_jitter(width = .2, size = 2)           + geom_violin(col = alpha("black", 0.2), fill=alpha("black", 0.2), draw_quantiles = 0.5) + ylim(c(0,1)) +     geom_hline( col = "red",  yintercept = 1/length(unique(sen_out$river_type)),  linetype = 2) + ylab("Uniqueness score") + xlab("River Type")

# create typical communities ----------------------------------------------

# from invertebrate species
# sty <- spe[B > 0.4 | B > 0.3 & p_value <= 0.05 | A > 0.7]
# gty <- gen[B > 0.8 | B > 0.6 & p_value <= 0.05 | A > .95]
# 
# bty <- rbindlist(list(sty, gty))

# What taxon level are the different typical assemblages made of ? ----------
ac <- data.table(
    river_type = character(20),
    species    =   integer(20),
    genus      =   integer(20)
)

for (i in 1:20) {
    river_type_var <- ifelse(i < 10, paste0("0", i), i)
    river_type_var <- paste0("RT", river_type_var)
    ac[i, river_type := river_type_var]
    ac[i, c("species", "genus") :=
           .(sty[group == river_type_var, .N],
             gty[group == river_type_var, .N])]
}

# drop stream types that were not considered
ac      <- ac[!(species == 0 & genus == 0)]
# reshape for plot
acp <- melt(
    ac,
    id.vars = c("river_type"),
    measure.vars = c("species", "genus")
)

acp$river_type %<>% str_remove_all(pattern = "RT")
acp[value == 0, value := NA]

taxa_per_level_plot <-
    ggplot(data = acp, aes(x = river_type, y = value)) +
    geom_point(aes(col = variable), size = 3) +
    scale_color_manual(values = my_color_palette) +
    ylab("number of taxa") +
    xlab("river type")

ggsave(filename = paste0("../004_plots/Zwischenbericht Diatom TAs/005_",
                         Sys.Date(),
                         "_number_of_taxa_per_level.jpeg"),
       plot = taxa_per_level_plot,
       dpi = 400)

# How different are the assemblages? --------------------------------------

utm <- sort(unique(bty$taxon))
# compute a "uniqueness score". Each taxon gets a score based on the number of
# TAs its part of. The score is 1 for taxa that only occur in one TA and 1/4 in
# those that occur in four. The score of a stream type is the mean score of its
# taxa.
for (i in seq_along(utm)) {
    bty[taxon == utm[i], score := 1/bty[taxon == utm[i], .N]]
}
for (i in seq_along(unique(bty$group))) {
    bty[group == unique(bty$group)[i], group_score := sum(score)/.N]
}

bty$group %<>% str_remove_all(pattern = "RT")
plot_uniqueness_scores <-
    ggplot(data = bty, aes(x = group, y = group_score))  +
    geom_point(size = 2) +
    ylim(c(0, 1)) +
    geom_hline(
        col = "red",
        yintercept = 1 / length(unique(bty$group)),
        linetype = 2
    ) +
    ylab("Uniqueness score") +
    xlab("River Type")

ggsave(filename = paste0("../004_plots/Zwischenbericht Diatom TAs/006_",
                         Sys.Date(),
                         "_uniqueness_scores.jpeg"),
       plot = plot_uniqueness_scores,
       dpi = 400)

# tsne --------------------------------------------------------------------
dfu <- copy(bty)
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
                      perplexity       = 4, 
                      check_duplicates = FALSE, 
                      max_iter         = 5000000,
                      theta            = 00)
plot_data    <- tsne_results$Y %>% as.data.frame() %>% setDT
plot_data[, stream_type := factor(rownames(dfu))]


tsne_plot <- ggplot(data = plot_data,
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
    theme(legend.position = "none") 
    
ggsave(filename = paste0("../004_plots/Zwischenbericht Diatom TAs/007_",
                         Sys.Date(),
                         "_t-SNE.jpeg"),
       plot = tsne_plot,
       dpi = 400)
# NMDS --------------------------------------------------------------------
mepa <- t(splist2presabs(data = bty, 
                                sites.col = 7,
                                sp.col = 1))

colnames(mepa) <- mepa[1,]
mepa <- mepa[-1,]
taxa_names <- rownames(mepa)
mepa <- apply(mepa, 2, as.numeric)
rownames(mepa) <- taxa_names
d_me <- 1 - simMat(mepa, method = "Jaccard")
plot(prcomp(d_me))
pcoa_obj <- pcoa(D= d_me)
par(mfrow = c(1,1))
jpeg(filename = paste0("../004_plots/Zwischenbericht Diatom TAs/008_", Sys.Date(), "_pcoa.jpeg"))
biplot(pcoa_obj)
dev.off()

me_NMDS <- metaMDS(comm = d_me, try = 1000, k = 2)
par(mfrow = c(1,2))
biplot(pcoa_obj)
plot(me_NMDS, type = "t", main = "NDMS")
stressplot(me_NMDS)

#   R-01 Very large rivers
#   R-02 Lowland, siliceous, medium-large
#   R-03 Lowland, siliceous, very small-small
#   R-04 Lowland, calcareous or mixed, medium-large
#   R-05 Lowland, calcareous or mixed, very small-small
#   R-06 Lowland, organic and siliceous, very small-large
#   R-08 Mid-altitude, siliceous, medium-large
#   R-09 Mid-altitude, siliceous, very small-small
#   R-10 Mid-altitude, calcareous or mixed, medium-large
#   R-12 Mid-altitude, organic and siliceous, very small-large __ All sites in Scandinavia. Explains outlier position. 
#   R-14 Highland (all Europe), siliceous, incl. organic (humic)
#   R-16 Glacial rivers (all Europe)
#   R-17 Mediterranean, lowland, medium-Large, perennial
#   R-18 Mediterranean, mid altitude, medium-large,
#   R-19 Mediterranean, very small-small, perennial

# Through which rule did taxa enter the typical assemblages? --------------
spe_me_typical <- spr_me[B > 0.25| B > 0.20 & p_value <= 0.05 | A > 0.8]
gen_me_typical <- ger_me[B > 0.5 | B > 0.33 & p_value <= 0.05 | A > .95]

typical_me[str_detect(taxon, "\\."), taxon_lvl := "species"]
typical_me[is.na(taxon_lvl), taxon_lvl := "genus"]

typical_me[taxon_lvl == "species" & A > 0.80 &  B >= 0.25 &              p_value <= 0.05,   rule := "All"]
typical_me[taxon_lvl == "species" & A > 0.80 &  B >= 0.25 &              p_value >  0.05,   rule := "A & B"]
typical_me[taxon_lvl == "species" & A > 0.80 &  B >= 0.20 & B < 0.25  &  p_value <= 0.05,   rule := "A & C"]
typical_me[taxon_lvl == "species" & A > 0.80 & (B <  0.25 | (B >= 0.2 &  p_value >  0.05)), rule := "A"]
typical_me[taxon_lvl == "species" & A < 0.80 &  B >= 0.25             &  p_value <= 0.05,   rule := "B & C"]
typical_me[taxon_lvl == "species" & A < 0.80 &  B >= 0.25             &  p_value >  0.05,   rule := "B"]
typical_me[taxon_lvl == "species" & A < 0.80 &  B >= 0.2  & B < 0.25  &  p_value <= 0.05,   rule := "C"]
typical_me[taxon_lvl == "genus"   & A > 0.95 &  B >= 0.50 &              p_value <= 0.05,   rule := "All"]
typical_me[taxon_lvl == "genus"   & A > 0.95 &  B >= 0.50 &              p_value >  0.05,   rule := "A & B"]
typical_me[taxon_lvl == "genus"   & A > 0.95 &  B >= 0.33 & B < 0.50  &  p_value <= 0.05,   rule := "A & C"]
typical_me[taxon_lvl == "genus"   & A > 0.95 & (B <  0.50 | (B >= 0.33 & p_value >  0.05)), rule := "A"]
typical_me[taxon_lvl == "genus"   & A < 0.95 &  B >= 0.50             &  p_value <= 0.05,   rule := "B & C"]
typical_me[taxon_lvl == "genus"   & A < 0.95 &  B >= 0.50             &  p_value >  0.05,   rule := "B"]
typical_me[taxon_lvl == "genus"   & A < 0.95 &  B >= 0.33  & B < 0.50  & p_value <= 0.05,   rule := "C"]

## -- quality - NAs? -- ## 
if (sum(is.na(typical_me$rule)) != 0) typical_me[is.na(rule)]
## --                -- ## 

typical_me %>% 
    ggplot(aes(x = river_type, fill = rule)) + 
    geom_bar(position = "dodge")
    


redundant(19)



map2(.x = c(1,2,3,4,5,6,8,9,12,16,17,18,19), .y = c(4,1,2,2,2,12,9,8,6,19,19,17,17), .f = redundant)

typical_me %>% 
    unique(by = "river_type") %>% 
    ggplot(aes(x = river_type, y = redundancy)) + 
    geom_col(aes(fill = river_type)) + 
    scale_fill_manual(values = my_color_palette) +
    theme(legend.position = "none") + 
    ylab("Redundancy") + 
    xlab("River Type") + 
    geom_hline(yintercept = 75, col = "red", size = 1) + 
    ylim(0,100)

# Actual lists  -----------------------------------------------------------
bty[group == "19", taxon] %>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()





## -- old stuff -- ## 
# # - how does the number of taxa and stream types change along a gradient of endemicity? 
# endemic_levels <- seq(from = 1, to = 0.66, length.out = 50)
# n_taxa <- c()
# n_types <- c()
# for (i in 1:50) {
#     lvl <- endemic_levels[i]
#     loop_data <- typical_go[A >= lvl]
#     n_taxa[i] <- nrow(loop_data)
#     n_types[i] <- length(unique(loop_data$group))
#     
# }
# endemic_table <- data.table(level = endemic_levels, n_taxa, n_types)
# endemic_table %>% 
#         ggplot(aes(x = level,
#                    y = n_taxa,
#                    col = n_types)) +
#         geom_line() + 
#         scale_color_viridis() + 
#         ggtitle("Number of Endemic taxa and stream types with cutoff point")
# 
# # - same thing for species with p<0.05 
# for (i in 1:50) {
#     lvl <- endemic_levels[i]
#     loop_data <- typical_go[A >= lvl & p_value <= 0.05]
#     n_taxa[i] <- nrow(loop_data)
#     n_types[i] <- length(unique(loop_data$group))
#     
# }
# endemic_table <- data.table(level = endemic_levels, n_taxa, n_types)
# endemic_table %>% 
#     ggplot(aes(x = level,
#                y = n_taxa,
#                col = n_types)) +
#     geom_line() + 
#     scale_color_viridis() + 
#     ggtitle("Number of Endemic taxa and stream types with cutoff point")
# 
# ## --> no differnce at all
# 
# # - how does the number of taxa and stream types change along a gradient of ubiquiticity? 
# common_levels <- seq(from = 1, to = 0.66, length.out = 50)
# n_taxa <- c()
# n_types <- c()
# for (i in 1:50) {
#         lvl <- common_levels[i]
#         loop_data <- typical_go[B >= lvl]
#         n_taxa[i] <- nrow(loop_data)
#         n_types[i] <- length(unique(loop_data$group))
#         
# }
# common_table <- data.table(level = endemic_levels,
#                             n_taxa, 
#                             n_types)
# common_table %>% 
#         ggplot(aes(x = level,
#                    y = n_taxa,
#                    col = n_types)) +
#         geom_line() + 
#         scale_color_viridis() + 
#         ggtitle("Number of Common taxa and stream types with cutoff point")
# 
# # - same thing for taxa with p <= 0.05 
# for (i in 1:50) {
#     lvl <- common_levels[i]
#     loop_data <- typical_go[B >= lvl & p_value <= 0.05]
#     n_taxa[i] <- nrow(loop_data)
#     n_types[i] <- length(unique(loop_data$group))
#     
# }
# common_table <- data.table(level = endemic_levels,
#                            n_taxa, 
#                            n_types)
# common_table %>% 
#     ggplot(aes(x = level,
#                y = n_taxa,
#                col = n_types)) +
#     geom_line() + 
#     scale_color_viridis() + 
#     ggtitle("Number of Common taxa and stream types with cutoff point")
# 
# ## --> reduced number of species by ~ 20%. Slope is similar
# 
# # - lets have a look at the different levels of 
# # p below 0.05 
# tax050_005 <- typical_go[p_value <= 0.05]
# table(tax050_005$group)
# # p below 0.01
# tax050_001 <- typical_go[p_value <= 0.01]
# table(tax050_001$group)
# # minimal p_vale (0.005)
# min(typical_go$p_value)
# tax050_min <- typical_go[p_value <= 0.0001]
# table(tax050_min$group)
# 
# hist(tax050_min$A)
# hist(tax050_min$B)
# 
# # ubiquitos taxa  ---------------------------------------------------------
# ubi_08 <- typical_go[B >= 0.8]
# ubi_07 <- typical_go[B >= 0.7]
# ubi_06 <- typical_go[B >= 0.6]
# 
# par(mfrow = c(2,2))
# table(ubi_08$taxon) %>% hist(main = "08", breaks = length(unique(ubi_08$taxon)))
# table(ubi_07$taxon) %>% hist(main = "07", breaks = length(unique(ubi_08$taxon)))
# table(ubi_06$taxon) %>% hist(main = "06", breaks = length(unique(ubi_08$taxon)))
# 
# table(ubi_08$taxon) %>% mean()
# table(ubi_07$taxon) %>% mean()
# table(ubi_06$taxon) %>% mean()
# 
# # Indicators --------------------------------------------------------------
# ind_05 <- typical_go[B >= 0.5 & p_value <= 0.01]
# ind_04 <- typical_go[B >= 0.4 & p_value <= 0.01]
# ind_03 <- typical_go[B >= 0.3 & p_value <= 0.01]
# 
# par(mfrow = c(2,2))
# 
# tax_sum <- summary(typical_go$B)
# hist(typical_go$B, breaks = 100)
# # max 
# abline(v = tax_sum[6], col = "red")
# # 3rd Quartile 
# abline(v = tax_sum[5], col = "blue")
# # mean 
# abline(v = tax_sum[4], col = "purple")
# quantile(typical_go$B, c(.999)) 
# quantile(typical_go$A, c(.999)) 
# 
# # Endemic  ----------------------------------------------------------------
# end_95 <-  typical_go[A >= quantile(typical_go$A, c(.99))]
# # ubiquitos ---------------------------------------------------------
# ubi_95 <-  typical_go[B >= quantile(typical_go$B, c(.99))]
# 
# typical <- rbind(end_95, ubi_95)
# table(typical$group)
# 
# ## --- RULES 
# # RULE all B > 0.66 | (B > 0.33 & p_value < 0.05) | (A > 0.9 & p_value < 0.05)
# 
# typical <- tax050[B > 0.66 | (B > 0.33 & p_value < 0.05) | (A > 0.9 & p_value < 0.05)]
# table(typical$group)
# table(typical$taxon) %>% sort
# table(typical$taxon) %>% hist()
# 
# # save results to file  ---------------------------------------------------
# saveRDS(typical, paste0("007_",Sys.Date(),"_typical_mzb_communities.RDS"))
