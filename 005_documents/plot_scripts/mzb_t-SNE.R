# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### -----  Macroinvertebrates -------- ###
### ------------- t-SNE  ------------- ###
# -------------------------------------- #

# 09.11.20
# GetReal
# Working Package 2 
# Diatoms

if(!require(pacman))install.packages("pacman")
p_load(data.table,
       dplyr,
       ggplot2,
       ggrepel,
       here, 
       magrittr,
       Rtsne)

dir_rs = here("002_working_package_02/001_community_data/002_combined/002_invertebrates/002_r_scripts/")
call_ta_setup = file.path(dir_rs, "09_a_setup_ta_analysis.R")
source(call_ta_setup)

my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")

dfu <- copy(dt_bty)
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
                      perplexity       = 1, 
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
        theme(legend.position = "none")  +
        ggtitle("t-SNE of typical macroinvertebrate assemblages")
