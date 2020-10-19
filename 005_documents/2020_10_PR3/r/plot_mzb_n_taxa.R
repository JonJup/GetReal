### ------------------------------------------------------------------------- ###
### -- script for number of taxa per river type plot - macroinvertebrates --- ###
### ------------------------------------------------------------------------- ###

# date written : 02.10.20 
# date changed : 
# date used    : 02.10.20

pacman::p_load(dplyr, magrittr, data.table,  stringr, purrr, ggplot2, here, viridis, ggrepel)
setwd("~/01_Uni/02_getreal/002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/")

#my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")

spe <- readRDS("007_2020-09-14_indicator_spe_reduntant_1.RDS")
gen <- readRDS("007_2020-09-14_indicator_gen_reduntant_1.RDS")
foh <- readRDS("007_2020-09-14_indicator_foh_reduntant_1.RDS")

spe_t <- spe[B > 0.25 | B > 0.20 & p_value <= 0.05 | A > 0.80]
gen_t <- gen[B > 0.50 | B > 0.33 & p_value <= 0.05 | A > 0.95]
foh_t <- foh[B > 0.95 | B > 0.80 & p_value <= 0.01 | A > 0.99]

spe_t <- spe_t[taxon != "Notonectidae"]

tm <- rbindlist(list(spe_t, 
                     gen_t, 
                     foh_t))

rt_vector <- unique(tm$group)
n_types   <- length(rt_vector)


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

acp <- acp[!is.na(value),]

mzb_per_level_plot <-
                ggplot(data = acp, aes(x = river_type, y = value)) +
                geom_point(aes(col = variable), size = 3) +
                #scale_color_manual(values = my_color_palette[c(1,2,4)]) +
                ylab("number of taxa") +
                xlab("river type") + 
                labs(col = "taxon level") + 
                ggtitle("Macro-invertebrates")

