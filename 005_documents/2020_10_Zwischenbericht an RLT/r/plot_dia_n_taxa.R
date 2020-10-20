### -------------------------------------------------------------- ###
### -- script for number of taxa per river type plot - diatoms --- ###
### -------------------------------------------------------------- ###

# date written : 02.10.20 
# date changed : 
# date used    : 02.10.20

pacman::p_load(dplyr, magrittr, data.table, stringr, ggplot2, ggthemr)

setwd("~/01_Uni/02_getreal/002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/")

spe <- readRDS("010_2020-09-14_indicator_spe_medi.RDS")
gen <- readRDS("010_2020-09-14_indicator_gen_medi.RDS")
my_color_palette <- c("#7fc97f","#d95f02","#1b9e77","#666666","#bf5b17","#5f64ff","#ff9a14","#dcce00","#03eaff","#e6ab02","#66a61e","#e7298a","#7570b3","#ff00bf","#00fe04","#a6cee3","#a6761d","#386cb0","#fdc086","#beaed4")

rt_vector <- paste0("RT", c("01","02","03","04","05","06","08","09","12","16","17","18","19"))

sty <- spe[B > 0.4 | B > 0.3 & p_value <= 0.05 | A > 0.7]
gty <- gen[B > 0.8 | B > 0.6 & p_value <= 0.05 | A > .95]

bty <- rbindlist(list(sty, gty))

#rm(sty,gty,bty)
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

acp$river_type %<>% str_remove_all(pattern = "RT0")
acp$river_type %<>% str_remove_all(pattern = "RT")
acp[value == 0, value := NA]

acp <- acp[!is.na(value),]
acp$river_type%<>%factor(levels=c(1,2,3,4,5,6,8,9,12,16,17,18,19))
dia_per_level_plot <-
        ggplot(data = acp, aes(x = river_type, y = value)) +
        geom_point(aes(col = variable), size = 3) +
        scale_color_manual(values = my_color_palette) +
        ylab("taxa") +
        xlab("river type")+
        ggtitle("Diatoms")+
        theme(legend.position = "none")
