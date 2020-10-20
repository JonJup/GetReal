### --------------------------------- ###
### -- script for uniqueness plot --- ###
### -- presentation  
###    Get Real 
###    Progress Review 3 ------------ ###
### --------------------------------- ###

# date written : 19.10.20 
# date changed : 
# date used    : 19.10.20

## -- macro-invertebrates -- ## 
utmm <- sort(unique(tm$taxon))
# compute a "uniqueness score". Each taxon gets a score based on the number of
# TAs its part of. The score is 1 for taxa that only occur in one TA and 1/4 in
# those that occur in four. The score of a stream type is the mean score of its
# taxa.
for (i in seq_along(utmm)) {
        tm[taxon == utmm[i], score := 1/tm[taxon == utmm[i], .N]]
}
for (i in seq_along(unique(tm$group))) {
        tm[group == unique(tm$group)[i], group_score := sum(score)/.N]
}

tm$group %<>% str_remove_all(pattern = "RT")
tm$group %<>% factor(levels = c("1", "2_3", "4_5", "8_9", "10_11", "12","14", "15_16", "18"))

plot_uni_mzb <-
                ggplot(data = tm, aes(x = group, y = group_score))  +
                geom_point(size = 4) +
                ylim(c(0, 1)) +
                geom_hline(
                        col = "white",
                        yintercept = 1 / length(unique(tm$group)),
                        linetype = 2,
                        size = 2
                ) +
                ylab("Uniqueness score") +
                xlab("River Type")
