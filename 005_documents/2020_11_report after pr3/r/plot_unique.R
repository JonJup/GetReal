### --------------------------------- ###
### -- script for uniqueness plot --- ###
### --------------------------------- ###

# date written : 02.10.20 
# date changed : 20.10.20
# date used    : 02.10.20

## -- diatoms -- ## 
utmd <- sort(unique(bty$taxon))
for (i in seq_along(utmd)) {
        bty[taxon == utmd[i], score := 1/bty[taxon == utmd[i], .N]]
}
for (i in seq_along(unique(bty$group))) {
        bty[group == unique(bty$group)[i], group_score := sum(score)/.N]
}

bty$group %<>% str_remove_all(pattern = "RT0")
bty$group %<>% str_remove_all(pattern = "RT")
bty$group%<>%factor(levels=c(1,2,3,4,5,6,8,9,12,16,17,18,19))
plot_uni_dia <-
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
                geom_point(size = 2) +
                ylim(c(0, 1)) +
                geom_hline(
                        col = "red",
                        yintercept = 1 / length(unique(tm$group)),
                        linetype = 2
                ) +
                ylab("Uniqueness score") +
                xlab("River Type")
