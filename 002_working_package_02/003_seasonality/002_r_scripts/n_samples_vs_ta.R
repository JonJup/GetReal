##-- plot number of samples against size of TA -- ## 
library(ggplot2)

data<-data.frame(n_samples = c(55,188,103,39, 7,42,27,36, 157,52,13, 82,19,52),
           TA        = c(2,7,17,6,4,3,16,4, 28,21,22, 19,16,29),
           taxon     = c("Macroinvertebrate", "Macroinvertebrate", "Macroinvertebrate", "Macroinvertebrate",
                         "Macroinvertebrate", "Macroinvertebrate", "Macroinvertebrate", "Macroinvertebrate", 
                         "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom"))

ggplot(data, aes(x = log(n_samples), y = TA)) + 
        stat_summary(fun.data=data) + 
        geom_smooth(method='lm', formula= y~x) + 
        geom_point(aes(col = taxon)) +
        ylab("Size of Typical Assemblage") + 
        xlab("Number of samples")


mod <- lm(TA ~ n_samples, data = data)
mod2 <- lm(TA ~ log(n_samples), data = data)

mod2
summary(mod2)
