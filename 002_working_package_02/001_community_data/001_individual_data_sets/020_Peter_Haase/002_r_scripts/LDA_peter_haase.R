## LDA with JJM 
pacman::p_load(sf, fuzzySim, dplyr, data.table, Rcpp, ggplot2, ggrepel)
bio1 = st_read("~/01_Uni/03_GetReal/02_WP_02/Community Data/20_Peter_Haase/03_FinalData/06_191129_MZB_peter_haase.gpkg")
bio2 = st_drop_geometry(bio1)

# for species 
bio2 = filter(bio2, !(is.na(species)))


bio2 = arrange(bio2, site_id, date_id)

# call for species 
names(bio2)
bio3 = splist2presabs(bio2, sites.col = 1, sp.col = 8)


# any sites less than five taxa? 
any(apply(bio3[,-1],1,sum) < 5)
# # any taxa less than 5 occurences? 
any(apply(bio3[,-1], 2, sum) < 5)
apply(bio3[,-1], 2, sum) %>% sort()

"NA." %in% names(bio3)
bio3 = bio3[!(names(bio3) %in% "NA.")]

# get functions 
source(file = '~/01_Uni/03_GetReal/02_WP_02/Clustering/LDA_PA/gibbs functions.R')
source(file = '~/01_Uni/03_GetReal/02_WP_02/Clustering/LDA_PA/gibbs sampler main function.R')
sourceCpp('~/01_Uni/03_GetReal/02_WP_02/Clustering/LDA_PA/aux1.cpp')

# run Gibbs Sampler 
# loc.id tells the sampler how many samples where taken at which id
nloc = length(unique(bio2$gr_sample_id)) 
loc.id = 1:nloc
n.group = 10
start.time = Sys.time()
results = lda.presence.absence(dat = bio3[,-1], id = loc.id, ncomm = n.group, a.phi = 1, b.phi = 1, gamma = 0.1, ngibbs = 4000)
run.time = Sys.time() - start.time
# plot results - look for a burnin period  
par(mfrow = c(2,1), mar = c(3,3,1,1))
plot(results$llk, type = "l", xlab = "iterations", ylab = "Log_likelihood")
seq = 300:3e3
plot(y = results$llk[seq], x = seq, type = "l")
# burnin after 500 

# summarize 
theta = matrix(colMeans(results$theta[seq,]),nloc, n.group)
phi   = matrix(colMeans(results$phi[seq,]), n.group, (ncol(bio3) - 1))

par(mfrow=c(3,1),mar=c(4,4,4,1)) 
boxplot(theta, ylab=expression(theta[lk]), names=paste('G',1:n.group,sep=''),xlab='Groups')

plot(NA,NA,xlim=c(0,105),ylim=c(0,1),xlab='Locations',ylab=expression(theta[lk]))
for(i in 1:ncol(theta.true)) lines(theta.true[,i],col='grey',lwd=2)
for(i in 1:10)lines(theta[,i],col=i)

plot(phi[,1], type = "p", ylim = c(0,1))
for (i in 1:ncol(phi)) points(phi[,i], col = i)

bio5 = left_join(bio1, 
                tibble(site = unique(bio1$site),
                       group = max.col(theta)),
                by = "site")
        
setDT(bio5)
bio51 = unique(bio5, by = "site")
bio52 = st_as_sf(bio51)
        
library(tmap)
tmap_mode("view")
tm_shape(bio52) + tm_dots(col = "group", palette = "Dark2",  style = "fixed",
                          breaks = c(1, 2, 3,4))

## -- what species drive the groups? 
phi.max = group = species.name = c()
for (i in 1:ncol(phi)) {
        phi.species = sort(phi[,i], decr = T)
        phi.max[i] = phi.species[1] - phi.species[2] - phi.species[3]
        group[i] = which(phi[,i] == max(phi[,i]))
        species.name[i] = names(bio3)[i + 1]
}
Indicator_species = tibble(phi.max, group, species.name)

Indicator_species = arrange(Indicator_species, desc(phi.max))

# find ranges for top 5 species in each group 
for (i in 1:n.group) {
        assign(
                paste0("range",i),
                Indicator_species %>% 
                        filter(group == i) %>% 
                        select(phi.max) %>% 
                        slice(5) %>% 
                        as.numeric
        )
}


Indicator_species %>%
ggplot(
        aes(x = 1:nrow(Indicator_species),
            y = phi.max)
        ) + 
        geom_point(aes(fill = group), pch = 21) +
        geom_label_repel(data = subset(Indicator_species, group == 1 & phi.max >= range1),
                  aes(100, y = phi.max, label = species.name)) +
        geom_label_repel(data = subset(Indicator_species, group == 2 & phi.max >= range2),
                  aes(100, y = phi.max, label = species.name)) +
        geom_label_repel(data = subset(Indicator_species, group == 3 & phi.max >= range3),
                  aes(100, y = phi.max, label = species.name)) +
        facet_wrap(. ~ group)

best = head(test3, 9)
phi[,which(test2 %in% best)]
names(bio3)[which(test2 %in% head(test3, 3))]

