# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### --------- Invertebrates ---------- ###
### --------- Seasons ---------------- ###
# -------------------------------------- #

# date written/ modified: 25.08.20 + 31 + 01.09
# date used: 25.08.20 + 31 + 01.09 
# Jonathan Jupke 
# Get Real WP2 
# Macroinvertebrates - Seasonality 

# setup -------------------------------------------------------------------
pacman::p_load(dplyr, magrittr, data.table, stringr, taxize, purrr, ggplot2, here, viridis, fuzzySim, vegan, ggplot2, faraway)
setwd(here("003_results"))

# load data ---------------------------------------------------------------
files <- dir()
files <- files[grep(pattern = "002_", x = files)]
for (i in seq_along(files)) {
    obj_name <- files[i]
    obj_name %<>% str_remove("[:number:]+_+[:number:]+-+[:number:]+-+[:number:]+_[:alpha:]+_") %>% 
        str_remove(".RDS$")
    assign(x = obj_name,
           value = readRDS(files[i]))
    if (length(get(obj_name)) == 0) rm(list = obj_name)
}
rm(obj_name, i, files);gc()

# create typical communities ----------------------------------------------
files <- ls()

for (i in seq_along(files)) {
    ld <- get(files[i])
    taxon_lvl <- str_extract(files[i], "^[:alpha:]")
    if (taxon_lvl == "s") B1 <- 0.25;p = 0.05; At = 0.8
    if (taxon_lvl == "g") B1 <- 0.50;p = 0.05; At = 0.95
    if (taxon_lvl == "f") B1 <- 0.95;p = 0.01; At = 0.99
        
    ld_ta <- ld[B > B1 | (B > (B1 - B1*0.25) & p_value <= p)]# | A > At]
    assign(x     = paste0("ta_", files[i]),
           value = ld_ta)
    rm(ld_ta, ld, B1, p, At, taxon_lvl, i);gc()
}
rm (list = files)
rm(files)
files <- ls()
ta01 <- rbindlist(list(         ta_g_01, ta_f_01))
ta10 <- rbindlist(list(ta_s_10, ta_g_10, ta_f_10))
ta15 <- rbindlist(list(         ta_g_10, ta_f_15))
ta18 <- rbindlist(list(         ta_g_10, ta_f_18))
rm(list = files)

#### How similar are the seasonal assemblages to each other #### 
sv        <- c("summer", "autumn")
om        <- data.frame(matrix(data = 0, ncol =  length(sv), nrow = length(sv)))
names(om) <- rownames(om) <- sv
om_l      <- list("01" = om, "10" = om, "15" = om, "18" = om)
rv        <- c("01", "10", "15", "18")


for (l in seq_along(om_l)){
    ld <- get(paste0("ta",rv[l]))
    for (i in sv) {
        ls <- ld[season == i, unique(taxon)]
        om_row <- which(row.names(om) == i)
        for (k in sv) {
            om_col <- which(names(om) == k)
            sls <- ld[season == k, unique(taxon)]
            om[om_row, om_col] <- round(sum(ls %in% sls)/length(ls) * 100,1)
            rm(om_col, sls, k);gc()
        }
        rm(om_row, ls, i);gc()
    }
    om_l[[l]] <- om
}

om01 <- om_l[[1]]
om10 <- om_l[[2]]
om15 <- om_l[[3]]
om18 <- om_l[[4]]


ta01_as   <- c("Ampullaceana balthica", "Ancylus", "Borysthenia naticina", "Caenis", "Chelicorophium", "Corbicula", "Dikerogammarus", "Esperiana esperi", "Hippolyte desmaresti","Isogenus nubecula", "Jaera", "Microcolpia daudebartii", "Obesogammarus obesus", "Potamopyrgus")
ta10_as <- c("Ancylus", "Baetis", "Chironomidae", "Ecdyonurus", "Elmis", "Ephemera", "Esolus", "Gammarus", "Hydraena", "Hydropsyche", "Hydroptila", "Leuctra", "Limnius", "Micronectidae", "Odontocerum", "Pisidium", "Rhithrogena", "Rhyacophila", "Riolus", "Serratella ignita", "Simuliidae" )
ta15_as <- c("Baetis", "Chironomidae", "Ecdyonurus", "Elmis", "Epeorus", "Hydraena", "Hydropsyche", "Isoperla", "Leuctra", "Limnephilidae", "Limoniidae", "Nemoura", "Oligochaeta", "Protonemura", "Pseudodiamesa branickii", "Rheosmittia spinicornis", "Rhithrogena", "Rhyacophila", "Serratella ignita", "Simuliidae", "Stilocladius montanus")
ta18_as <- c("Baetis", "Caenis", "Calopteryx", "Chironomidae", "Ecdyonurus", "Elmis", "Empididae", "Ephemerella", "Esolus", "Gammarus", "Hydropsyche", "Hydroptila", "Hydroscapha", "Lestidae", "Leuctra", "Limnius", "Oligochaeta", "Onychogomphus", "Oulimnius", "Potamopyrgus", "Procambarus", "Psychomyia", "Rhyacophila", "Riolus", "Serratella ignita", "Simuliidae", "Sympecma" )

for (i in 1:4) {
    file1 <- get(paste0("ta", rv[i]))
    file2 <- get(paste0("ta", rv[i], "_as"))
    out1 <- sum(unique(file1$taxon) %in% file2) / length(file2)
    out2 <- sum(unique(file1$taxon) %in% file2) / length(unique(file1$taxon))
    out1 <- round(out1 * 100, 0) 
    out2 <- round(out2 * 100, 0) 
    print(paste(rv[i], ":", out1, "|", out2, "|", length(unique(file1$taxon)), "/",length(file2)))
    rm(file1,file2, i, out1, out2 );gc()
}


# A closer Look  ----------------------------------------------------------

# RT1 
ta01[season == "summer", unique(taxon), .N]
ta01[season == "autumn", unique(taxon), .N]
sum (ta01[season == "autumn", unique(taxon)] %in% ta01[season == "summer", unique(taxon)])
both_id <- which(ta01[season == "autumn", unique(taxon)] %in% ta01[season == "summer", unique(taxon)])
ta01[season == "autumn", unique(taxon)][both_id]
ta01[, unique(taxon), .N]
ta01_as
as_id <- which(ta01[season == "autumn", unique(taxon)][both_id] %in% ta01_as)
ta01[season == "autumn", unique(taxon)][both_id][as_id]

# RT10
ta10[season == "summer", unique(taxon), .N]
ta10[season == "autumn", unique(taxon), .N]
sum (ta10[season == "autumn", unique(taxon)] %in% ta10[season == "summer", unique(taxon)])
both_id <- which(ta10[season == "autumn", unique(taxon)] %in% ta10[season == "summer", unique(taxon)])
ta10[season == "autumn"][both_id]
ta10[, unique(taxon), .N]
ta10_as
as_id <- which(ta10[season == "autumn", unique(taxon)][both_id] %in% ta10_as)
ta10[season == "autumn", unique(taxon)][both_id][as_id]
ta10[season == "autumn", unique(taxon)][both_id]%>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()

# RT15
ta15[season == "summer", unique(taxon), .N]
ta15[season == "autumn", unique(taxon), .N]
(shared <- sum (ta15[season == "autumn", unique(taxon)] %in% ta15[season == "summer", unique(taxon)]))
shared/length(unique(ta15$taxon))
both_id <- which(ta15[season == "autumn", unique(taxon)] %in% ta15[season == "summer", unique(taxon)])
ta15[season == "autumn"][both_id]
as_id <- which(ta15[season == "autumn", unique(taxon)][both_id] %in% ta15_as)
as_aut <- which(ta15[season == "autumn", unique(taxon)] %in% ta15_as)
as_sum <- which(ta15[season == "summer", unique(taxon)] %in% ta15_as)

ta15[as_aut]

ta15[season == "autumn", unique(taxon)][both_id][as_id]
ta15[season == "autumn", unique(taxon)][both_id]%>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()
ta15[, unique(taxon)] %>%
    str_replace_all(pattern = "\\.", "\\ ") %>%
    paste(sep = ",", collapse = ", ") %>%
    writeClipboard()
