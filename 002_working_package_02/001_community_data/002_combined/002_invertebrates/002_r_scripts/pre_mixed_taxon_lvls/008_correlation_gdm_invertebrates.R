### ----------------------------------- ###
# ---- Correlation between variables ---- #
### ----------------------------------- ###

# date: 25.02.20
# part of: create typology for invertebrates 
# In this script, we inspect the correlation between the variables that should be used for clustering. 

## -- OVERVIEW -- ## 
# 1.Setup
# 2.Compute and visualize correlation
## -------------- ##

# Setup -------------------------------------------------------------------

pacman::p_load(
        dplyr,
        data.table,
        corrplot,
        psych,
        sjstats
)

setwd("~/01_Uni/03_GetReal/002_WP_02/001_Community Data/100_Combined/002_invertebrates/")


# Input data --------------------------------------------------------------

data = readRDS("003_processed_data/005_gdm2020-03-25_species.RDS")
data_old <- readRDS("../../../../001_WP_01/009_create_typology/003_data_processed/2020-02-14_base_data_for_typology.RDS")
# clean data  -------------------------------------------------------------

sum(duplicated(data_old$WSO1_ID)) 
 
data_old <- data_old[-which(duplicated(data_old$WSO1_ID))]

zero_id <- names(which(colSums(data) == 0))
data[,(zero_id) := NULL]

data_old <- data_old[,.(WSO1_ID, lgm_name, dominant_geology)]

data2 <- left_join(data, 
                   data_old,
                   by = "WSO1_ID") %>% 
        setDT

# Analyze Correlations ----------------------------------------------------

library(Hmisc)
correlations <- rcorr(as.matrix(data2[,4:25]))
library(corrgram)
corrgram(data2[,4:25])

# strahler           - distance to source x     
# area               - segment length     x   
# elev_range         - precip range       x  
# elev_range         - slope              x   
# elev_range         - slope_range       x   
# slope              - slope_range       x    
# distance to source - ar1_correlation   x     
# discharge          - acc_area         x 
# discharge          - tau2             x 
# discharge          - tau3             x 
# tau2               - tau3        x
# tau2               - acc_area x


# distance to source wins 
sum(data2$strahler); mean(data2$strahler)
sum(data2$distance_to_source); mean(data2$distance_to_source)
sum(data2$ar1_correlation); mean(data2$ar1_correlation)

# area wins 
sum(data2$area); mean(data2$area)
sum(data2$Segement_length); mean(data2$Segement_length)

# slope wins 
sum(data2$elev_range); mean(data2$elev_range)
sum(data2$precipitation_range); mean(data2$precipitation_range)
sum(data2$slope); mean(data2$slope)
sum(data2$slope_range); mean(data2$slope_range)

# tau2 wins  
sum(data2$discharge); mean(data2$discharge)
sum(data2$acc_area); mean(data2$acc_area)
sum(data2$tau2); mean(data2$tau2)
sum(data2$tau3); mean(data2$tau3)


# remove loosers 

data2[, strahler := NULL]
data2[, ar1_correlation := NULL]
data2[, Segement_length := NULL]
data2[, slope_range := NULL]
data2[, elev_range := NULL]
data2[, discharge := NULL]
data2[, tau3 := NULL]
data2[, acc_area := NULL]

# -- dominant geology 
(sqrt(summary(lm(area ~ dominant_geology               , data = data2 ))$r.squared))
(sqrt(summary(lm(elevation ~ dominant_geology          , data = data2 ))$r.squared))
(sqrt(summary(lm(slope ~ dominant_geology              , data = data2 ))$r.squared))
(sqrt(summary(lm(precipitation ~ dominant_geology      , data = data2 ))$r.squared))
(sqrt(summary(lm(precipitation_range ~ dominant_geology, data = data2 ))$r.squared))
(sqrt(summary(lm(temperature ~ dominant_geology        , data = data2 ))$r.squared))
(sqrt(summary(lm(temperature_range ~ dominant_geology  , data = data2 ))$r.squared))
(sqrt(summary(lm(distance_to_source ~ dominant_geology , data = data2 ))$r.squared))
(sqrt(summary(lm(prop_Lakes ~ dominant_geology         , data = data2 ))$r.squared))
(sqrt(summary(lm(amplitude ~ dominant_geology          , data = data2 ))$r.squared))
(sqrt(summary(lm(tau2 ~ dominant_geology               , data = data2 ))$r.squared))
(sqrt(summary(lm(tau4 ~ dominant_geology               , data = data2 ))$r.squared))
(sqrt(summary(lm(sinuosity ~ dominant_geology          , data = data2 ))$r.squared))
(sqrt(summary(lm(phase ~ dominant_geology              , data = data2 ))$r.squared))
(sqrt(summary(lm(sinuosity ~ dominant_geology          , data = data2 ))$r.squared))

# -- last glacial maximum  
(sqrt(summary(lm(area ~         lgm_name, data = data2 ))$r.squared))
(sqrt(summary(lm(elevation ~ lgm_name          , data = data2 ))$r.squared))
(sqrt(summary(lm(slope ~ lgm_name              , data = data2 ))$r.squared))
(sqrt(summary(lm(precipitation ~ lgm_name      , data = data2 ))$r.squared))
(sqrt(summary(lm(precipitation_range ~ lgm_name, data = data2 ))$r.squared))
(sqrt(summary(lm(temperature ~ lgm_name        , data = data2 ))$r.squared))
(sqrt(summary(lm(temperature_range ~ lgm_name  , data = data2 ))$r.squared))
(sqrt(summary(lm(distance_to_source ~ lgm_name , data = data2 ))$r.squared))
(sqrt(summary(lm(prop_Lakes ~ lgm_name         , data = data2 ))$r.squared))
(sqrt(summary(lm(amplitude ~ lgm_name          , data = data2 ))$r.squared))
(sqrt(summary(lm(tau2 ~ lgm_name               , data = data2 ))$r.squared))
(sqrt(summary(lm(tau4 ~ lgm_name               , data = data2 ))$r.squared))
(sqrt(summary(lm(sinuosity ~ lgm_name          , data = data2 ))$r.squared))
(sqrt(summary(lm(phase ~ lgm_name              , data = data2 ))$r.squared))
(sqrt(summary(lm(sinuosity ~ lgm_name          , data = data2 ))$r.squared))


# corr with temperature high  drop lgm name 
data2[,lgm_name := NULL]

# - Temperature vs stat at last glacial maximum 
# I decide to drop state at lgm because Temperature is a continuous variable and
# hence more variable and also scaled to importance thorugh the gdm.

saveRDS(data2, paste0(
        "003_processed_data/",
        Sys.Date(),
        "_base_data_gdm_inv_no_corr.RDS"
))
