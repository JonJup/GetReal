### ----------------------------------- ###
# ---- Correlation between variables ---- #
### ----------------------------------- ###

# date: 17.09.19
# part of: 06_create_typology project 
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

setwd("~/01_Uni/03_GetReal/01_WP_01/06_create_typology/")

# Input 
data = readRDS("03_data_processed/2020-02-14_base_data_for_typology.RDS")
data = data[lgm_name != "NA"]
data[,lgm_name := droplevels(lgm_name)]
data = data[dominant_geology != "unknown -- island"]
data[,dominant_geology := droplevels(dominant_geology)]

numerical_columns = c(3:15,17:23,26,27)

data[,c(3:4,7:10,12:15,18,20,21,24,26)] %>% 
        mixedCor()
data[,c(3:4,7:10,12:15,18,20,21,24,26)] %>% 
        names
        
        
cor_tab <- table(unlist(data[,2]), unlist(data[,17]))
cor_tab <- table(unlist(data[,2]), unlist(data[,25]))
cor_tab <- table(unlist(data[,17]), unlist(data[,25]))

cramer(cor_tab)
# strahler 
(sqrt(summary(lm(area ~ strahler, data = data ))$r.squared))
(sqrt(summary(lm(elevation ~ strahler, data = data ))$r.squared))
(sqrt(summary(lm(slope_range ~ strahler, data = data ))$r.squared))
(sqrt(summary(lm(precipitation ~ strahler, data = data ))$r.squared))
(sqrt(summary(lm(precipitation_range ~ strahler, data = data ))$r.squared))
(sqrt(summary(lm(temperature ~ strahler, data = data ))$r.squared))
# high 
(sqrt(summary(lm(distance_to_source ~ strahler, data = data ))$r.squared))
(sqrt(summary(lm(amplitude ~ strahler, data = data ))$r.squared))
(sqrt(summary(lm(phase ~ strahler, data = data ))$r.squared))
# high
(sqrt(summary(lm(discharge ~ strahler, data = data ))$r.squared))
(sqrt(summary(lm(tau4 ~ strahler, data = data ))$r.squared))
(sqrt(summary(lm(sinuosity ~ strahler, data = data ))$r.squared))

# dominant geology 
(sqrt(summary(lm(area ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(elevation ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(slope_range ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(precipitation ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(precipitation_range ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(temperature ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(distance_to_source ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(amplitude ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(phase ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(discharge ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(tau4 ~ dominant_geology, data = data ))$r.squared))
(sqrt(summary(lm(sinuosity ~ dominant_geology, data = data ))$r.squared))
        
# lgm 
(sqrt(summary(lm(area ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(elevation ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(slope_range ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(precipitation ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(precipitation_range ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(temperature ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(distance_to_source ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(amplitude ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(phase ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(discharge ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(tau4 ~ lgm_name, data = data ))$r.squared))
(sqrt(summary(lm(sinuosity ~ lgm_name, data = data ))$r.squared))



# drop correlated variables  ----------------------------------------------

data2 = data 
data2[,c("lgm_name", "strahler", "elev_range", "slope",
        "y_Coords","x_Coords","ar1_correlation", "acc_area", "tau2", "tau3", "temperature_range") := list(NULL)]

saveRDS(data2, paste0(
        "03_data_processed/",
        Sys.Date(),
        "_base_data_no_corr.RDS"
))
