##### ------------------------------------------- #####
# ----- Exctract threshold values from PUP data ----- #
##### ------------------------------------------- #####

#date: 12.12.19 
#GR:WP2 

#The dataset from Philippe Usseglio Polaterra provides actual concentrations (as
#continuous variable) together with related (though not equal) judgmentes on
#water quality (as ordinal variables). Because of this combination I can use it
#to derive threshold values that I can apply to other data sets.

pacman::p_load(dplyr, ggplot2)

#read in data 
pup.data = readxl::read_excel("../16_PhillipeUsseglioPolterra/01_OriginalData/data_invertebrate_GETREAL project-final version.xlsx",
                              skip = 4) %>% 
        select(1:32) 
names(pup2)
pup2 <- select(pup.data, c(1, 15:29)) %>%
        rename(
                site = `List No`,
                OM = `Organic Matter`,
                N_tot = `Nitrogen compounds (except nitrates)`,
                NH_four = `NH4+ (mg/l NH4)`,
                NO_two = `NO2- (mg/l NO2)`,
                NO_qual = Nitrates,
                NO_quant = `Nitrates (mg/l NO3)`,
                P_tot = `Phosphorous compounds`,
                P_quant = `Total Phosphorous (mg/l P)`,
                SM = `Suspended Matter`
        ) 
# factors
pup2$N_tot <- factor(pup2$N_tot, levels = c("High","Good", "Intermediate", "Poor", "Bad"))
pup2$P_tot <- factor(pup2$P_tot, levels = c("High","Good", "Intermediate", "Poor", "Bad"))
pup2$NO_qual <- factor(pup2$NO_qual, levels = c("High","Good", "Intermediate", "Poor", "Bad"))

# Nitrogen
pup2 %>% 
        group_by(N_tot) %>% 
        summarise(nh4_min = min(NH_four),
                  nh4mean = mean(NH_four), 
                  nh4max = max(NH_four),
                  n02_min = min(NO_two),
                  no2mean = mean(NO_two), 
                  no2max = max(NO_two))

pup2 %>% 
        ggplot(aes(x = NH_four, y = NO_two, fill = N_tot)) + 
        geom_point(alpha = 0.5, shape = 21)

pup2 %>% 
        ggplot(aes(x = NH_four, y = NO_two, fill = Pesticides)) + 
        geom_point(alpha = 0.5, shape = 21)

# Phosphor 
pup2 %>% 
        group_by(P_tot) %>% 
        summarise(min = min(P_quant),
                  mean = mean(P_quant),
                  max = max(P_quant)) -> Threshold_P

# Nitrates
pup2 %>% 
        group_by(NO_qual) %>% 
        summarise(min = min(NO_quant),
                  mean = mean(NO_quant),
                  max = max(NO_quant)) -> Threshold_NO

                  