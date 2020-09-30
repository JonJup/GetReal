#########################################
#### ---- Single geology column ---- ####
#########################################

# date: 10.09.19

# Originally, my geology layer had three variables: siliceous, sediments, and
# calcareous. Given that siliceous and calcareous are strongly negatively
# correlated I decided to reduce the data set to just one variable "dominant
# geology" which gives the most prevalent geology in the catchment.



# Setup -------------------------------------------------------------------

pacman::p_load(data.table, 
               dplyr,
               sf)

ihme = readRDS("03_data_processed/final_ihme_geology.RDS")
names(ihme)
which(ihme[1,] == max(ihme[1,.SD, .SDcols = 2:4]))

for (i in seq_len(nrow(ihme))) {
        if (ihme[i,2] == "NaN") next()
        ihme$dominant_geology[i] = names(ihme)[which(ihme[i,] == max(ihme[i, .SD, .SDcols = 2:4]))]
        print(i)
        
} 

saveRDS(ihme, "final_ihme_dg.RDS")
