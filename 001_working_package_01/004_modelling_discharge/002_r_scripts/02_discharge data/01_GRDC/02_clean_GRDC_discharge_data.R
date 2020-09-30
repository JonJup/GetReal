# --------------------------------- #
### --- Clean discharage data --- ###
# --------------------------------- #

# WP1 GR 
# 17 + 22.05.19

#Preapare the discharge data for modelling with accumulated precipitation 


# setup -------------------------------------------------------------------

      pacman::p_load(data.table, here)
      
      setwd(here("03_Data_Processed/01_Discharge/"))
      discharge <- fread("discharge_complete2.csv") 

# Cleaning ----------------------------------------------------------------
      
      # remove Norway 
      norwayID = which(discharge$Country == "NO")
      discharge = discharge[-norwayID]
      
      # seperate data column into three columns
      discharge[,c("Year","Month","Day") := tstrsplit(`YYYY-MM-DD`, "-")]
      
      # how many stations have less than 10 years  
      no.ten = which(discharge$Numer_Of_Year < 10)
      # -> in total 3715 observation are deleted because they have less than 10 consecutive years of data 
      # -> they stem from three different stations all from Germany
      # -> All them just started in 2010, 2014, 2016 
      discharge = discharge[-no.ten]
      

# Save to file  -----------------------------------------------------------

readr::write_csv(discharge, "discharge_work.csv")
      