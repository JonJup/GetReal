#####################################
### --- Combine GRDC and NRFA --- ### 
#####################################

# 11.07.19
# In this script I combine the GRDC and NRFA working data. 

# Setup -------------------------------------------------------------------

pacman::p_load(sf, dplyr, data.table, magrittr)

setwd(here::here())

grdc = readRDS("03_Data_Processed/01_Discharge/GRDC/19-07-11_grdc_work_xnrfa.RDS")
nrfa = readRDS("03_Data_Processed/01_Discharge/NRFA/nrfa_work.RDS")

grdc %<>% setDT
nrfa %<>% setDT



# Clean -------------------------------------------------------------------
grdc[, dataset := "grdc"]
nrfa[, dataset := "nrfa"]

# drop columns
grdc[, c("YYYY-MM-DD", "hh:mm", "Numer_Of_Year", "Altitude", "Country", "Begin_Date", "End_Date") := NULL]

# rename and reorder
grdc2 = grdc[,.(ID = GRDC_Number, discharge = Value, Latitude, Longitude, river = River_Name, station = Station_Name, Year, Month, Day, dataset)]
nrfa2 = nrfa[, .(ID, discharge = Value, Latitude, Longitude, river = Rivername, station = Stationname, Year, Month, Day, dataset)]

# row bind tables 
combo = rbindlist(list(grdc2, nrfa2))

# create new ID column and drop old data set column 
combo[, ID := paste0(dataset,"_",ID)]
combo[, dataset := NULL]

# Save to file  -----------------------------------------------------------
saveRDS(combo, "03_Data_Processed/19-07-11_discharge_combo.RDS")

