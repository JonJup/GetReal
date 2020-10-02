#######################################################
#### ---- Download Landau-Germany from Server ---- #### 
#######################################################

## GR WP2
## date: 07.03.19; 20.03.19; 30.07.19

# Setup -------------------------------------------------------------------
pacman::p_load(tidyverse,
               sf,
               data.table, 
               RPostgreSQL)

wd <- rstudioapi::getActiveDocumentContext() %>% .$path %>% dirname()
setwd(wd)



# Download files  ---------------------------------------------------------
pw <- {"9720b78fd7"}

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, 
                 dbname = "bfg_monitoring",
                 host = "139.14.20.252", 
                 port = 5432,
                 user = "jupke", 
                 password = pw)

## list all tables 
dbListTables(con)
test = dbGetQuery(con, "SELECT * FROM dgm")


diatom_sites = dbGetQuery(con, "SELECT * FROM dia.dia_sites")
diatom_samples = dbGetQuery(con, "SELECT * FROM dia.dia_samples")

mzb_samples <- dbGetQuery(con, "SELECT * FROM mzb.mzb_samples")
mzb_sites <- dbGetQuery(con, "SELECT * FROM mzb.mzb_sites")

# save to file -----------------------------------------------


setDT(diatom_samples)
fwrite(diatom_samples, file = "diatom_samples.csv")
setDT(diatom_sites)
fwrite(diatom_sites, file = "diatom_sites.csv")
setDT(mzb_samples)
fwrite(mzb_samples, file = "mzb_samples.csv")
setDT(mzb_sites)
fwrite(mzb_sites, file = "mzb_sites.csv")