## -- River Discharge Database creation -- ##

## 28.03.19
## GR WP1


# Setup -------------------------------------------------------------------
## load packags 
pacman::p_load(dplyr, 
               readr, 
               stringr, 
               tidyr,
               here,
               foreach, 
               parallel, 
               doParallel,
               beepr,
               data.table)


## define and set wds 

setwd(here("01_Data_Raw/01_River Discharge data/02_RawData/ "))

# Reading files in --------------------------------------------------------


# all files in folder 
files <- fs::dir_ls()

# we only care for the files with "day" in their name 
day.files <- subset(files, str_detect(files, "Day"))

# read all files, extract relevant information 
temp.list <- list()
max.k <- length(day.files) 

# start cluster 
n.cores <- detectCores()
cl <- makeCluster(n.cores - 2)
clusterExport(cl, c("day.files","temp.list","max.k"))
# clusterEvalQ(cl, function() library(dplyr))
# clusterEvalQ(cl, function() library(readr))
# clusterEvalQ(cl, function() library(tidyr))
registerDoParallel(cl)




out <- foreach(k = 1:max.k) %dopar% {
        

        # read txt file 
        y <- readr::read_delim(day.files[k],
                        delim = "#",
                        n_max = 35,
                        trim_ws = T)[, 2]
        
        # rename column so it can be easily referred to 
        names(y) <- "data"
        
        # Safe the variables with character values to objects
        for (j in c("River", "Station", "Time", "Country")) {
                assign(
                        x = j,
                        value =  stringr::str_trim(
                                stringr::str_extract(
                                                dplyr::filter(y, stringr::str_detect(data, pattern = j)),
                                        " +.*")
                        )
                )
                
        }
        
        # Safe variables with numeric values to objects
        for (i in c("GRDC-No", "Latitude", "Catchment", "Altitude")) {
                assign(
                        x = i,
                        value = as.numeric(
                                stringr::str_extract(
                                                dplyr::filter(y, stringr::str_detect(data, i)),
                                                "[0-9]+.*")
                                )               
                )
                
        }
        
        assign(x = "Longitude",
               value = as.numeric(
                       stringr::str_extract(
                               dplyr::filter(y,
                                             stringr::str_detect(data, "Longitude")),
                               "[:punct:]*+[0-9]+.*"
                       )
               ))
        
        
        # Due to its name Number of years can not be as easily extracte as the
        # other variables. Therefore I do this seperatly here.
        NoOfYears <-  as.numeric(stringr::str_extract(dplyr::filter(y, stringr::str_detect(data, "No. of")),"[0-9]+.*"))
        
        # The time variable still looks awfull. Here I remove all unnecessary
        # information and split it in two objelts: the start and end date of
        # measurements.
       
        time_split = stringr::str_split(
                stringr::str_trim(
                        stringr::str_extract(
                                Time,
                                " .*[1-9]*-[0-9]{2} - [0-9]*-[0-9]{2}")
                        ), 
                pattern = " - ") 

                
        
        Begin_Date <- zoo::as.Date(zoo::as.yearmon(time_split[[1]][1], "%Y-%m"))
        End_Date <- zoo::as.Date(zoo::as.yearmon(time_split[[1]][2], "%Y-%m"))
        
        # Here I read in the second part of the txt file. This contains the
        # actual measurement values.
        y2 <- readr::read_delim(day.files[k],
                         delim = ";",
                         skip = 35,
                         trim_ws = T)
        # Compile objects into a tibble with named variables which we can join
        # with the y2 tbl.
        y_stats <- dplyr::tibble(
                GRDC_Number = rep(`GRDC-No`, times = nrow(y2)),
                Latitude    = rep(Latitude, times = nrow(y2)),
                Longitude   = rep(Longitude, times = nrow(y2)),
                Numer_Of_Year = rep(NoOfYears, times = nrow(y2)),
                Altitude      = rep(Altitude, times = nrow(y2)),
                River_Name    = rep(River, times = nrow(y2)),
                Country       = rep(Country, times = nrow(y2)),
                Station_Name  = rep(Station, times = nrow(y2)),
                Begin_Date    = rep(Begin_Date, times = nrow(y2)),
                End_Date      = rep(End_Date,times = nrow(y2))
        )
        
        # join tables 
        y3 <- dplyr::bind_cols(y2,y_stats)
        # -999 signifies NAs. 
        y3[which(y3$Value == -999),"Value"] <- NA     
        
        y3
        
};beep()
stopCluster(cl)
# save list to file in case computer crashes again ... 
save(out, file = "dischage_list.RData")

# load list if you started here 
# setwd(wd1)
# load("dischage_list.RData")

out2 <- rbindlist(out)

# Not tried yet 
fwrite(x = out2, file = "../../../03_Data_Processed/01_Discharge/discharge_complete2.csv")




