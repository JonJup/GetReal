#####################################
#### ---- magnificent Seven ---- ####
#####################################

# 13.08.19 
# With "Prediction function" I created RDS files. Each file holds the
# predicted discharge for all catchments for one month. To calculate the
# statistics from Archfield et al 2013 I need to change the format into one
# segment with all days.






# Setup -------------------------------------------------------------------

pacman::p_load(
      magrittr,
      data.table,
      EflowStats,
      tidyr,
      dplyr
)

setwd(here::here())

# Fix functions -----------------------------------------------------------

# unfortunately the calc_magnifSeven function in EFlowStats are messed up here I provied fixed versions 
calc_magnifSeven = function(x, yearType = "water", digits = 3) 
{
      x <- validate_data(x, yearType)
      x$month_val <- lubridate::month(x$date)
      complmom <- lmom::.samlmu(x$discharge)
      complmom[2] = complmom[2]/complmom[1]
      complmom = round(complmom, digits = 2)
      lam1 <- complmom[1]
      tau2 <- complmom[2]
      tau3 <- complmom[3]
      tau4 <- complmom[4]
      calc_ar1v <- calc_ar1(x, yearType = yearType, digits = digits)
      get_seasonality_vars <- get_seasonality(x, yearType = yearType)
      amplitude <- get_seasonality_vars[1]
      phase <- get_seasonality_vars[2]
      magnif7 <- data.frame(indice = c("lam1", "tau2", "tau3", 
                                       "tau4", "ar1", "amplitude", "phase"), statistic = c(lam1, 
                                                                                           tau2, tau3, tau4, calc_ar1v, amplitude, phase), stringsAsFactors = F)
      magnif7$statistic <- round(magnif7$statistic, digits = digits)
      return(magnif7)
}



# Calculations  -----------------------------------------------------------

files = fs::dir_ls("03_Data_Processed/04_Modelled_Discharge/02_Predicted_Discharge/") 
files = files[-length(files)]
f.list = list()

n.files = length(files)

for (i in 1:n.files) {
      
      f.list[[i]] = readRDS(files[i])
      
};beepr::beep()      
       
u.catch = unique(f.list[[1]]$WSO1_ID)
n.catch = length(u.catch)
#final = list()
final = readRDS("116315_final.RDS")
u.catch = u.catch[-c(1:116315)]
counter = 116316

# only relevant for second run with NAs
pre.list = list()

for (catch_loop in 1:length(ids)) {
      for (file_loop in 1:n.files) {
          if (file_loop == 1) dis.vec = c()   
          if (file_loop == 1) date.vec = c()   
          
            dis.vec = append(dis.vec, 
                   f.list[[file_loop]][WSO1_ID == ids[catch_loop]] %>% slice() %>% .[-1] %>% as.numeric)
            date.vec = append(date.vec, 
                              f.list[[file_loop]][WSO1_ID == ids[catch_loop]] %>% .[,-1] %>%  names)
      }
      if (sum(is.na(dis.vec)) == length(dis.vec)) {
         final[[catch_loop]] = "Only NAs"
         print(paste(catch_loop, "only NAs"))
         
         
      } else{
      #fix negative entries 
      dis.vec[which(dis.vec < 0)] = 0
      prep.date = lubridate::ymd(date.vec)
      prep.eflow = data.table(
                   date = prep.date,
                  discharge = dis.vec)
      MS = calc_magnifSeven(prep.eflow, yearType = "calendar", digits = 3) %>%
            spread(indice, statistic)
      MS[,"WSO1_ID"] =  ids[catch_loop]
      #for full run 
      #final[[catch_loop]] = MS
      # for second run with NAs
      pre.list[[catch_loop]] = MS
      
      print(catch_loop)
      }
}  




# Save and restart  -------------------------------------------------------

#50297
final
saveRDS(final,"116315_final.RDS")

test = rbindlist(final)

car::spm(test)
test[,-8] %>%
      scale %>%
      kmeans(centers = 4) -> fit
groupID <- fit$cluster %>% tibble::enframe(name = c("CLUSTER")) %>% .[,2]
clust1 = bind_cols(test, groupID)
fit$centers %>% vegan::rda() %>% biplot
plot(clust1$amplitude, col = clust1$value, pch = 21)
plot(clust1$ar1, col = clust1$value)
plot(clust1$lam1, col = clust1$value)
plot(clust1$tau4, col = clust1$value)
plot(clust1$tau3, col = clust1$value)
plot(clust1$phase, col = clust1$value)

