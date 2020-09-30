##################################################################
#### ---- Applying spatial corssvalidation to prediction ---- ####
##################################################################

# 12.08.19
# Predict discharge data and assess fit by spatial cv 



# Setup -------------------------------------------------------------------

pacman::p_load(mlr, 
               ggplot2,
               data.table,
               sf,
               magrittr)

setwd(here::here())
# IO 
# layer with all aggregated precipitations 
        prec = st_read(dsn = "03_Data_Processed/02_Precipitation/03_Europe_Precipitation/all_acc_prec.gpkg") %>% st_drop_geometry() %>% setDT
# dicharge ID to WSO1_ID 
        IDtoID = readRDS("03_Data_Processed/01_Discharge/combined/Stations_ID_to_WSO1ID.RDS") %>% st_drop_geometry() %>% setDT
# discharge data
        discharge = readRDS("03_Data_Processed/01_Discharge/combined/discharge_combined_and_filtered.RDS") %>% 
                setDT()

        largerivers = discharge[discharge > 1000, unique(river)]
        discharge2 = discharge[!(river %in% largerivers)]
# Cleaning ----------------------------------------------------------------

# joins 
discharge.join = discharge2[IDtoID, on = "ID"]
names(prec)[2] = "winter_1999"


# Setup Calculation -------------------------------------------------------
evaluations = 50

years = sample(2000:2013, evaluations, replace = T)
months = sample(1:12, evaluations, replace = T)
days = sample(1:28, evaluations, replace = T)

result_table = data.table(
        year = years,
        month = months,
        day = days,
        rmse = rep(0, evaluations),
        nrmse = rep(0, evaluations)

)


# Spatial CV - Loop -------------------------------------------------------


for (i in 1:evaluations) {
        
        
        dis.join.sub = discharge.join[Year == result_table$year[i] & 
                                              Month == result_table$month[i] &
                                              Day == result_table$day[i]]
        
        season = ifelse(result_table$month[i] %in% c(12,1,2), "winter",
                        ifelse(result_table$month[i] %in% c(3,4,5), "spring",
                               ifelse(result_table$month[i] %in% c(6,7,8), "summer", "autumn")))
        
        exp.var = paste0(season,"_",result_table$year[i])
        prec.sub = prec[,c("WSO1_ID", exp.var), with = F]
        dis.join = prec.sub[dis.join.sub, on = "WSO1_ID"] %>% na.omit()
        
        coords = dis.join[,list(Latitude, Longitude)]
        
        dis.join2 = dis.join[,c(2,4)]
        # mlr takes a task and a learner -- this is the task 
        task = makeRegrTask(data = dis.join2, target = "discharge", coordinates = coords)
        
        # and this the learner
        lrnr1 = makeLearner(cl = "regr.lm", 
                            predict.type = "response")
        
        
        # specify hyperparamters of CV 
        perf_level = makeResampleDesc(method = "SpRepCV", folds = 5, reps = 100)
        set.seed(012348)
        sp_cv1 = mlr::resample(learner = lrnr1, task = task,
                               resampling = perf_level, 
                               measures = rmse)
        # 
        
        result_table$rmse[i] = sp_cv1$measures.test$rmse %>% mean
        result_table$nrmse[i] = result_table$rmse[i]/max((dis.join2$discharge) - min(dis.join2$discharge))
        print(i)
        
}


# Results -----------------------------------------------------------------


result_table[, date := lubridate::ymd(paste(year, month,day,sep = "-"))]
result_table[, season :=  ifelse(month %in% c(12,1,2), "winter",
                                 ifelse(month %in% c(3,4,5), "spring",
                                        ifelse(month %in% c(6,7,8), "summer", "autumn")))  ]


mean(result_table$rmse)
mean(result_table$nrmse)

mrmse <- result_table[,.(mean(rmse)), by = season]
mnrmse <- result_table[,.(mean(nrmse)), by = season]


result_table %>% 
        ggplot(aes(x = date, y = rmse)) + 
        geom_line(aes(col = season), lwd = 1.5) +
        geom_hline(data = mrmse1, aes(col = season, yintercept = V1)) +
        facet_grid(.~season)

result_table %>% 
        ggplot(aes(x = date, y = rmse)) + 
        geom_line() + 
        geom_hline(yintercept = mean(result_table$rmse))


result_table %>% 
        ggplot(aes(x = date, y = nrmse)) + 
        geom_line(aes(col = season), lwd = 1.5) +
        geom_hline(data = mnrmse, aes(col = season, yintercept = V1)) +
        facet_grid(.~season)


summary(discharge$discharge) 


