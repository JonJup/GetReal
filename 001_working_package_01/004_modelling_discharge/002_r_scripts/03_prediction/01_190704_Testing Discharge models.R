
#TO DO 
# mehr variablen
# mehr modelle 
pacman::p_load(dplyr,
               data.table,
               magrittr,
               sf,
               ggplot2, 
               vtreat, # kWayCrossValidation()
               robust,
               mgcv) 
               
setwd(here::here())
# layer with all aggregated precipitations 
prec = st_read(dsn = "03_Data_Processed/02_Precipitation/03_Europe_Precipitation/all_acc_prec.gpkg") %>% st_drop_geometry() %>% setDT
# 
# ccm2 to add variables 
ccm2 = st_read("../01_Stream_Network/01_CCM2/02_GPKG/Catchment/2019-05-27_allGRcountires02.gpkg")
# subset ccm2 to interesting variables 
ccm2 %<>% st_drop_geometry() %>% setDT
ccm2.s = ccm2[,list(WSO1_ID,SLOPE_MEAN, SLOPE_MIN, SLOPE_MAX, SLOPE_STD, ELEV_MIN, ELEV_MAX, ELEV_MEAN, ELEV_STD)]

IDtoID = readRDS("03_Data_Processed/01_Discharge/combined/Stations_ID_to_WSO1ID.RDS") %>% st_drop_geometry() %>% setDT
discharge = readRDS("03_Data_Processed/01_Discharge/combined/discharge_combined_and_filtered.RDS") %>% 
  setDT()

ccm2.s = ccm2.s[IDtoID, on = "WSO1_ID"]
discharge.join = discharge[IDtoID, on = "ID"]
discharge.join = discharge.join[ccm2.s, on = "ID"]
names(prec)[2] = "winter_1999"

# load rediction test function 
source("02_Processing/Prediction/model_testing function.R")


# test runs 
test = random.evaluation(100, test.model = c("l", "glm.p"))
test = random.evaluation(100, test.model = c("gam","l^2O", "l^3O", "l", "glm.p"),
                         folds = 10)
test3 = random.evaluation(100, test.model = c("lw","l"))


ggplot(data = test3, aes(x = date, y = nrmse)) +
  geom_line(aes(col = model)) 
  
ggplot(data = test3, aes(x = date, y = rmse.cv))  +
  geom_line(aes(col = model)) 
  

test3 %>% 
  group_by(model) %>% 
  summarize(mean.nrmse = mean(nrmse),
            mean.rmse.cv = mean(rmse.cv),
            meanR = mean(Rsq))



# filter to year and season and day 
dis = discharge %>% filter(Year == 2000 & Month  == 1 & Day == 1)
# convert to spatial 
discharge.sf = st_as_sf(dis,  coords = c("Longitude","Latitude")) %>%
        st_set_crs(4326)
# join with catchments 
dis.join = st_join(x = discharge.sf,
                   y = prec,
                   join = st_intersects)





## exploratory data analysis 
# plot(Value ~ acc_prec, data = dis.join2)
# plot(Value ~ STRAHLER, data = dis.join2)
# plot(acc_prec ~ STRAHLER, data = dis.join2)
# plot(Value ~ AREA, data = dis.join2)
# plot(acc_prec ~ AREA, data = dis.join2)
# dis.join2 %<>% mutate(prec_area = acc_prec/AREA)
# plot(Value ~ prec_area, data = dis.join2)
# 
# plot(Value ~ RAIN_MIN, dis.join2)
# plot(Value ~ RAIN_MAX, dis.join2)
# 
# plot(acc_prec ~ RAIN_MIN, dis.join2)
# plot(acc_prec ~ RAIN_MAX, dis.join2)
# plot(prec_area ~ RAIN_MIN, dis.join2)
# plot(prec_area ~ RAIN_MAX, dis.join2)

# rsme should be smaller than  309.1832
sd(dis.join2$Value)

# to calculate nRMSE 

# linear model 1 -- basic -- 
# rmse =  187.5917
# nRMSE = 0.04
# rmse cv 3 fold = 187.7557
  
fmla = as.formula(Value~acc_prec)
mod1 = lm(fmla, data = dis.join2)
plot(mod1)
mod1.pred = predict(mod1)
plot(x = mod1.pred,
     y = dis.join2$Value)
abline(0,1)
(rmse = sqrt(mean((mod1.pred - dis.join2$Value)^2)))
rmse / stand
k <- 5
splitPlan = kWayCrossValidation(nRows = nrow(dis.join2), nSplits = k, dframe = NULL, y = NULL) 
dis.join2$pred.cv = 0

for (i in 1:k) {
        split <- splitPlan[[i]]
        model <- lm(fmla, data = dis.join2[split$train,])
        dis.join2$pred.cv[split$app] <- predict(model, newdata = dis.join2[split$app,])
}
(rmse.cv = sqrt(mean((dis.join2$pred.cv - dis.join2$Value)^2)))


# linear model 2 -- log -- 
# rmse =  316.4084
# rmse cv 5 fold = 316.408
fmla = as.formula(log(Value + 0.001) ~ acc_prec)
mod2 = lm(fmla, data = dis.join2)
plot(mod2)
mod2.pred = predict.lm(mod2)
plot(x = mod2.pred,
     y = dis.join2$Value)
abline(0,1)
(rmse = sqrt(mean((mod2.pred - dis.join2$Value)^2)))
# cv 
dis.join2$pred.cv = 0
for (i in 1:k) {
        split <- splitPlan[[i]]
        model <- lm(fmla, data = dis.join2[split$train,])
        dis.join2$pred.cv[split$app] <- predict(model, newdata = dis.join2[split$app,])
}
(rmse.cv = sqrt(mean((dis.join2$pred.cv - dis.join2$Value)^2)))

# linear model 3 -- sqrt -- 
# rmse =  312.277
# rmse cv 5 fold = 312.276
fmla = as.formula(sqrt(Value+ 0.001) ~ acc_prec)
mod3 = lm(fmla, data = dis.join2)
summary.lm(mod3)
mod3.pred = predict.lm(mod3, type = "response")
plot(x = mod3.pred,
     y = dis.join2$Value)
abline(0,1)
plot(mod3)
(rmse = sqrt(mean((mod3.pred - dis.join2$Value)^2)))
# cv 
dis.join2$pred.cv = 0
for (i in 1:k) {
        split <- splitPlan[[i]]
        model <- lm(fmla, data = dis.join2[split$train,])
        dis.join2$pred.cv[split$app] <- predict(model, newdata = dis.join2[split$app,])
}
(rmse.cv = sqrt(mean((dis.join2$pred.cv - dis.join2$Value)^2)))



# linear model 4 -- quadratic -- 
# rmse =  184.219
# rmse cv 5 fold = 184.2832
fmla = as.formula(Value ~acc_prec + I(acc_prec^2))
mod4 = lm(fmla, data = dis.join2)
summary(mod4)
plot(mod4)
mod4.pred = predict(mod4)
plot(x = mod4.pred,
     y = dis.join2$Value)
abline(0,1)
(rmse = sqrt(mean((mod4.pred - dis.join2$Value)^2)))
rmse / stand 
# cv 
dis.join2$pred.cv = 0
for (i in 1:k) {
        split <- splitPlan[[i]]
        model <- lm(fmla, data = dis.join2[split$train,])
        dis.join2$pred.cv[split$app] <- predict(model, newdata = dis.join2[split$app,])
}
(rmse.cv = sqrt(mean((dis.join2$pred.cv - dis.join2$Value)^2)))
rmse.cv / stand

## weighted model 
#rmse 261.0579
# 
my.control = lmRob.control(mxf = 100, mxr = 100)
weights     = robust::lmRob(Value ~ acc_prec, 
                            data = dis.join2, 
                            control = my.control)
mod         = lm(Value   ~ acc_prec,
                 data    = dis.join2, 
                 weights = weights$M.weights) 

pred.weight = predict(mod)
plot(dis.join2$Value ~ pred.weight)
abline(0,1)
(rmse = sqrt(mean((pred.weight - dis.join2$Value)^2)))


## glms 

glm1 = glm(Value~acc_prec + I(acc_prec^2) + I(acc_prec^3), data = dis.join2, family = "poisson")
summary.glm(glm1)
plot(glm1)
glm1.pred = predict.glm(glm1, type = "response")
plot(x = glm1.pred,
     y = dis.join2$Value)
abline(0,1)
(rmse = sqrt(mean((glm1.pred - dis.join2$Value)^2)))
rmse/stand



glm2 = glm(Value~acc_prec + I(acc_prec^2) + I(acc_prec^3) + I(acc_prec^4), data = dis.join2, 
           family = "quasipoisson")
summary.glm(glm2)
plot(glm2)
glm2.pred = predict.glm(glm2, type = "response")
plot(x = glm2.pred,
     y = dis.join2$Value)
abline(0,1)
(rmse = sqrt(mean((glm2.pred - dis.join2$Value)^2)))
rmse/stand
