### --- Model testing function --- ### 


random.evaluation = function(evaluations = 5, folds = 3, test.model = "l") {
      
      ## assign variables
   
      # for the weighted model 
      my.control = lmRob.control(mxf = 100, mxr = 100)
   
      # sample date variables 
      years = sample(2000:2013, evaluations, replace = T)
      months = sample(1:12, evaluations, replace = T)
      days = sample(1:28, evaluations, replace = T)
      
      # how many models should be tested ? 
      p = length(test.model)
      
      result_table = data.table(
            year = rep(years, times = p),
            month = rep(months, times = p),
            day = rep(days, times = p),
            rmse = rep(0, evaluations * p),
            nrmse = rep(0, evaluations * p),
            rmse.cv = rep(0, evaluations * p),
            Rsq = rep(0, evaluations * p),
            model = rep("none", evaluations * p)
      )
      # loop over models 
      for (ml in 1:p) {
            
            mia = test.model[ml]
            print(ml)
            ## loop over evaluations 
            for (i in 1:evaluations) {
                  print(i)
                  dis = discharge.join[Year == result_table$year[i] &
                                       Month == result_table$month[i] &
                                       Day == result_table$day[i]]
                  # select the right predictor variable
                  # adjust year for winter
                  if (result_table$month[i] %in% c(1,2)) {
                        year = result_table$year[i] - 1
                  } else year = result_table$year[i]
                  
                  season = ifelse(result_table$month[i] %in% c(12,1,2), "winter",
                                  ifelse(result_table$month[i] %in% c(3,4,5), "spring",
                                         ifelse(result_table$month[i] %in% c(6,7,8), "summer", "autumn")))
                  exp.var = paste0(season,"_",year)
                  prec.sub = prec[,c("WSO1_ID", exp.var), with = F]
                  dis.join = prec.sub[dis, on = "WSO1_ID"] %>% na.omit()
                  stand = max(dis.join$discharge) - min(dis.join$discharge)
                  result_table$model[i + (ml - 1) * evaluations] = mia
                  ## chose model 
                  if (mia == "l") {
                        fmla = as.formula(paste("discharge ~",exp.var))
                        mod = lm(fmla, data = dis.join)
                  }
                  if (mia == "glm.p") {
                        fmla = as.formula(paste("round(discharge+.1) ~",exp.var))
                        mod = glm(fmla, data = dis.join, family = "poisson")
                  }
                  if (mia == "l^2R") {
                        fmla = as.formula(paste("discharge ~","poly(", exp.var, ", degree = 2, raw = T)"))
                        mod = lm(fmla, data = dis.join)
                  }
                  if (mia == "l^2O") {
                        fmla = as.formula(paste("discharge ~","poly(", exp.var, ", degree = 2, raw = F)"))
                        mod = lm(fmla, data = dis.join)
                  }
                  if (mia == "l^3O") {
                        fmla = as.formula(paste("discharge ~","poly(", exp.var, ", degree = 3, raw = F)"))
                        mod = lm(fmla, data = dis.join)
                  }
                  
                  if (mia == "gam") {
                        fmla = as.formula(paste("discharge ~ s(",exp.var,")"))
                        mod = gam(fmla, family = gaussian ,data = dis.join)
                  }
                  if (mia == "lw") {
                     fmla = as.formula(paste("discharge ~ ", exp.var))
                     weights     = robust::lmRob(fmla, 
                                                 data = dis.join, 
                                                 control = my.control)
                     mod         = lm(fmla,
                                      data    = dis.join, 
                                      weights = weights$M.weights)
                  }
                  
                  pred =  predict(mod, type = "response")
                  result_table$rmse[i + (ml - 1) * evaluations] = sqrt(mean((pred - dis.join$discharge)^2))
                  result_table$nrmse[i + (ml - 1) * evaluations] = result_table$rmse[i + (ml - 1) * evaluations] / stand
                  result_table$Rsq[i + (ml - 1) * evaluations] = summary(mod)$r.squared
                  
                  splitPlan = kWayCrossValidation(nRows = nrow(dis.join), nSplits = folds,
                                                  dframe = NULL, y = NULL)
                  dis.join$pred.cv = 0
                  for (j in 1:folds) {
                        split <- splitPlan[[j]]
                        if (mia %in% c("l","l^2R", "l^2O", "l^3O")) model <- lm(fmla, data = dis.join[split$train,])
                        if (mia == "glm.p") model <- glm(fmla, data = dis.join[split$train,], family = "poisson")
                        if (mia == "gam") model = gam(fmla, data = dis.join[split$train,], family = gaussian)
                        if (mia == "lw") {
                                       weights     = robust::lmRob(fmla, data = dis.join[split$train,], control = my.control)
                                        model         = lm(fmla, data = dis.join[split$train,], weights = weights$M.weights)
                        }
                        dis.join$pred.cv[split$app] <- predict(model, newdata = dis.join[split$app,], type = "response")
                  }
                  result_table$rmse.cv[i + (ml - 1) * evaluations] = sqrt(mean((dis.join$pred.cv - dis.join$discharge)^2))
                  
            }
      }
      result_table[,"date" := lubridate::ymd(paste(year,month,day,sep = "-"))]
      (return(result_table))
      
      
}