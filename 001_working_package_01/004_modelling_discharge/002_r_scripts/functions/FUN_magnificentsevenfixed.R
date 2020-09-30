calc_magnificentseven_fix <- function (x, yearType = "water", digits = 3) 
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
