library(shiny)
source("model_ar copy.R")

server = function(input, output, session) {
  data_used = get_data(input$end_quarter)
  #rval_data = reactive({get_data(input$end_quarter)})
  #data_used = rval_data()?
  AR_lags = rep(0, 8)
  
  if (input$window == "rolling") {
    cv_fn = function(data_full, p, h) {
      return(cv_rolling(data_full, p, h))
    }
    test_fn = function(data_full, p, h) {
      return(test_rolling(data_full, p, h))
    }
  } else if (input$window == "expanding") {
    #expanding window code
    cv_fn = function(data_full, p, h) {
      return(cv_rolling(data_full, p, h))
    }
    test_fn = function(data_full, p, h) {
      return(test_rolling(data_full, p, h))
    }
  }
  
  AR_models = sapply(AF_lags, function(i) {return(cv_fn(data_full = data_used, p = i, h = input$h)$errors)})
  
   
  #for (i in 1:8) {
  #  AR_models[i] = cv_rolling(data_used, p = i, h = input$h)$errors
  #}
  
  rank_ar = order(AR_models[1,])
  
  best_ar_lag = rank_ar[1]
  
  #performance metrics
  #code
  output$rmsfe = test_fn(data_used, p = best_ar_lag, h = input$h)$errors[1]
  #text output? not sure put where
  
  
  
  #get h-step forecasts for test window
  if (input$model_type == "AR") {
    model = test_fn(data_used, p = best_ar_lag, h = input$h)
    forecasts = model$pred
    rmsfe = model$error[1]
  }
  
  
  #for forecast combinations
  AR_preds = sapply(AF_lags, function(i) {return(test_rolling(data_full = data_used, p = i, h = 1)$pred)})  
  AR_combined = sapply(AR_preds, mean)
  
  
  #plot for chosen model
  #Syntax: ts(object, start=startdate, end=enddate, freq=frequency (periods per year))
  rval_year_end = reactive({as.numeric(str_sub(input$end_quarter, start = 1, end = 2))})
  rval_quarter_end = reactive({as.numeric(str_sub(input$end_quarter, start = 4, end = 4))})
  year_end = rval_year_end()
  quarter_end = rval_quarter_end()
  year_start = ifelse(quarter_end > 2, year_end - 3, year_end - 4)
  quarter_start = ifelse(quarter_end == 1, 3, 
                         ifelse(quarter_end == 2, 4, 
                                ifelse(quarter_end == 3, 1, 2)))
  year_start_pred = ifelse(quarter_end > 3, year_end - 2, year_end - 3)
  quarter_start_pred = ifelse(quarter_end == 1, 4, 
                              ifelse(quarter_end == 2, 1, 
                                     ifelse(quarter_end == 3, 2, 3)))
  last_obs = data_used %>% select(last_col()) %>% rename_with(.cols = 1, ~"gdp") %>%
    filter(!is.na(as.numeric(gdp))) %>% nrow()
  true_values = tail(mse_data[1:last_obs+1,], 15) #add some context before test window
  # dates1 = tail(mse_data[1:last_obs+1,], 15)
  # dates2 = tail(mse_data[1:last_obs+1,], 10)
  # plot_data = as.data.frame(c(true_values, dates1))
  # plot_data_2 = as.data.frame(forecasts, dates2)
  
  
  
    
  output$plot = renderPlot({
    true_ts = ts(true_values, start = c(year_start, quarter_start), end = c(year_end, quarter_end), frequency = 4)
    forecast.ts = ts(forecasts, start = c(year_start_pred, quarter_start_pred), end = c(year_end, quarter_end), frequency = 4)
    #upper bound interval
    #lower bound interval
    
    forecast_intervals = intervals(forecasts, input$sig_level, rmsfe)
    upper.ts = ts(forecast_intervals[,1], start = c(year_start_pred, quarter_start_pred), end = c(year_end, quarter_end), frequency = 4)
    lower.ts = ts(forecast_intervals[,3], start = c(year_start_pred, quarter_start_pred), end = c(year_end, quarter_end), frequency = 4)
    
    plot.ts(true_ts, main = "h-step Forecasts", cex.axis=1.5, lwd=1.8, col="black", ylab="GDP growth")
    points(forecasts_ts, type = "l", col = "red", lwd = 1.8)
    points(upper.ts, type = "l", col = "blue", lwd = 1.8)
    points(lower.ts, type = "l", col = "blue", lwd = 1.8)
    legend("bottomleft", legend = c("True values", "Forecasts", "Upper bound of interval", "Lower bound of interval"))
 
  })
  
  
  
  # #Similarly, I create ts objects out of 1-step and 12-step benchmark forecasts
  # fcasts.ts=ts(cbind(ar1$pred,ar2$pred,ar3$pred,ar4$pred,oosy), start=c(1995,1), end=c(2019,4), freq=4)
  # colnames(fcasts.ts)=c("h=1","h=2","h=3","h=4","True Value")
  # 
  # #Now plot the forecasts:
  # cols=c("black","blue","red","deeppink","green")
  # plot.ts(fcasts.ts[,5], main="h-step AR(2) forecasts", cex.axis=1.5, lwd=1.8, col="black", ylab="GDP growth")
  # points(fcasts.ts[,1], type="l", col="blue",lwd=2)
  # points(fcasts.ts[,2], type="l", col="red",lwd=1.8)
  # points(fcasts.ts[,3], type="l", col="deeppink",lwd=2)
  # points(fcasts.ts[,4], type="l", col="green",lwd=2)
  # legend("bottomleft",legend=c("GDP growth","1-step","2-step","3-step","4-step"), pch=15, col=cols)
  # 
  
}

#how to use reactive?


#x = list(c(3, 4, 1, 25, 9))
#order(x[[1]])

x = c(1:8)
x1 = sapply(x, function(i) {return(cv_rolling(data_full, p = i, h = 1)$errors)})
for (i in 1:8) {
  x[i] = cv_rolling(data_full, p = i, h=1)$errors
}
order(x1[1,])
class(ar1$errors)
