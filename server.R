library(shiny)
source("model_ar copy.R")

server = function(input, output, session) {
  data_used = get_data(input$end_quarter)
  AR_lags = rep(0, 8)
  
  if (input$window == "rolling") {
    AR_models = sapply(AF_lags, function(i) {return(cv_rolling(data_full = data_used, p = i, h = 1)$errors)})
  }
  else if (input$window == "expanding") {
    #expanding window code
  }
   
  #for (i in 1:8) {
  #  AR_models[i] = cv_rolling(data_used, p = i, h = input$h)$errors
  #}
  
  rank_ar = order(AR_models[1,])
  
  best_ar_lag = rank_ar[1]
  
  #performance metrics
  
  
  #for forecast combinations
  AR_preds = sapply(AF_lags, function(i) {return(test_rolling(data_full = data_used, p = i, h = 1)$pred)})  
  AR_combined = sapply(AR_preds, mean)
  
  
  #plot for chosen model
  
}

#x = list(c(3, 4, 1, 25, 9))
#order(x[[1]])

x = c(1:8)
x1 = sapply(x, function(i) {return(cv_rolling(data_full, p = i, h = 1)$errors)})
for (i in 1:8) {
  x[i] = cv_rolling(data_full, p = i, h=1)$errors
}
order(x1[1,])
class(ar1$errors)
