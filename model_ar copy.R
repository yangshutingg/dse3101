rm(list=ls())

library(readxl)
library(tidyverse)
library(zoo)
library(sandwich)
library(lsei)
data = read_excel("ROUTPUTQvQd.xlsx") 
data = data[-c(1:49), ] # remove data before 1959Q2 due to NAs

data_spread = read_excel("allmonth.xls")

data_spread = data_spread[-c(1:3, 781:795),]
data_spread = data_spread %>%
  select("Date", "Spread")

data_spread_mth <- ts(data_spread[,"Spread"], start = c(1959, 4), frequency = 12)
data_spread_qtr <- aggregate(data_spread_mth, nfrequency = 4, mean)
data_spread_qtr <- as.data.frame(data_spread_qtr)
X2 = embed(data_spread_qtr[,1], 2)
X2 = as.matrix(X2[,1] - X2[,2])
#plot(X2)

# user input:
# starting & ending quarter for testing, h-step, expanding/rolling window for cv
# no restriction for the length of interval, but earliest 150 quarters from 1965q4 -> 2003q2, latest 2023q4, length of cv is fixed at 50 quarters

# function: get necessary data 
# input: user select the end of the desired forecast interval
# output: get all the data for training, cv and testing

# sample input: 2003q2, 2005q4
get_data = function(start_q, end_q) {
  start_quarter = str_remove(start_q, ":")
  end_quarter = str_remove(end_q, ":")
  start_col_name = paste("ROUTPUT", str_sub(start_quarter, start = 3, end = 6), sep = "")
  end_col_name = paste("ROUTPUT", str_sub(end_quarter, start = 3, end = 6), sep = "")
  
  end_index = as.numeric(str_sub(end_quarter, start = 3, end = 4)) + as.numeric(str_sub(end_quarter, start = 6, end = 6)) / 4
  # 23q4 --> 23.75
  start_index = as.numeric(str_sub(start_quarter, start = 3, end = 4)) + as.numeric(str_sub(start_quarter, start = 6, end = 6)) / 4
  num_quarters <<- (end_index - start_index) * 4 + 1
  
  data_full = data %>%
    select("DATE" : end_col_name)
  
  num_col = num_quarters + 50
  # extract last num_col columns up til the end period: 50 quarters in cross validation, num_quarters in testing
  data_full = data_full[, (ncol(data_full)-num_col+1):ncol(data_full)]
  
  # adding the date column
  data_full = cbind(data$DATE, data_full)
  
  return(data_full)
}

data_full = get_data("2003Q2","2005Q4")

# year_end = as.numeric(str_sub("24Q1", start = 1, end = 2))
# quarter_end = as.numeric(str_sub("24Q1", start = 4, end = 4))
# ll = get_data("14Q1") %>% select(last_col()) %>% rename_with(.cols = 1, ~"gdp") %>%
#   filter(!is.na(as.numeric("gdp"))) %>% nrow()
# true_values = tail(mse_data[1:ll,], 15)
# data2 = get_data("12Q3")
# get_data("24Q1") %>% select(last_col()) %>% rename_with(.cols = 1, ~"gdp") %>%
#   filter(is.na(as.numeric("gdp")))

fitAR=function(Y,p,h){
  
  #Inputs: Y- predicted variable,  p - AR order, h -forecast horizon
  
  aux=embed(Y,p+h) #create 2 lags + forecast horizon shift (=h option)
  y=aux[,1] #  Y variable aligned/adjusted for missing data due to lags
  X=as.matrix(aux[,-c(1:(ncol(Y)*h))]) # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)  
  
  if(h==1){ 
    X.out=tail(aux,1)[1:ncol(X)] #retrieve last p observations if one-step forecast 
  }else{
    X.out=aux[,-c(1:(ncol(Y)*(h-1)))] #delete first (h-1) columns of aux,  
    X.out=tail(X.out,1)[1:ncol(X)] #last p observations to predict T+1 
  }
  
  model=lm(y~X) #estimate direct h-step AR(p) by OLS 
  coef=coef(model) #extract coefficients
  
  pred=c(1,X.out)%*%coef #make a forecast using the last few observations: a direct h-step forecast.
  #note the addition of a constant to the test observation vector
  
  rmsfe=sqrt(sum(model$residuals^2)/nrow(X)) #get unadjusted rmsfe (ignoring estimation uncertainty)
  
  return(list("model"=model,"pred"=pred,"coef"=coef, "rmsfe"=rmsfe)) #save estimated AR regression, prediction, and estimated coefficients
}


# get latest Y - 2024q1
mse_data = data[,ncol(data)]

data_most_recent = mse_data %>% # extract last column
  rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
  mutate(gdp = as.numeric(gdp)) %>%
  drop_na() %>%
  mutate(loggdp = log(gdp))

temp=embed(data_most_recent$loggdp,2) #create lag of log(GDP) and align the original series

Y_recent=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference


# cross validation - rolling window, adjusted code to add p as a parameter
ar.rolling.window=function(data_cv,Y,noos,p,h){ #equality here  means default inputs
  
  save.coef=matrix(NA,noos,p+1) #blank matrix for coefficients at each iteration (3=constant+ 2 lags)
  save.pred=matrix(NA,noos,1) #blank for forecasts
  real=matrix(NA,noos,1)
  sign=matrix(NA,noos,1)
  for(i in 1:noos){ 
    # get real-time data
    temp_Y = data_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
      mutate(gdp = as.numeric(gdp)) %>%
      drop_na() %>%
      mutate(loggdp = log(gdp)) %>%
      pull(loggdp)
    temp_Y = as.matrix(temp_Y)
    index = nrow(temp_Y)
    
    temp_Y = temp_Y[i:nrow(temp_Y),]
    
    temp=embed(temp_Y,2) #create lag of log(GDP) and align the original series
    #with it for the available
    
    Y.window=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference
    
    
    winfit=fitAR(Y.window,p,h) #call the function to fit the AR(2) and generate h-step forecast
    save.coef[(i),]=winfit$coef #save estimated coefficients
    save.pred[(i),]=winfit$pred #save the forecast
    #cat("iteration",(1+noos-i),"\n") #display iteration number (useful for slower ML methods)
    
    real[i]=Y[index] #get actual values
    sign[i]=ifelse(Y[index]>0, 
                   ifelse(winfit$pred>0, 0, 1), 
                   ifelse(winfit$pred>0, 1, 0))
  }
  
  #Some useful post-prediction misc stuff:
  #plot(real,type="l")
  #lines(c(rep(NA,length(real)-noos),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((real-save.pred)^2)) #compute RMSE
  mae=mean(abs(real-save.pred)) #compute MAE (Mean Absolute Error)
  signs = sum(sign)/noos #no of signs predicted wrongly
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors,"real"=real)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}


# function: rolling window - cross validation (50 quarters)
# input: data, noos = 50, p, h=1
# output: cross validation results in a list + plot

cv_rolling = function(data_full, Y, noos = 50, p, h){
  data_cv = data_full %>%
    select(c(1, 2:51))
  return(ar.rolling.window(data_cv, Y, noos=50, p, h))
}

ar12=cv_rolling(data_full,Y_recent,noos=50,2,1) #1-step POOS AR(2) forecast

test_rolling = function(data_full, Y, noos = num_quarters, p, h){
  data_test = data_full %>%
    select(c(1, 52:ncol(data_full)))
  return(ar.rolling.window(data_test, Y, noos = num_quarters, p, h))
}

ar12=test_rolling(data_full,Y_recent,num_quarters,2,1) #1-step POOS AR(2) forecast

#expanding window CV
ar.expanding.window=function(data_cv,Y,noos,p,h){ #equality here  means default inputs
  
  save.coef=matrix(NA,noos,p+1) #blank matrix for coefficients at each iteration (3=constant+ 2 lags)
  save.pred=matrix(NA,noos,1) #blank for forecasts
  real=matrix(NA,noos,1)
  sign=matrix(NA,noos,1)
  for(i in 1:noos){ 
    # get real-time data
    temp_Y = data_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
      mutate(gdp = as.numeric(gdp)) %>%
      drop_na() %>%
      mutate(loggdp = log(gdp)) %>%
      pull(loggdp)
    
    temp_Y = as.matrix(temp_Y)
    
    
    temp=embed(temp_Y,2) #create lag of log(GDP) and align the original series
    #with it for the available
    
    Y.window=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference
    
    
    winfit=fitAR(Y.window,p,h) #call the function to fit the AR(2) and generate h-step forecast
    save.coef[(i),]=winfit$coef #save estimated coefficients
    save.pred[(i),]=winfit$pred #save the forecast
    #cat("iteration",(1+noos-i),"\n") #display iteration number (useful for slower ML methods)
    
    real[i]=Y[length(Y.window)+1] #get actual values
    sign[i]=ifelse(Y[length(Y.window)+1]>0, 
                   ifelse(winfit$pred>0, 0, 1), 
                   ifelse(winfit$pred>0, 1, 0))
  }
  
  #Some useful post-prediction misc stuff:
  #plot(real,type="l")
  #lines(c(rep(NA,length(real)-noos),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((real-save.pred)^2)) #compute RMSE
  mae=mean(abs(real-save.pred)) #compute MAE (Mean Absolute Error)
  signs = sum(sign)/noos
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors,"real"=real)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}

cv_expanding = function(data_full, Y, noos = 50, p, h){
  data_cv = data_full %>%
    select(c(1, 2:51))
  return(ar.expanding.window(data_cv, Y, noos = 50, p, h))
}

ar1.1=cv_expanding(data_full,Y_recent,50,2,1) #1-step POOS AR(2) forecast

test_expanding = function(data_full, Y, noos = num_quarters, p, h){
  data_test = data_full %>%
    select(c(1, 52:ncol(data_full)))
  return(ar.expanding.window(data_test, Y, noos = num_quarters, p, h))
}

ar1.1=test_expanding(data_full,Y_recent,num_quarters,2,1) #1-step POOS AR(2) forecast



#combined forecast

ar_combined = function(data_full, h, test_fn, Y) {
  AR_preds = sapply(1:8, function(i) {return(test_fn(data_full, Y, p = i, h = h)$pred)})  
  AR_simple_combined = apply(AR_preds, 1, mean)
  no_obs = data_full %>%
    select(last_col()) %>%
    rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
    mutate(gdp = as.numeric(gdp)) %>%
    drop_na() %>%
    mutate(loggdp = log(gdp)) %>%
    nrow()
  real = Y[(no_obs-num_quarters+1):no_obs]
  rmse=sqrt(mean((real-AR_simple_combined)^2)) #compute RMSE
  mae=mean(abs(real-AR_simple_combined)) #compute MAE (Mean Absolute Error)
  sign = sapply(1:nrow(AR_preds), function(i) {ifelse(real[i]>0, 
                                                      ifelse(AR_simple_combined[i]>0, 0, 1), 
                                                      ifelse(AR_simple_combined[i]>0, 1, 0))})
  signs = sum(sign)/nrow(AR_preds)
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs) #stack errors in a vector
  return(list("pred" = AR_simple_combined, "errors" = errors,"real"=real))
}

AR_combined1 = ar_combined(data_full, 1, test_rolling, Y_recent)

# ar_combined_bg = function(data_full, h, errors, test_fn, Y) {
#   ervec = sapply(errors, function(i) {return(1/i^2)})
#   weights = ervec/sum(ervec)
#   AR_preds = sapply(1:8, function(i) {return(test_fn(data_full, p = i, h = h)$pred)})  
#   AR_bg_combined = sapply(1:8, function(i) {return(weights[i]*AR_preds[,i])})
#   AR_bg_combined = apply(AR_bg_combined, 1, sum)
#   no_obs = data_full %>%
#     select(last_col()) %>%
#     rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
#     mutate(gdp = as.numeric(gdp)) %>%
#     drop_na() %>%
#     mutate(loggdp = log(gdp)) %>%
#     nrow()
#   real = Y[(no_obs-num_quarters+1):no_obs]
#   rmse=sqrt(mean((real-AR_bg_combined)^2)) #compute RMSE
#   mae=mean(abs(real-AR_bg_combined)) #compute MAE (Mean Absolute Error)
#   errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
#   return(list("pred" = AR_bg_combined, "errors" = errors))
# }
# 
# ar_errors = sapply(1:8, function(i){
#   cv_rolling(data_full, p = i, h = 1)$errors[1]})
# AR_combined2 = ar_combined_bg(data_full, 1, ar_errors, test_fn = test_rolling, Y_recent)

#testing function
# err = sapply(1:8, function(i) {return(cv_rolling(data_full, p = i, 1)$errors[1])})  
# ervec = sapply(err, function(i) {return(1/i^2)})
# weights = ervec/sum(ervec)
# AR_preds = sapply(1:8, function(i) {return(test_rolling(data_full, p = i, h)$pred)})  
# AR_bg_combined = sapply(1:8, function(i) {return(weights[i]*AR_preds[,i])})
# AR_bg_combined = apply(AR_bg_combined, 1, sum)


#granger-ramanathan with constant and constraints
ar_gr_combined = function(data_full, h, cv_preds, oosy, test_fn, Y) {
  e_mat = diag(9) #8 AR models
  e_mat[1, 1] = 0
  X = cbind(rep(1, 50), cv_preds) #50 predictions
  weights = lsei(X, oosy, c=c(0, rep(1,8)), d=1, e=e_mat, f=rep(0,9))
  AR_preds = sapply(1:8, function(i) {return(test_fn(data_full, Y, p = i, h = h)$pred)})  
  AR_gr_combined = sapply(1:8, function(i) {return(weights[i]*AR_preds[,i])})
  AR_gr_combined = apply(AR_gr_combined, 1, sum)
  no_obs = data_full %>%
    select(last_col()) %>%
    rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
    mutate(gdp = as.numeric(gdp)) %>%
    drop_na() %>%
    mutate(loggdp = log(gdp)) %>%
    nrow()
  real = Y[(no_obs-num_quarters+1):no_obs]
  rmse=sqrt(mean((real-AR_gr_combined)^2)) #compute RMSE
  mae=mean(abs(real-AR_gr_combined)) #compute MAE (Mean Absolute Error)
  sign = sapply(1:nrow(AR_preds), function(i) {ifelse(real[i]>0, 
                                                      ifelse(AR_gr_combined[i]>0, 0, 1), 
                                                      ifelse(AR_gr_combined[i]>0, 1, 0))})
  signs = sum(sign)/nrow(AR_preds)
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs) #stack errors in a vector
  return(list("pred" = AR_gr_combined, "errors" = errors))
}

ar_preds = sapply(1:8, function(i){
  cv_rolling(data_full, p = i, h = 1)$pred})
no_obs_cv = data_full %>%
  select(50) %>%
  rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
  mutate(gdp = as.numeric(gdp)) %>%
  drop_na() %>%
  mutate(loggdp = log(gdp)) %>%
  nrow()
oosy = as.matrix(Y_recent[(no_obs_cv-48):(no_obs_cv+1)])
ar_combined3 = ar_gr_combined(data_full, 1, ar_preds, oosy, test_rolling, Y_recent)

#find interval boundaries for plotting
intervals = function(x, p, rmsfe) {
  alpha = qnorm(p+(1-p)/2, lower.tail = TRUE)
  upper = x + alpha*rmsfe
  lower = x - alpha*rmsfe
  
  boundaries = data.frame(upper, x, lower)
  return(boundaries)
}

int = intervals(ar12$pred, 0.5, ar12$errors[1])
int[,3]
int$lower
true_ts = ts(tail(Y, 15), start = c(20, 3), end = c(24, 1), frequency = 4)
forecast.ts = ts(ar1.1$pred, start = c(21, 4), end = c(24, 1), frequency = 4)
forecast.ts1 = ts(ar1.1$pred, start = c(20, 3), end = c(24, 1), frequency = 4)
plot.ts(true_ts)
points(forecast.ts, type = "l", col = "red")
points(forecast.ts)
plot(forecast.ts)





# ADL - real personal consumption, same transformation as gdp level
rpc = read_excel("RCONQvQd.xlsx")
rpc = rpc[-c(1:49), ] # remove data before 1959Q2 due to NAs

get_data_rpc = function(start_q, end_q) {
  start_quarter = str_remove(start_q, ":")
  end_quarter = str_remove(end_q, ":")
  start_col_name = paste("RCON", str_sub(start_quarter, start = 3, end = 6), sep = "")
  end_col_name = paste("RCON", str_sub(end_quarter, start = 3, end = 6), sep = "")
  
  rpc_full = rpc %>%
    select("DATE" : end_col_name)
  
  end_index = as.numeric(str_sub(end_quarter, start = 3, end = 4)) + as.numeric(str_sub(end_quarter, start = 6, end = 6)) / 4
  # 23q4 --> 23.75
  start_index = as.numeric(str_sub(start_quarter, start = 3, end = 4)) + as.numeric(str_sub(start_quarter, start = 6, end = 6)) / 4
  num_quarters <<- (end_index - start_index) * 4 +1
  
  num_col = num_quarters + 50 
  # extract last num_col columns up til the end period: 50 quarters in cross validation, num_quarters in testing
  rpc_full = rpc_full[, (ncol(rpc_full)-num_col+1):ncol(rpc_full)]
  
  # adding the date column
  rpc_full = cbind(rpc$DATE, rpc_full)
  
  return(rpc_full)
}

rpc_full = get_data_rpc("2003Q2","2005Q4")

fitADL=function(Y,X1,X2,p_y,p_x1,p_x2,h){
  
  max_lags = max(p_y, p_x1, p_x2)
  
  aux=embed(Y,p_y+h) #create 2 lags + forecast horizon shift (=h option)
  y=aux[,1] #  Y variable aligned/adjusted for missing data due to lags
  lags_y=as.matrix(aux[,-c(1:(ncol(Y)*h))]) # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)  
  diff = max_lags - p_y + 1
  y = y[diff:length(y)]
  lags_y = as.matrix(lags_y[diff:nrow(lags_y),])
  
  
  aux_x1=embed(X1,p_x1+h) # create lags for x1
  lags_x1=as.matrix(aux_x1[,-c(1:(ncol(X1)*h))]) # lags of x1
  diff = max_lags - p_x1 + 1
  lags_x1 = as.matrix(lags_x1[diff:nrow(lags_x1),])
  #print(nrow(lags_x1))
  
  aux_x2 = embed(X2, p_x2+h)
  lags_x2 = as.matrix(aux_x2[, -c(1:h)])
  diff = max_lags - p_x2 + 1
  lags_x2 = as.matrix(lags_x2[diff:nrow(lags_x2),])
  #print(nrow(lags_x2))
  
  X = cbind(lags_y, lags_x1, lags_x2) # column binding all the predictors (lags of Y and lags of X1)
  
  if(h==1){ 
    new_y = matrix(tail(aux,1)[1:ncol(lags_y)],nrow=1) # extract y_t, y_t-1...
    new_x1 = matrix(tail(aux_x1,1)[1:ncol(lags_x1)],nrow=1) # extract x_t, x_t-1...
    new_x2 = matrix(tail(aux_x2, 1)[1:ncol(lags_x2)], nrow = 1)
    X.out=c(new_y, new_x1, new_x2) #retrieve last p observations if one-step forecast 
  }else{
    temp_y=aux[,-c(1:(ncol(Y)*(h-1)))] #delete first (h-1) columns of aux
    temp_x1=aux_x1[,-c(1:(ncol(X1)*(h-1)))] #delete first (h-1) columns of aux_x1
    temp_x2 = aux_x2[,-c(1:(ncol(X2)*(h-1)))]
    new_y = matrix(tail(temp_y,1)[1:ncol(lags_y)],nrow=1)
    new_x1 = matrix(tail(temp_x1,1)[1:ncol(lags_x1)],nrow=1)
    new_x2 = matrix(tail(temp_x2,1)[1:ncol(lags_x2)],nrow=1)
    X.out=c(new_y,new_x1,new_x2) #last p observations to predict T+1 
  }
  
  model=lm(y~X) #estimate direct h-step AR(p) by OLS 
  coef=coef(model) #extract coefficients
  
  pred=c(1,X.out)%*%coef #make a forecast using the last few observations: a direct h-step forecast.
  #note the addition of a constant to the test observation vector
  
  rmsfe=sqrt(sum(model$residuals^2)/nrow(X)) #get unadjusted rmsfe (ignoring estimation uncertainty)
  
  return(list("model"=model,"pred"=pred,"coef"=coef, "rmsfe"=rmsfe)) #save estimated AR regression, prediction, and estimated coefficients
}

# cross validation - rolling window, adjusted code to add p as a parameter
adl.rolling.window=function(data_cv,rpc_cv,spread,Y,noos,p_y,p_x1,p_x2,h=1){ #equality here  means default inputs
  
  save.coef=matrix(NA,noos,p_y+p_x1+p_x2+1) #blank matrix for coefficients at each iteration (3=constant+ 2 lags)
  save.pred=matrix(NA,noos,1) #blank for forecasts
  real=matrix(NA,noos,1)
  sign=matrix(NA,noos,1)
  for(i in 1:noos){ 
    # get real-time data
    temp_Y = data_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
      mutate(gdp = as.numeric(gdp)) %>%
      drop_na() %>%
      #tail(80+i+1) %>%
      mutate(loggdp = log(gdp)) %>%
      pull(loggdp)
    temp_Y = as.matrix(temp_Y)
    
    temp_Y = temp_Y[i:nrow(temp_Y),]
    
    temp=embed(temp_Y,2) #create lag of log(GDP) and align the original series
    #with it for the available
    Y.window=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference
    no_qtrs = nrow(Y.window)+i-1
    #print(nrow(Y.window))
    
    
    temp_X1 = rpc_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"rpc") %>%  # renaming columns
      mutate(rpc = as.numeric(rpc)) %>%
      drop_na() %>%
      #tail(80+i+1) %>%
      mutate(logrpc = log(rpc)) %>%
      pull(logrpc)
    temp_X1 = as.matrix(temp_X1)
    
    temp_X1 = temp_X1[i:nrow(temp_X1),]
    
    tempX1=embed(temp_X1,2) #create lag of log(rpc) and align the original series
    X1.window=as.matrix(400*(tempX1[,1]-tempX1[,2])) #rpc growth via log difference
    #print(nrow(X1.window))
    
    X2.window = as.matrix(spread[i:no_qtrs])
    #print(nrow(X2.window))
    
    winfit=fitADL(Y.window,X1.window,X2.window,p_y,p_x1,p_x2,h) #call the function to fit the AR(2) and generate h-step forecast
    save.coef[(i),]=winfit$coef #save estimated coefficients
    save.pred[(i),]=winfit$pred #save the forecast
    #cat("iteration",(1+noos-i),"\n") #display iteration number (useful for slower ML methods)
    
    real[i] = Y_recent[no_qtrs+1]
    sign[i]=ifelse(Y[no_qtrs+1]>0, 
                   ifelse(winfit$pred>0, 0, 1), 
                   ifelse(winfit$pred>0, 1, 0))
  }
  
  #Some useful post-prediction misc stuff:
  #plot(real,type="l")
  #lines(c(rep(NA,length(real)-noos),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((real-save.pred)^2)) #compute RMSE
  mae=mean(abs(real-save.pred)) #compute MAE (Mean Absolute Error)
  signs=sum(sign)/noos
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors,"real"=real)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}

adl_rolling_test = adl.rolling.window(data_full, rpc_full, X2, Y_recent, 50, 2, 2,2)

# function: rolling window - cross validation (50 quarters)
# input: data, noos = 10, p, h=1
# output: cross validation results in a list

cv_rolling_adl = function(data_full, rpc_full, spread, Y, noos = 50, p_y, p_x1, p_x2, h = 1){
  data_cv = data_full %>%
    select(c(1, 2:51))
  rpc_cv = rpc_full %>%
    select(c(1, 2:51))
  return(adl.rolling.window(data_cv, rpc_cv, spread, Y, noos = 50, p_y, p_x1, p_x2, h))
}

adl22=cv_rolling_adl(data_full,rpc_full,X2,Y_recent,50,2,2,3,1) #1-step POOS ADL(2,2) forecast

test_rolling_adl = function(data_full, rpc_full, spread, Y, noos = num_quarters, p_y, p_x1, p_x2, h = 1){
  data_test = data_full %>%
    select(c(1, 52:ncol(data_full)))
  rpc_test = rpc_full %>%
    select(c(1, 52:ncol(rpc_full)))
  return(adl.rolling.window(data_test, rpc_test, spread, Y, noos = num_quarters, p_y, p_x1, p_x2, h))
}

adl22=test_rolling_adl(data_full,rpc_full,X2,Y_recent,num_quarters,3,2,2,1) #1-step POOS ADL(2,2) forecast

# DM test - between AR and ADL
# output: t-statistic and plot

dm_test = function(Y, start_quarter, end_quarter, ar_p, adl_p_y, adl_p_x, h) {
  data_full = get_data(start_quarter, end_quarter)
  rpc_full = get_data_rpc(start_quarter, end_quarter)
  
  # earliest possible start: 1959Q2, latest end: 2023q4
  year_start = as.numeric(str_sub(start_quarter, start = 1, end = 4))
  year_end = as.numeric(str_sub(end_quarter, start = 1, end = 4))
  q_start = as.numeric(str_sub(start_quarter, start = 6, end = 6))
  q_end = as.numeric(str_sub(end_quarter, start = 6, end = 6))
  
  row_start = (year_start - 1959) * 4 + q_start - 1 
  row_end = (year_end - 1959) * 4 + q_end - 1 - 1 # additional -1 due to gdp growth transformation
  
  oosy = Y[c(row_start:row_end), ] # get real values
  
  ar = test_rolling(data_full, Y, num_quarters, ar_p, h)
  #Compute absolute loss of AR model
  lar = abs(oosy-ar$pred)
  
  adl = test_rolling_adl(data_full, rpc_full, Y, num_quarters, adl_p_y, adl_p_x, h)
  #Compute absolute loss of ADL model
  ladl = abs(oosy-adl$pred)
  
  #Compute loss differential (d_t) (AR-ADL)
  dt = lar -ladl
  
  #Create ts object containing loss differential
  dt.ts=ts(dt, start=c(year_start,q_start), end=c(year_end,q_end), freq=4)
  
  #Plot to examine stationarity:
  plot.ts(dt.ts, main="Absolute Loss differential AR-ADL",cex.axis=1.8)
  
  #Regress d_t (AR-ADL) for 1-step forecasts on a constant - get estimate of mean(d_t)
  dmreg=lm(dt~1) #regression
  
  x=dmreg$coefficients/sqrt(NeweyWest(dmreg,lag=num_quarters^(1/3))) #form the DM t-statistic
  if (num_quarters<50) {
    x[1,1] = x[1,1]*sqrt(1+(1/num_quarters)*(1-2*h) + (1/num_quarters^2)*h*(h-1))
  }
  return(x[1,1]) # extract result
}

# comparing AR(2) and ADL(2, 2), 1-step ahead
dm_test(Y, "2003Q2", "2005Q4", 2, 2, 2, 1)

