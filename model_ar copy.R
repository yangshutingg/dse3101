rm(list=ls())

library(readxl)
library(tidyverse)
library(zoo)
data = read_excel("ROUTPUTQvQd.xlsx") 
data = data[-c(1:48), ] # remove data before 1959Q1 due to NAs

# user input:
# starting & ending quarter for testing(min 10 quarters?), h-step, expanding/rolling window for cv
# starting must > 1959q1+60, ending must >= starting+10, 10 <= ending-starting <= 40 --> training cv testing ratio = 80:20:20

# function: get necessary data 
# input: user select the end of the desired forecast interval
# output: get all the data for training, cv and testing
# e.g. if user selects 24Q1, then 19Q1 will be used for training, 19Q2-21Q3 will be used for cv, 21Q4-24Q1 will be used for testing

mse_data = data[,ncol(data)]

get_data = function(start_quarter, end_quarter) {
  start_col_name = paste("ROUTPUT", start_quarter, sep = "")
  end_col_name = paste("ROUTPUT", end_quarter, sep = "")
  
  end_index = as.numeric(str_sub(end_quarter, start = 1, end = 2)) + as.numeric(str_sub(end_quarter, start = 4, end = 4)) / 4
  # 24q1 --> 24.25
  start_index = as.numeric(str_sub(start_quarter, start = 1, end = 2)) + as.numeric(str_sub(start_quarter, start = 4, end = 4)) / 4
  num_quarters <<- (end_index - start_index) * 4
  
  data_full = data %>%
    select("DATE" : end_col_name)
  
  num_col = num_quarters * 2 + 1
  # extract last num_col columns up til the end period: 1 column for training, num_quarters in cross validation, num_quarters in testing
  data_full = data_full[, (ncol(data_full)-num_col+1):ncol(data_full)]
  
  # adding the date column
  data_full = cbind(data$DATE, data_full)
  
  return(data_full)
}

data_full = get_data("20Q1","24Q1")

year_end = as.numeric(str_sub("24Q1", start = 1, end = 2))
quarter_end = as.numeric(str_sub("24Q1", start = 4, end = 4))
ll = get_data("14Q1") %>% select(last_col()) %>% rename_with(.cols = 1, ~"gdp") %>%
  filter(!is.na(as.numeric("gdp"))) %>% nrow()
true_values = tail(mse_data[1:ll,], 15)
data2 = get_data("12Q3")
get_data("24Q1") %>% select(last_col()) %>% rename_with(.cols = 1, ~"gdp") %>%
  filter(is.na(as.numeric("gdp")))

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


# function: train AR(p) model
# input: p, h, data for training
# output: fitted training model, in a list
# take the most recent data(column), retrieve the latest 80 quarters as training data

num_quarters_training = num_quarters * 4
trainAR = function(data_full, p, h) {
  data_training = data_full %>%
    select(1:2) %>%
    rename_with(.cols = 1, ~"date") %>%
    rename_with(.cols = 2, ~"gdp") %>%  # renaming columns
    mutate(gdp = as.numeric(gdp)) %>%
    drop_na() %>%
    tail(n = num_quarters_training) %>%
    mutate(loggdp = log(gdp))
  
  temp=embed(data_training$loggdp,2) #create lag of log(GDP) and align the original series
  
  Y=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference
  
  return(fitAR(Y, p, h))
}


# 1 step AR(2):
AR2_1=trainAR(data_full,2,1)

# get 1 step ahead point forecast
AR2_1_result = AR2_1$pred



# get real-time Y (at the end of the forecast period?) for cv
data_most_recent = mse_data %>% # extract last column
  rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
  mutate(gdp = as.numeric(gdp)) %>%
  drop_na() %>%
  #tail(n = 91) %>%
  mutate(loggdp = log(gdp))
  
temp=embed(data_most_recent$loggdp,2) #create lag of log(GDP) and align the original series

Y=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference


# cross validation - rolling window, adjusted code to add p as a parameter
ar.rolling.window=function(data_cv,noos,p,h=1){ #equality here  means default inputs
  
  save.coef=matrix(NA,noos,p+1) #blank matrix for coefficients at each iteration (3=constant+ 2 lags)
  save.pred=matrix(NA,noos,1) #blank for forecasts
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
    
    
    winfit=fitAR(Y.window,p,h) #call the function to fit the AR(2) and generate h-step forecast
    save.coef[(1+noos-i),]=winfit$coef #save estimated coefficients
    save.pred[(1+noos-i),]=winfit$pred #save the forecast
    #cat("iteration",(1+noos-i),"\n") #display iteration number (useful for slower ML methods)
  }
  
  #Some useful post-prediction misc stuff:
  real=Y #get actual values
  #plot(real,type="l")
  #lines(c(rep(NA,length(real)-noos),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,noos)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,noos)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}


# function: rolling window - cross validation (10 quarters)
# input: data, noos = 10, p, h=1
# output: cross validation results in a list + plot

cv_rolling = function(data_full, noos = 10, p, h = 1){
  data_cv = data_full %>%
    select(c(1, 3:(num_quarters+2)))
  return(ar.rolling.window(data_cv, noos, p, h))
}

ar12=cv_rolling(data_full,num_quarters,2,1) #1-step POOS AR(2) forecast

test_rolling = function(data_full, noos = 10, p, h){
  data_test = data_full %>%
    select(c(1, (num_quarters+3):ncol(data_full)))
  return(ar.rolling.window(data_test, noos, p, h))
}


#expanding window CV
ar.expanding.window=function(data_cv,noos,p,h=1){ #equality here  means default inputs
  
  save.coef=matrix(NA,noos,p+1) #blank matrix for coefficients at each iteration (3=constant+ 2 lags)
  save.pred=matrix(NA,noos,1) #blank for forecasts
  for(i in 1:noos){ 
    # get real-time data
    temp_Y = data_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
      mutate(gdp = as.numeric(gdp)) %>%
      drop_na() %>%
    #  tail(80+i+1) %>%
      mutate(loggdp = log(gdp)) %>%
      pull(loggdp)
    
    temp_Y = as.matrix(temp_Y)
    
    
    temp=embed(temp_Y,2) #create lag of log(GDP) and align the original series
    #with it for the available
    
    Y.window=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference
    
    
    winfit=fitAR(Y.window,p,h) #call the function to fit the AR(2) and generate h-step forecast
    save.coef[(1+noos-i),]=winfit$coef #save estimated coefficients
    save.pred[(1+noos-i),]=winfit$pred #save the forecast
    #cat("iteration",(1+noos-i),"\n") #display iteration number (useful for slower ML methods)
  }
  
  #Some useful post-prediction misc stuff:
  real=Y #get actual values
  #plot(real,type="l")
  #lines(c(rep(NA,length(real)-noos),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
  rmse=sqrt(mean((tail(real,noos)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,noos)-save.pred)) #compute MAE (Mean Absolute Error)
  errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}

cv_expanding = function(data_full, noos = 10, p, h = 1){
  data_cv = data_full %>%
    select(c(1, 3:(num_quarters+2)))
  return(ar.expanding.window(data_cv, noos, p, h))
}

ar1.1=cv_expanding(data_full,num_quarters,2,1) #1-step POOS AR(2) forecast

test_expanding = function(data_full, noos = 10, p, h){
  data_test = data_full %>%
    select(c(1, (num_quarters+3):ncol(data_full)))
  return(ar.expanding.window(data_test, noos, p, h))
}


#find interval boundaries for plotting
intervals = function(x, p, rmsfe) {
  alpha = qnorm(p+(1-p)/2, lower.tail = TRUE)
  upper = x + alpha*rmsfe
  lower = x - alpha*rmsfe
  
  boundaries = data.frame(upper, x, lower)
  return(boundaries)
}

int = intervals(ar1.1$pred, 0.95, ar1.1$errors[1])
int[,3]
int$lower
true_ts = ts(tail(Y, 15), start = c(20, 3), end = c(24, 1), frequency = 4)
forecast.ts = ts(ar1.1$pred, start = c(21, 4), end = c(24, 1), frequency = 4)
forecast.ts1 = ts(ar1.1$pred, start = c(20, 3), end = c(24, 1), frequency = 4)
plot.ts(true_ts)
points(forecast.ts, type = "l", col = "red")
points(forecast.ts)
plot(forecast.ts)
