rm(list=ls())

library(readxl)
library(dplyr)
library(zoo)
data = read_excel("group project/ROUTPUTQvQd.xlsx") 

# function: get necessary data 
# input: user select the end of the desired forecast interval
# output: get all the data for training, cv and testing
# e.g. if user selects 24Q1, then 19Q1 will be used for training, 19Q2-21Q3 will be used for cv, 21Q4-24Q1 will be used for testing

get_data = function(end_quarter) {
  col_name = paste("ROUTPUT", end_quarter, sep = "")
  
  data_full = data %>%
    select("DATE" : col_name)
  
  # extract last 21 columns up til the end period: 1 column for training, 10 in cross validation, 10 in testing
  data_full = data_full[, (ncol(data_full)-21+1):ncol(data_full)]
  
  # adding the date column
  data_full = cbind(data$DATE, data_full)
  
  return(data_full)
}

data_full = get_data("24Q1")

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

trainAR = function(data_full, p, h) {
  data_training = data_full %>%
    select(1:2) %>%
    rename_with(.cols = 1, ~"date") %>%
    rename_with(.cols = 2, ~"gdp") %>%  # renaming columns
    mutate(gdp = as.numeric(gdp)) %>%
    drop_na() %>%
    tail(n = 80) %>%
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
data_most_recent = data_full %>%
  select(ncol(data_full)) %>% # extract last column
  rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
  mutate(gdp = as.numeric(gdp)) %>%
  drop_na() %>%
  tail(n = 91) %>%
  mutate(loggdp = log(gdp))
  
temp=embed(data_most_recent$loggdp,2) #create lag of log(GDP) and align the original series

Y=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference


# cross validation - rolling window, adjusted code to add p as a parameter
ar.rolling.window=function(data_cv,noos,p,h=1){ #equality here  means deafult inputs
  
  save.coef=matrix(NA,noos,p+1) #blank matrix for coefficients at each iteration (3=constant+ 2 lags)
  save.pred=matrix(NA,noos,1) #blank for forecasts
  for(i in 1:noos){ 
    # get real-time data
    temp_Y = data_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
      mutate(gdp = as.numeric(gdp)) %>%
      drop_na() %>%
      tail(80+i+1) %>%
      mutate(loggdp = log(gdp))
    
    temp=embed(temp_Y$loggdp,2) #create lag of log(GDP) and align the original series
    #with it for the available
    
    Y.window=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference
    
    
    winfit=fitAR(Y.window,p,h) #call the function to fit the AR(2) and generate h-step forecast
    save.coef[(1+noos-i),]=winfit$coef #save estimated coefficients
    save.pred[(1+noos-i),]=winfit$pred #save the forecast
    #cat("iteration",(1+noos-i),"\n") #display iteration number (useful for slower ML methods)
  }
  
  #Some useful post-prediction misc stuff:
  real=Y #get actual values
  plot(real,type="l")
  lines(c(rep(NA,length(real)-noos),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  
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
    select(c(1, 3:12))
  return(ar.rolling.window(data_cv, noos, p, h))
}

ar1=cv_rolling(data_full,10,2,1) #1-step POOS AR(2) forecast


