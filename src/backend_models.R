rm(list=ls())

#install.packages("pacman")
pacman::p_load(tidyverse,
               readxl,
               zoo,
               sandwich,
               lsei,
               shiny,
               RSelenium,
               shinyWidgets,
               shinythemes,
               plotly)

data = read_excel("data/ROUTPUTQvQd.xlsx") 
data = data[-c(1:50), ] # remove data before 1959Q3 due to NAs

data_spread = read_excel("data/allmonth.xls")

data_spread = data_spread[-c(1:6, 781:795),]
data_spread = data_spread %>%
  select("Date", "Spread")

data_spread_mth <- ts(data_spread[,"Spread"], start = c(1959, 7), frequency = 12)
data_spread_qtr <- aggregate(data_spread_mth, nfrequency = 4, mean)
data_spread_qtr <- as.data.frame(data_spread_qtr)
X2 = embed(data_spread_qtr[,1], 2)
X2 = as.matrix(X2[,1] - X2[,2])

# user input:
# starting & ending quarter for testing, h-step
# no restriction for the length of interval, but earliest 150 quarters from 1959q3 -> 1997q1, latest 2023q4, length of cv is fixed at 50 quarters

# function: get necessary data 
# input: user select the end of the desired forecast interval
# output: get all the data for training, cv and testing

# sample input: 2003Q2, 2005Q4
get_data = function(start_q, end_q) {
  start_quarter = str_remove(start_q, ":")
  end_quarter = str_remove(end_q, ":")
  start_col_name = paste("ROUTPUT", str_sub(start_quarter, start = 3, end = 6), sep = "")
  end_col_name = paste("ROUTPUT", str_sub(end_quarter, start = 3, end = 6), sep = "")
  
  if (as.numeric(str_sub(end_quarter, start = 3, end = 4)) >= 97) {
    end_index <<- as.numeric(str_sub(end_quarter, start = 3, end = 4)) + as.numeric(str_sub(end_quarter, start = 6, end = 6)) / 4 + 1900
  } else {
    end_index <<- as.numeric(str_sub(end_quarter, start = 3, end = 4)) + as.numeric(str_sub(end_quarter, start = 6, end = 6)) / 4 + 2000
  }
  # 23q3 --> 23.75 --> 2023.75
  if (as.numeric(str_sub(start_quarter, start = 3, end = 4)) >= 97) {
    start_index <<- as.numeric(str_sub(start_quarter, start = 3, end = 4)) + as.numeric(str_sub(start_quarter, start = 6, end = 6)) / 4 + 1900
  } else {
    start_index <<- as.numeric(str_sub(start_quarter, start = 3, end = 4)) + as.numeric(str_sub(start_quarter, start = 6, end = 6)) / 4 + 2000
  }
  
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

# data_full = get_data("2003Q2","2005Q4")
# data_full = get_data("2018Q1","2023Q4")
# data_full = get_data("2020Q4", "2023Q4")
data_full = get_data("1997Q1", "2003Q4")

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


# get latest Y - from 2024q1
mse_data = data[,ncol(data)]

data_most_recent = mse_data %>% # extract last column
  rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
  mutate(gdp = as.numeric(gdp)) %>%
  drop_na() %>%
  mutate(loggdp = log(gdp))

temp=embed(data_most_recent$loggdp,2) #create lag of log(GDP) and align the original series

Y_recent=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference

# covid dummy AR
fitAR_dummy=function(Y,p,h){
  
  dum_neg=Y[, 3] # extract dummy
  dum_pos=Y[, 4]
  date = Y[, 2]
  Y=Y[, 1] #data without the dummy, gdp growth
  Y = as.matrix(Y)
  
  #Inputs: Y- predicted variable,  p - AR order, h -forecast horizon
  
  aux=embed(Y,p+h) #create 2 lags + forecast horizon shift (=h option)
  y=aux[,1] #  Y variable aligned/adjusted for missing data due to lags
  X=as.matrix(aux[,-c(1:(ncol(Y)*h))]) # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)  
  
  dum_neg=tail(dum_neg,length(y)) #cut the dummy to size to account for lost observations due to lags
  dum_pos=tail(dum_pos,length(y))
  
  # X=cbind(X, dum_neg, dum_pos)
  
  if(h==1){ 
    # ncol(X)-2 due to dummy
    X.out=tail(aux,1)[1:(ncol(X))] #retrieve last p observations if one-step forecast 
  }else{
    X.out=aux[,-c(1:(ncol(Y)*(h-1)))] #delete first (h-1) columns of aux,  
    X.out=tail(X.out,1)[1:(ncol(X))] #last p observations to predict T+1 
  }
  
  model=lm(y~X+dum_neg+dum_pos) #estimate direct h-step AR(p) by OLS with the dummies
  coef=coef(model)[1:(ncol(X)+1)] #extract coefficients
  
  pred=c(1,X.out)%*%coef #make a forecast using the last few observations: a direct h-step forecast.
  #note the addition of a constant to the test observation vector
  
  rmsfe=sqrt(sum(model$residuals^2)/nrow(X)) #get unadjusted rmsfe (ignoring estimation uncertainty)
  
  full_coef = model$coefficients
  full_coef[is.na(full_coef)] = 0 # replace NA with 0 for dummy not used
  
  return(list("model"=model,"pred"=pred,"coef"=full_coef, "rmsfe"=rmsfe)) #save estimated AR regression, prediction, and estimated coefficients
}


# cross validation - rolling window, adjusted code to add p as a parameter
ar.rolling.window=function(data_cv,Y,noos,p,h){ #equality here  means default inputs
  
  save.coef=matrix(NA,noos,p+1) #blank matrix for coefficients at each iteration (3=constant+ 2 lags)
  save.pred=matrix(NA,noos,1) #blank for forecasts
  real=matrix(NA,noos,1)
  neg_sign=matrix(NA,noos,1)
  pos_sign=matrix(NA,noos,1)
  for(i in 1:noos){ 
    # get real-time data
    temp_Y = data_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
      mutate(gdp = suppressWarnings(as.numeric(gdp))) %>%
      drop_na() %>%
      mutate(loggdp = log(gdp)) %>%
      pull(loggdp)
    temp_Y = as.matrix(temp_Y)
    index = nrow(temp_Y)
    
    temp_Y = temp_Y[i:nrow(temp_Y),]
    
    temp=embed(temp_Y,2) #create lag of log(GDP) and align the original series
    
    Y.window=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference
    
    
    winfit=fitAR(Y.window,p,h) #call the function to fit the AR and generate h-step forecast
    save.coef[i,]=winfit$coef #save estimated coefficients
    save.pred[i,]=winfit$pred #save the forecast
    
    real[i]=Y[index] #get actual values
    neg_sign[i]=ifelse(Y[index]<0 & winfit$pred>0, 
                       1,0)
    pos_sign[i]=ifelse(Y[index]>0 & winfit$pred<0, 1, 0)
  }
  
  rmse=sqrt(mean((real-save.pred)^2)) #compute RMSE
  mae=mean(abs(real-save.pred)) #compute MAE (Mean Absolute Error)
  signs = sum(neg_sign,pos_sign)/noos #no of signs predicted wrongly
  neg_signs = sum(neg_sign)/sum(real<0) #no of negative signs predicted wrongly
  abs_errors = abs(real-save.pred)
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs,"neg_signs"=neg_signs) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors,"real"=real,"abs_loss" = abs_errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}


# testing with dummy - rolling window, added covid dummy variable for 2020q1 to q2 gdp growth rate, and 2020q2 to q3 gdp growth rate
ar.rolling.window_covid=function(data_test,Y,noos,p,h){
  
  save.coef=matrix(NA,noos,p+1+2) #blank matrix for coefficients at each iteration (p+1=constant+ p lags), +2 for dummy
  save.pred=matrix(NA,noos,1) #blank for forecasts
  real=matrix(NA,noos,1)
  neg_sign=matrix(NA,noos,1)
  pos_sign = matrix(NA,noos,1)
  for(i in 1:noos){ 
    # get real-time data
    temp_Y = data_test %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
      mutate(gdp = suppressWarnings(as.numeric(gdp))) %>%
      drop_na() %>%
      mutate(loggdp = log(gdp)) %>%
      pull(loggdp)
    temp_Y = as.matrix(temp_Y)
    index = nrow(temp_Y)
    
    temp_Y = temp_Y[i:nrow(temp_Y),]
    
    temp=embed(temp_Y,2) #create lag of log(GDP) and align the original series
    #with it for the available
    
    Y.mat = as.matrix(400*(temp[,1]-temp[,2]))
    
    Y.window=data.frame(400*(temp[,1]-temp[,2])) %>% #GDP growth via log difference
      rename_with(.cols = 1, ~"gdpgrowth")
    
    # gdp growth rate from 2020q1 to 2020q2 -> gdp growth rate of 2020q2
    
    Y.window$date = seq(1959.75+(i-1)*0.25, by = 0.25, length.out = nrow(Y.window)) # add time column
    Y.window$dummy_neg = ifelse(Y.window$date == 2020.5, 1, 0)
    Y.window$dummy_pos = ifelse(Y.window$date == 2020.75, 1, 0)
    
    if (sum(Y.window$dummy_neg)==0 & sum(Y.window$dummy_pos==0)) {
      winfit=fitAR(Y.mat,p,h) #call the function to fit the AR and generate h-step forecast
      
      save.coef[i,]=c(winfit$coef,0,0) #save estimated coefficients, 0 for dummy since not involved
      save.pred[i,]=winfit$pred #save the forecast
      
    } else { # covid dummy involved
      winfit=fitAR_dummy(Y.window,p,h)
      
      save.coef[i,]=winfit$coef #save estimated coefficients
      save.pred[i,]=winfit$pred #save the forecast
    }
    
    real[i]=Y[index] #get actual values
    neg_sign[i]=ifelse(Y[index]<0 & winfit$pred>0, 
                       1,0)
    pos_sign[i]=ifelse(Y[index]>0 & winfit$pred<0, 1, 0)
  }
  
  rmse=sqrt(mean((real-save.pred)^2)) #compute RMSE
  mae=mean(abs(real-save.pred)) #compute MAE (Mean Absolute Error)
  signs = sum(neg_sign,pos_sign)/noos #no of signs predicted wrongly
  neg_signs = sum(neg_sign)/sum(real<0) #no of negative signs predicted wrongly
  abs_errors = abs(real-save.pred)
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs,"neg_signs"=neg_signs) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors,"real"=real,"abs_loss" = abs_errors)) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}


# function: rolling window - cross validation (50 quarters)
# input: data, noos = 50, p, h=1
# output: cross validation results in a list

cv_rolling = function(data_full, Y, noos = 50, p, h){
  data_cv = data_full %>%
    select(c(1, 2:51))
  if (start_index>=2020.5) {
    return(ar.rolling.window_covid(data_cv, Y, noos = 50, p, h))
  } else {
    return(ar.rolling.window(data_cv, Y = Y, noos=50, p, h))
  }
}

ar12=cv_rolling(data_full,Y_recent,noos=50,2,1) #1-step POOS AR(2) forecast

test_rolling = function(data_full, Y, noos = num_quarters, p, h){
  data_test = data_full %>%
    select(c(1, 52:ncol(data_full)))
  if (start_index<=2020.75 & end_index>=2020.5) { # only 2020.5 and 2020.75 are affected
    return(ar.rolling.window_covid(data_test, Y, noos = num_quarters, p, h))
  } else {
    return(ar.rolling.window(data_test, Y, noos = num_quarters, p, h))
  }
}


ar12=test_rolling(data_full,Y_recent,num_quarters,2,1) #1-step POOS AR(2) forecast




#combined forecast

ar_combined = function(data_full, h, test_fn, Y) {
  AR_preds = sapply(1:8, function(i) {return(test_fn(data_full, Y, p = i, h = h)$pred)})  
  AR_simple_combined = apply(AR_preds, 1, mean)
  no_obs = data_full %>%
    select(last_col()) %>%
    rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
    mutate(gdp = suppressWarnings(as.numeric(gdp))) %>%
    drop_na() %>%
    mutate(loggdp = log(gdp)) %>%
    nrow()
  real = Y[(no_obs-num_quarters+1):no_obs]
  rmse=sqrt(mean((real-AR_simple_combined)^2)) #compute RMSE
  mae=mean(abs(real-AR_simple_combined)) #compute MAE (Mean Absolute Error)
  neg_sign=sapply(1:nrow(AR_preds), function(i) {ifelse(real[i]<0 & AR_simple_combined[i]>0, 
                                                        1,0)})
  pos_sign=sapply(1:nrow(AR_preds), function(i) {ifelse(real[i]>0 & AR_simple_combined[i]<0, 1, 0)})
  signs = sum(neg_sign,pos_sign)/nrow(AR_preds) #no of signs predicted wrongly
  neg_signs = sum(neg_sign)/sum(real<0) #no of negative signs predicted wrongly
  abs_errors = abs(real-AR_simple_combined)
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs,"neg_signs"=neg_signs) #stack errors in a vector
  return(list("pred" = AR_simple_combined, "errors" = errors,"real"=real, "abs_loss" = abs_errors))
}

AR_combined1 = ar_combined(data_full, 1, test_rolling, Y_recent)




#granger-ramanathan with constant and constraints
ar_gr_combined = function(data_full, h, test_fn, Y) {
  cv_preds = sapply(1:8, function(i){
    cv_rolling(data_full, Y_recent, p = i, h = h)$pred})
  no_obs_cv = data_full %>%
    select(50) %>%
    rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
    mutate(gdp = suppressWarnings(as.numeric(gdp))) %>%
    drop_na() %>%
    mutate(loggdp = log(gdp)) %>%
    nrow()
  oosy = as.matrix(Y_recent[(no_obs_cv-48):(no_obs_cv+1)])
  
  e_mat = diag(9) #8 AR models
  e_mat[1, 1] = 0
  X = cbind(rep(1, 50), cv_preds) #50 predictions
  weights = lsei(X, oosy, c=c(0, rep(1,8)), d=1, e=e_mat, f=rep(0,9))
  AR_preds = sapply(1:8, function(i) {return(test_fn(data_full, Y, p = i, h = h)$pred)})  
  AR_gr_combined = sapply(1:8, function(i) {return(weights[i+1]*AR_preds[,i])})
  AR_gr_combined = apply(AR_gr_combined, 1, sum) + weights[1]
  no_obs = data_full %>%
    select(last_col()) %>%
    rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
    mutate(gdp = suppressWarnings(as.numeric(gdp))) %>%
    drop_na() %>%
    mutate(loggdp = log(gdp)) %>%
    nrow()
  real = Y[(no_obs-num_quarters+1):no_obs]
  rmse=sqrt(mean((real-AR_gr_combined)^2)) #compute RMSE
  mae=mean(abs(real-AR_gr_combined)) #compute MAE (Mean Absolute Error)
  neg_sign=sapply(1:nrow(AR_preds), function(i) {ifelse(real[i]<0 & AR_gr_combined[i]>0, 
                                                        1,0)})
  pos_sign=sapply(1:nrow(AR_preds), function(i) {ifelse(real[i]>0 & AR_gr_combined[i]<0, 1, 0)})
  signs = sum(neg_sign,pos_sign)/nrow(AR_preds) #no of signs predicted wrongly
  neg_signs = sum(neg_sign)/sum(real<0) #no of negative signs predicted wrongly
  abs_errors = abs(real-AR_gr_combined)
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs,"neg_signs"=neg_signs) #stack errors in a vector
  return(list("pred" = AR_gr_combined, "errors" = errors,"real"=real, "abs_loss"=abs_errors,"weights"=weights))
}

ar_combined3.1 = ar_gr_combined(data_full, 1, test_rolling, Y_recent)


#find interval boundaries for plotting
intervals = function(x, p, rmsfe) {
  alpha = qnorm(p+(1-p)/2, lower.tail = TRUE)
  upper = x + alpha*rmsfe
  lower = x - alpha*rmsfe
  
  boundaries = data.frame(upper, x, lower)
  return(boundaries)
}





# ADL - real personal consumption, same transformation as gdp level
rpc = read_excel("data/RCONQvQd.xlsx")
rpc = rpc[-c(1:50), ] # remove data before 1959Q3 due to NAs


get_data_rpc = function(start_q, end_q) {
  start_quarter = str_remove(start_q, ":")
  end_quarter = str_remove(end_q, ":")
  start_col_name = paste("RCON", str_sub(start_quarter, start = 3, end = 6), sep = "")
  end_col_name = paste("RCON", str_sub(end_quarter, start = 3, end = 6), sep = "")
  
  rpc_full = rpc %>%
    select("DATE" : end_col_name)
  
  if (as.numeric(str_sub(end_quarter, start = 3, end = 4)) >= 97) {
    end_index <<- as.numeric(str_sub(end_quarter, start = 3, end = 4)) + as.numeric(str_sub(end_quarter, start = 6, end = 6)) / 4 + 1900
  } else {
    end_index <<- as.numeric(str_sub(end_quarter, start = 3, end = 4)) + as.numeric(str_sub(end_quarter, start = 6, end = 6)) / 4 + 2000
  }
  # 23q3 --> 23.75 --> 2023.75
  if (as.numeric(str_sub(start_quarter, start = 3, end = 4)) >= 97) {
    start_index <<- as.numeric(str_sub(start_quarter, start = 3, end = 4)) + as.numeric(str_sub(start_quarter, start = 6, end = 6)) / 4 + 1900
  } else {
    start_index <<- as.numeric(str_sub(start_quarter, start = 3, end = 4)) + as.numeric(str_sub(start_quarter, start = 6, end = 6)) / 4 + 2000
  }
  
  num_quarters <<- (end_index - start_index) * 4 +1
  
  num_col = num_quarters + 50 
  # extract last num_col columns up til the end period: 50 quarters in cross validation, num_quarters in testing
  rpc_full = rpc_full[, (ncol(rpc_full)-num_col+1):ncol(rpc_full)]
  
  # adding the date column
  rpc_full = cbind(rpc$DATE, rpc_full)
  
  return(rpc_full)
}

# rpc_full = get_data_rpc("2003Q2","2005Q4")
# rpc_full = get_data_rpc("2018Q1", "2023Q4")
# rpc_full = get_data_rpc("2020Q4", "2023Q4")
rpc_full = get_data_rpc("1997:Q1", "2003:Q4")


fitADL=function(Y,X1,X2,p_y,p_x1,p_x2,h){
  
  max_lags = max(p_y, p_x1, p_x2)
  
  aux=embed(Y,p_y+h) #create p_y lags + forecast horizon shift (=h option)
  y=aux[,1] #  Y variable aligned/adjusted for missing data due to lags
  lags_y=as.matrix(aux[,-c(1:(ncol(Y)*h))]) # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)  
  diff = max_lags - p_y + 1
  y = y[diff:length(y)]
  lags_y = as.matrix(lags_y[diff:nrow(lags_y),])
  
  #if (p_x1>0) {
  aux_x1=embed(X1,p_x1+h) # create lags for x1
  lags_x1=as.matrix(aux_x1[,-c(1:(ncol(X1)*h))]) # lags of x1
  diff = max_lags - p_x1 + 1
  lags_x1 = as.matrix(lags_x1[diff:nrow(lags_x1),])
  
  
  #if (p_x2>0) {
  aux_x2 = embed(X2, p_x2+h)
  lags_x2 = as.matrix(aux_x2[, -c(1:h)])
  diff = max_lags - p_x2 + 1
  lags_x2 = as.matrix(lags_x2[diff:nrow(lags_x2),])
  
  
  X = cbind(lags_y, lags_x1, lags_x2) # column binding all the predictors (lags of Y and lags of X1)
  
  if(h==1){ 
    new_y = matrix(tail(aux,1)[1:ncol(lags_y)],nrow=1) # extract y_t, y_t-1...
    if (p_x1>0) {new_x1 = matrix(tail(aux_x1,1)[1:ncol(lags_x1)],nrow=1)} # extract x_t, x_t-1...
    if (p_x2>0) {new_x2 = matrix(tail(aux_x2, 1)[1:ncol(lags_x2)], nrow = 1)}
    
    if (p_x1==0 & p_x2==0) {
      X.out = new_y
    }
    else if (p_x1 == 0 & p_x2>0) {
      X.out = c(new_y, new_x2)
    }
    else if (p_x1>0 & p_x2==0) {
      X.out = c(new_y, new_x1)
    }
    else {
      X.out = c(new_y, new_x1, new_x2)
    }
  }else{
    temp_y=aux[,-c(1:(ncol(Y)*(h-1)))] #delete first (h-1) columns of aux
    temp_x1=aux_x1[,-c(1:(ncol(X1)*(h-1)))] #delete first (h-1) columns of aux_x1
    temp_x2 = aux_x2[,-c(1:(ncol(X2)*(h-1)))]
    new_y = matrix(tail(temp_y,1)[1:ncol(lags_y)],nrow=1)
    if (p_x1>0) {new_x1 = matrix(tail(temp_x1,1)[1:ncol(lags_x1)],nrow=1)}
    if (p_x2>0) {new_x2 = matrix(tail(temp_x2,1)[1:ncol(lags_x2)],nrow=1)}
    #X.out=c(new_y,new_x1,new_x2) #last p observations to predict T+1 
    if (p_x1==0 & p_x2==0) {
      X.out = new_y
    }
    else if (p_x1 == 0 & p_x2>0) {
      X.out = c(new_y, new_x2)
    }
    else if (p_x1>0 & p_x2==0) {
      X.out = c(new_y, new_x1)
    }
    else {
      X.out = c(new_y, new_x1, new_x2)
    }
  }
  
  model=lm(y~X) #estimate direct h-step AR(p) by OLS 
  coef=coef(model) #extract coefficients
  
  pred=c(1,X.out)%*%coef #make a forecast using the last few observations: a direct h-step forecast.
  #note the addition of a constant to the test observation vector
  
  rmsfe=sqrt(sum(model$residuals^2)/nrow(X)) #get unadjusted rmsfe (ignoring estimation uncertainty)
  
  return(list("model"=model,"pred"=pred,"coef"=coef, "rmsfe"=rmsfe)) #save estimated AR regression, prediction, and estimated coefficients
}



fitADL_dummy=function(Y,X1,X2,p_y,p_x1,p_x2,h){
  
  max_lags = max(p_y, p_x1, p_x2)
  
  dum_neg=Y[, 3] # extract dummy
  dum_pos=Y[, 4]
  date = Y[, 2]
  Y=Y[, 1] #data without the dummy, gdp growth
  Y = as.matrix(Y)
  
  aux=embed(Y,p_y+h) #create p_y lags + forecast horizon shift (=h option)
  y=aux[,1] #  Y variable aligned/adjusted for missing data due to lags
  lags_y=as.matrix(aux[,-c(1:(ncol(Y)*h))]) # lags of Y (predictors) corresponding to forecast horizon (prevent leakage)  
  diff = max_lags - p_y + 1
  y = y[diff:length(y)]
  lags_y = as.matrix(lags_y[diff:nrow(lags_y),])
  
  
  aux_x1=embed(X1,p_x1+h) # create lags for x1
  lags_x1=as.matrix(aux_x1[,-c(1:(ncol(X1)*h))]) # lags of x1
  diff = max_lags - p_x1 + 1
  lags_x1 = as.matrix(lags_x1[diff:nrow(lags_x1),])
  
  
  aux_x2 = embed(X2, p_x2+h)
  lags_x2 = as.matrix(aux_x2[, -c(1:h)])
  diff = max_lags - p_x2 + 1
  lags_x2 = as.matrix(lags_x2[diff:nrow(lags_x2),])
  
  
  X = cbind(lags_y, lags_x1, lags_x2) # column binding all the predictors (lags of Y and lags of X1)
  
  dum_neg=tail(dum_neg,nrow(X)) #cut the dummy to size to account for lost observations due to lags
  dum_pos=tail(dum_pos,nrow(X))
  
  if(h==1){ 
    new_y = matrix(tail(aux,1)[1:ncol(lags_y)],nrow=1) # extract y_t, y_t-1...
    if (p_x1>0) {new_x1 = matrix(tail(aux_x1,1)[1:ncol(lags_x1)],nrow=1)} # extract x_t, x_t-1...
    if (p_x2>0) {new_x2 = matrix(tail(aux_x2, 1)[1:ncol(lags_x2)], nrow = 1)}
    if (p_x1==0 & p_x2==0) {
      X.out = new_y
    }
    else if (p_x1 == 0 & p_x2>0) {
      X.out = c(new_y, new_x2)
    }
    else if (p_x1>0 & p_x2==0) {
      X.out = c(new_y, new_x1)
    }
    else {
      X.out = c(new_y, new_x1, new_x2)
    }
  }else{
    temp_y=aux[,-c(1:(ncol(Y)*(h-1)))] #delete first (h-1) columns of aux
    temp_x1=aux_x1[,-c(1:(ncol(X1)*(h-1)))] #delete first (h-1) columns of aux_x1
    temp_x2 = aux_x2[,-c(1:(ncol(X2)*(h-1)))]
    new_y = matrix(tail(temp_y,1)[1:ncol(lags_y)],nrow=1)
    if (p_x1>0) {new_x1 = matrix(tail(temp_x1,1)[1:ncol(lags_x1)],nrow=1)}
    if (p_x2>0) {new_x2 = matrix(tail(temp_x2,1)[1:ncol(lags_x2)],nrow=1)}
    #X.out=c(new_y,new_x1,new_x2) #last p observations to predict T+1 
    if (p_x1==0 & p_x2==0) {
      X.out = new_y
    }
    else if (p_x1 == 0 & p_x2>0) {
      X.out = c(new_y, new_x2)
    }
    else if (p_x1>0 & p_x2==0) {
      X.out = c(new_y, new_x1)
    }
    else {
      X.out = c(new_y, new_x1, new_x2)
    }
  }
  
  model=lm(y~X+dum_neg+dum_pos) #estimate direct h-step AR(p) by OLS with the dummy
  coef=coef(model)[1:(ncol(X)+1)] #extract coefficients
  
  pred=c(1,X.out)%*%coef #make a forecast using the last few observations: a direct h-step forecast.
  #note the addition of a constant to the test observation vector
  
  rmsfe=sqrt(sum(model$residuals^2)/nrow(X)) #get unadjusted rmsfe (ignoring estimation uncertainty)
  
  full_coef = model$coefficients
  full_coef[is.na(full_coef)] = 0 # replace NA with 0 for dummy not used
  
  return(list("model"=model,"pred"=pred,"coef"=full_coef, "rmsfe"=rmsfe)) #save estimated AR regression, prediction, and estimated coefficients
}



# cross validation - rolling window, adjusted code to add p as a parameter
adl.rolling.window=function(data_cv,rpc_cv,spread,Y,noos,p_y,p_x1,p_x2,h=1){ #equality here  means default inputs
  
  save.coef=matrix(NA,noos,p_y+p_x1+p_x2+1) #blank matrix for coefficients at each iteration (3=constant+ 2 lags)
  save.pred=matrix(NA,noos,1) #blank for forecasts
  real=matrix(NA,noos,1)
  neg_sign=matrix(NA,noos,1)
  pos_sign=matrix(NA,noos,1)
  for(i in 1:noos){ 
    # get real-time data
    temp_Y = data_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
      mutate(gdp = suppressWarnings(as.numeric(gdp))) %>%
      drop_na() %>%
      mutate(loggdp = log(gdp)) %>%
      pull(loggdp)
    temp_Y = as.matrix(temp_Y)
    
    temp_Y = temp_Y[i:nrow(temp_Y),]
    
    temp=embed(temp_Y,2) #create lag of log(GDP) and align the original series with it for the available
    Y.window=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference
    no_qtrs = nrow(Y.window)+i-1
    
    
    temp_X1 = rpc_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"rpc") %>%  # renaming columns
      mutate(rpc = suppressWarnings(as.numeric(rpc))) %>%
      drop_na() %>%
      mutate(logrpc = suppressWarnings(log(rpc))) %>%
      pull(logrpc)
    temp_X1 = as.matrix(temp_X1)
    
    temp_X1 = temp_X1[i:nrow(temp_X1),]
    
    tempX1=embed(temp_X1,2) #create lag of log(rpc) and align the original series
    X1.window=as.matrix(400*(tempX1[,1]-tempX1[,2])) #rpc growth via log difference
    
    
    X2.window = as.matrix(spread[i:no_qtrs])
    
    
    winfit=fitADL(Y.window,X1.window,X2.window,p_y,p_x1,p_x2,h) #call the function to fit the AR(2) and generate h-step forecast
    save.coef[(i),]=winfit$coef #save estimated coefficients
    save.pred[(i),]=winfit$pred #save the forecast
    
    
    real[i] = Y_recent[no_qtrs+1]
    neg_sign[i]=ifelse(Y[no_qtrs+1]<0 & winfit$pred>0, 
                       1,0)
    pos_sign[i]=ifelse(Y[no_qtrs+1]>0 & winfit$pred<0, 1, 0)
  }
  
  
  rmse=sqrt(mean((real-save.pred)^2)) #compute RMSE
  mae=mean(abs(real-save.pred)) #compute MAE (Mean Absolute Error)
  neg_signs=sum(neg_sign)/sum(real<0)
  signs=sum(neg_sign,pos_sign)/noos
  abs_errors = abs(real-save.pred)
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs,"neg_signs"=neg_signs) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors,"real"=real,"abs_loss"=abs_errors,"lags"=c(p_y,p_x1,p_x2))) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}


adl.rolling.window_covid=function(data_cv,rpc_cv,spread,Y,noos,p_y,p_x1,p_x2,h=1){ #equality here  means default inputs
  
  save.coef=matrix(NA,noos,p_y+p_x1+p_x2+1+2) #blank matrix for coefficients at each iteration (3=constant+ 2 lags), +2 for dummy
  save.pred=matrix(NA,noos,1) #blank for forecasts
  real=matrix(NA,noos,1)
  neg_sign=matrix(NA,noos,1)
  pos_sign=matrix(NA,noos,1)
  for(i in 1:noos){ 
    # get real-time data
    temp_Y = data_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
      mutate(gdp = suppressWarnings(as.numeric(gdp))) %>%
      drop_na() %>%
      mutate(loggdp = log(gdp)) %>%
      pull(loggdp)
    temp_Y = as.matrix(temp_Y)
    
    temp_Y = temp_Y[i:nrow(temp_Y),]
    
    temp=embed(temp_Y,2) #create lag of log(GDP) and align the original series with it for the available
    Y.mat=as.matrix(400*(temp[,1]-temp[,2])) #GDP growth via log difference
    no_qtrs = nrow(Y.mat)+i-1
    
    
    Y.window=data.frame(400*(temp[,1]-temp[,2])) %>% #GDP growth via log difference
      rename_with(.cols = 1, ~"gdpgrowth")
    Y.window$date = seq(1959.75+(i-1)*0.25, by = 0.25, length.out = nrow(Y.window)) # add time column
    Y.window$dummy_neg = ifelse(Y.window$date == 2020.5, 1, 0)
    Y.window$dummy_pos = ifelse(Y.window$date == 2020.75, 1, 0)
    
    temp_X1 = rpc_cv %>%
      select(i+1) %>%
      rename_with(.cols = 1, ~"rpc") %>%  # renaming columns
      mutate(rpc = suppressWarnings(as.numeric(rpc))) %>%
      drop_na() %>%
      mutate(logrpc = suppressWarnings(log(rpc))) %>%
      pull(logrpc)
    temp_X1 = as.matrix(temp_X1)
    
    temp_X1 = temp_X1[i:nrow(temp_X1),]
    
    tempX1=embed(temp_X1,2) #create lag of log(rpc) and align the original series
    X1.window=as.matrix(400*(tempX1[,1]-tempX1[,2])) #rpc growth via log difference
    
    
    X2.window = as.matrix(spread[i:no_qtrs])
    
    
    if (sum(Y.window$dummy_neg)==0 & sum(Y.window$dummy_pos==0)) {
      winfit=fitADL(Y.mat,X1.window,X2.window,p_y,p_x1,p_x2,h) #call the function to fit the AR and generate h-step forecast
      save.coef[(i),]=c(winfit$coef,0,0) #save estimated coefficients, 0 for dummy since not involved
      save.pred[(i),]=winfit$pred #save the forecast
      
    } else { # covid dummy involved
      winfit=fitADL_dummy(Y.window,X1.window,X2.window,p_y,p_x1,p_x2,h)
      save.coef[(i),]=winfit$coef #save estimated coefficients
      save.pred[(i),]=winfit$pred #save the forecast
    }
    
    real[i] = Y_recent[no_qtrs+1]
    neg_sign[i]=ifelse(Y[no_qtrs+1]<0 & winfit$pred>0, 
                       1,0)
    pos_sign[i]=ifelse(Y[no_qtrs+1]>0 & winfit$pred<0, 1, 0)
  }
  
  
  rmse=sqrt(mean((real-save.pred)^2)) #compute RMSE
  mae=mean(abs(real-save.pred)) #compute MAE (Mean Absolute Error)
  neg_signs=sum(neg_sign)/sum(real<0)
  signs=sum(neg_sign,pos_sign)/noos
  abs_errors = abs(real-save.pred)
  errors=c("rmse"=rmse,"mae"=mae,"signs"=signs,"neg_signs"=neg_signs) #stack errors in a vector
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors,"real"=real,"abs_loss"=abs_errors,"lags"=c(p_y,p_x1,p_x2))) #return forecasts, history of estimated coefficients, and RMSE and MAE for the period.
}

adl_rolling_test = adl.rolling.window(data_full, rpc_full, X2, Y_recent, 50, 2, 0,2)

adl_rolling_test_covid = adl.rolling.window_covid(data_full, rpc_full, X2, Y_recent, 50, 2, 2,0)


# function: rolling window - cross validation (50 quarters)
# input: data, noos = 10, p, h=1
# output: cross validation results in a list

cv_rolling_adl = function(data_full, rpc_full, spread, Y, noos = 50, p_y, p_x1, p_x2, h = 1){
  data_cv = data_full %>%
    select(c(1, 2:51))
  rpc_cv = rpc_full %>%
    select(c(1, 2:51))
  if (start_index>=2020.5) {
    return(adl.rolling.window_covid(data_cv, rpc_cv, spread, Y, noos = 50, p_y, p_x1, p_x2, h))
  } else {
    return(adl.rolling.window(data_cv, rpc_cv, spread, Y, noos = 50, p_y, p_x1, p_x2, h))
  }
}

adl22=cv_rolling_adl(data_full,rpc_full,X2,Y_recent,50,2,0,3,1) #1-step POOS ADL(2,0,3) forecast



test_rolling_adl = function(data_full, rpc_full, spread, Y, noos = num_quarters, p_y, p_x1, p_x2, h = 1){
  data_test = data_full %>%
    select(c(1, 52:ncol(data_full)))
  rpc_test = rpc_full %>%
    select(c(1, 52:ncol(rpc_full)))
  if (start_index<=2020.75 & end_index>=2020.5) { # only 2020.5 and 2020.75 are affected
    return(adl.rolling.window_covid(data_test, rpc_test, spread, Y, noos = num_quarters, p_y, p_x1, p_x2, h))
  } else {
    return(adl.rolling.window(data_test, rpc_test, spread, Y, noos = num_quarters, p_y, p_x1, p_x2, h))
  }
}

adl22=test_rolling_adl(data_full,rpc_full,X2,Y_recent,num_quarters,3,2,2,1) #1-step POOS ADL(3,2,2) forecast



# DM test

dm_test = function(l1, l2, h) {
  dt = l1 - l2
  dmreg = lm(dt~1)
  x=dmreg$coefficients/sqrt(NeweyWest(dmreg,lag=num_quarters^(1/3))) #form the DM t-statistic
  if (num_quarters<50) {
    x[1,1] = x[1,1]*sqrt(1+(1/num_quarters)*(1-2*h) + (1/num_quarters^2)*h*(h-1))
  }
  return(x[1,1]) # extract result
}

dm_test(ar12$abs_loss, AR_combined1$abs_loss, 1)

