library(shiny)
source("model_ar copy.R")

server = function(input, output, session) {
  #data_used = get_data(input$end_quarter)
  rval_data_used = reactive({
    get_data(input$start_quarter, input$end_quarter)
  })
  rval_start_quarter = reactive({str_remove(input$start_quarter, ":")})
  rval_end_quarter = reactive({str_remove(input$end_quarter, ":")})
  
  start_quarter = rval_start_quarter()
  end_quarter = rval_end_quarter()
  data_used = rval_data_used()

  
  #dynamically choosing window type based on user input
  cv_fn <- reactive({
    if(input$window == "rolling"){
      cv_rolling
    } else if (input$window == "expanding"){
      cv_expanding
    }
  })
  test_fn = reactive({
    if (input$window == "rolling"){
      test_rolling
    }
    else if (input$window == "expanding"){
      test_expanding
    }
  })
  
  #reactive exp for computing AR model errors 
  rval_AR_models = reactive({
    sapply(1:8, function(i){
      cv_fn()(data_full = data_used, Y_recent, p = i, h = input$h)
    })
  })
  
  AR_models = rval_AR_models()
  AR_errors = AR_models$errors
  
  rank_ar = reactive({
    order(AR_errors[1,])
  })
  
  best_ar_lag = reactive({
    rank_ar()[1]
  })
  
  
  #performance metrics
  #code
  benchmark_AR = reactive({
    test_fn()(data_full = data_used, Y_recent, p = best_ar_lag(), h = input$h)
  })
  
  
  output$rmsfe = renderText({
    paste("RMSFE for the best AR model (lag", best_ar_lag(), "):", benchmark_AR()$errors[1])
  })
  
  output$mae <- renderText({
    paste("MAE for the best AR model (lag", best_ar_lag(), "):", benchmark_AR()$errors[2])
  })
  
  output$pct_signs_wrong <- renderText(({
    paste("Percentage of signs predicted wrongly for the best AR model (lag", 
          best_ar_lag(), "):", benchmark_AR()$errors[3])
  }))
  
  #get h-step forecasts for test window
  # model <- reactive({
  #   if (input$model_type == "AR") {
  #     test_fn()(data_used(), best_ar_lag(), input$h)
  #     list(pred = model$pred, error = model$error)
  #   }
  # })
  
  build_model = function(type, h) {
    if (type == "AR") {
      model = test_fn(data_used, Y_recent, p = best_ar_lag, h = h)
    }
    else if (type == "ADL") {
      rval_rpc_used = reactive({
        get_data_rpc(input$start_quarter, input$end_quarter)
      })
      rpc_used = rval_rpc_used()
      spread_used = X2
      #cv for ADL
      ar_lag = best_ar_lag()
      x1_lags = c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
      x2_lags = c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
      combs = data.frame(x1_lags, x2_lags)
      ADL_models = reactive({
        sapply(1:16, function(i){
        cv_rolling_adl(data_full = data_used,rpc_full = rpc_used,spread = spread_used, Y_recent,p_y = ar_lag, p_x1 = combs[i,1], p_x2 = combs[i,2], h = h)
      })
      })
      ADL_errors = ADL_models()$errors[1]
      rank_adl = order(AR_errors[1,])
      
      best_adl_lag = combs[rank_adl[1,]]
        
      #test using best ADL model
      model = test_rolling_adl(data_used, rpc_used, spread_used,Y_recent,p_y = ar_lag, p_x1 = best_adl_lag[1], p_x2 = best_adl_lag[2], h = h)
    }
    else if (type == "Simple Average") {
      model = ar_combined(data_used, h, test_fn(), Y_recent)
    }
    # else if (type == "Bates-Granger") {
    #   cv_errors_ar = AR_errors[1,]
    #   model = ar_combined_bg(data_used, input$h, cv_errors_ar, test_fn())
    # }
    else if (type == "Granger-Ramanathan") {
      no_obs_cv = data_used %>%
        select(50) %>%
        rename_with(.cols = 1, ~"gdp") %>%  # renaming columns
        mutate(gdp = as.numeric(gdp)) %>%
        drop_na() %>%
        mutate(loggdp = log(gdp)) %>%
        nrow()
      oosy = Y_recent[(no_obs_cv-49):no_obs_cv+1]
      model = ar_gr_combined(data_used, h, AR_models$pred, oosy, test_fn(), Y_recent)
    }
  }
  
  model = reactive({build_model(input$model_type, input$h)})
  
  forecasts = model()$pred
  #output$rmsfe_chosen = model()$errors[1]
  #output$mae_chosen = model()$errors[2]
  #output$pct_signs_wrong_chosen = model()$errors[3]
  
  #reactive for intervals if needed 
  # forecast_intervals <- reactive({
  #  intervals(forecasts$pred, input$sig_level, rmsfe)
  # })
  
  #reactive plot 
  # output$plot <- renderPlot({
  #   preds <- forecasts()$pred
  #   year_start <- year_end() - ifelse(quarter_end() > 2, 3, 4)
  #   quarter_start <- ifelse(quarter_end() == 1, 3, 
  #                           ifelse(quarter_end() == 2, 4, 
  #                                  ifelse(quarter_end() == 3, 1, 2)))
  #   year_start_pred <- year_end() - ifelse(quarter_end() > 3, 2, 3)
  #   quarter_start_pred <- ifelse(quarter_end() == 1, 4, 
  #                                ifelse(quarter_end() == 2, 1, 
  #                                       ifelse(quarter_end() == 3, 2, 3)))
  #   true_ts <- ts(true_values(), start = c(year_start, quarter_start), end = c(year_end(), quarter_end()), frequency = 4)
  #   forecast_ts <- ts(preds, start = c(year_start_pred, quarter_start_pred), end = c(year_end(), quarter_end()), frequency = 4)
  #   
  #   plot.ts(true_ts, main = "h-step Forecasts", col = "black", ylab = "GDP growth")
  #   lines(forecast_ts, col = "red", lwd = 1.8)
  #   
  #   # Add intervals to the plot if they are part of the output
  #   if (input$model_type == "AR" && !is.null(forecast_intervals())) {
  #     upper_ts <- ts(forecast_intervals()[,1], start = c(year_start_pred, quarter_start_pred), end = c(year_end(), quarter_end()), frequency = 4)
  #     lower_ts <- ts(forecast_intervals()[,3], start = c(year_start_pred, quarter_start_pred), end = c(year_end(), quarter_end()), frequency = 4)
  #     
  #     lines(upper_ts, col = "blue", lwd = 1.8)
  #     lines(lower_ts, col = "blue", lwd = 1.8)
  #   }
  #   
  #   legend("bottomleft", legend = c("True values", "Forecasts", "Upper bound of interval", "Lower bound of interval"), 
  #          col = c("black", "red", "blue", "blue"), lwd = 1.8, bty = "n")
  # })
  
  
  #plot for chosen model
  #Syntax: ts(object, start=startdate, end=enddate, freq=frequency (periods per year))
  
  output$plot = renderPlot({
    rval_year_end = reactive({
      as.numeric(str_sub(end_quarter, start = 3, end = 4))})
    rval_quarter_end = reactive({as.numeric(str_sub(end_quarter, start = 6, end = 6))})
    year_end = rval_year_end()
    quarter_end = rval_quarter_end()
    
    rval_year_start = reactive({as.numeric(str_sub(start_quarter, start = 3, end = 4))})
    rval_quarter_start = reactive({as.numeric(str_sub(start_quarter, start = 6, end = 6))})
    year_start = rval_year_start()
    quarter_start = rval_quarter_start()
    
    year_start_pred = year_start - 2
    quarter_start_pred = quarter_start
    last_obs = data_used %>% select(last_col()) %>% rename_with(.cols = 1, ~"gdp") %>%
      filter(!is.na(as.numeric(gdp))) %>% nrow()
    true_values = tail(Y_recent[1:last_obs+1,], num_quarters+8) #add some context before test window
    # dates1 = tail(mse_data[1:last_obs+1,], 15)
    # dates2 = tail(mse_data[1:last_obs+1,], 10)
    # plot_data = as.data.frame(c(true_values, dates1))
    # plot_data_2 = as.data.frame(forecasts, dates2)
  
  
    true_ts = ts(true_values, start = c(year_start, quarter_start), end = c(year_end, quarter_end), frequency = 4)
    forecast.ts = ts(forecasts, start = c(year_start_pred, quarter_start_pred), end = c(year_end, quarter_end), frequency = 4)
    #upper bound interval
    #lower bound interval
    
    forecast_intervals = reactive({intervals(forecasts, input$sig_level, rmsfe)})
    upper.ts = ts(forecast_intervals()[,1], start = c(year_start, quarter_start), end = c(year_end, quarter_end), frequency = 4)
    lower.ts = ts(forecast_intervals()[,3], start = c(year_start, quarter_start), end = c(year_end, quarter_end), frequency = 4)
    
    plot.ts(true_ts, main = "h-step Forecasts", cex.axis=1.5, lwd=1.8, col="black", ylab="GDP growth")
    points(forecast.ts, type = "l", col = "red", lwd = 1.8)
    points(upper.ts, type = "l", col = "blue", lwd = 1.8)
    points(lower.ts, type = "l", col = "blue", lwd = 1.8)
    legend("bottomleft", legend = c("True values", "Forecasts", "Upper bound of interval", "Lower bound of interval"))
    
  })
  
}

#add more performance metrics 

# true_ts_test = ts(tail(Y_recent[1:187,],19), start = c(2001,2), end = c(2005,4),frequency = 4)
# forecast.ts_test = ts(ar12$pred, start = c(2003, 2), end = c(2005, 4), frequency = 4)
# upper.ts_test = ts(int[,1], start = c(2003, 2), end = c(2005, 4), frequency = 4)
# lower.ts_test = ts(int[,3], start = c(2003, 2), end = c(2005, 4), frequency = 4)
# 
# plot.ts(true_ts_test, main = "h-step Forecasts", cex.axis=1.5, lwd=1.8, col="black", ylab="GDP growth")
# points(forecast.ts_test, type = "l", col = "red", lwd = 1.8)
# points(upper.ts_test, type = "l", col = "blue", lwd = 1.8, alpha = 0.5)
# points(lower.ts_test, type = "l", col = "blue", lwd = 1.8, alpha = 0.5)
# legend("bottomleft", legend = c("True values", "Forecasts", "Upper bound of interval", "Lower bound of interval"))

