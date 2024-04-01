library(shiny)
source("model_ar copy.R")

server = function(input, output, session) {
  #data_used = get_data(input$end_quarter)
  data_used = reactive({
    get_data(input$end_quarter)
  })
  
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
  
  #reactive exp for computing AR model erros 
  AR_models = reactive({
    sapply(1:8, function(i){
      cv_fn()(data_full = data_used(), p = i, h = input$h)$errors
    })
  })
  
  rank_ar = reactive({
    order(AR_models()[1,])
  })
  
  best_ar_lag = reactive({
    rank_ar()[1]
  })
  
  
  #performance metrics
  #code
  output$rmsfe = renderText({
    best_lag = best_ar_lag() #reactive best AR lag
    rmsfe_val = test_fn()(data_full = data_used(), p = best_lag, h = input$h)$errors[1]
    paste("RMSFE for the best AR model (lag", best_lag, "):", rmsfe_val)
  })
  
  #get h-step forecasts for test window
  forecasts <- reactive({
    if (input$model_type == "AR") {
      model <- test_fn()(data_used(), best_ar_lag(), input$h)
      list(pred = model$pred, error = model$error)
    }
  })
  #reactive for intervals if needed 
  forecast_intervals <- reactive({
    if (input$model_type == "AR") {
      intervals(forecasts()$pred, input$sig_level, forecasts()$rmsfe)
    }
  })
  
  #reactive plot 
  output$plot <- renderPlot({
    preds <- forecasts()$pred
    year_start <- year_end() - ifelse(quarter_end() > 2, 3, 4)
    quarter_start <- ifelse(quarter_end() == 1, 3, 
                            ifelse(quarter_end() == 2, 4, 
                                   ifelse(quarter_end() == 3, 1, 2)))
    year_start_pred <- year_end() - ifelse(quarter_end() > 3, 2, 3)
    quarter_start_pred <- ifelse(quarter_end() == 1, 4, 
                                 ifelse(quarter_end() == 2, 1, 
                                        ifelse(quarter_end() == 3, 2, 3)))
    true_ts <- ts(true_values(), start = c(year_start, quarter_start), end = c(year_end(), quarter_end()), frequency = 4)
    forecast_ts <- ts(preds, start = c(year_start_pred, quarter_start_pred), end = c(year_end(), quarter_end()), frequency = 4)
    
    plot.ts(true_ts, main = "h-step Forecasts", col = "black", ylab = "GDP growth")
    lines(forecast_ts, col = "red", lwd = 1.8)
    
    # Add intervals to the plot if they are part of the output
    if (input$model_type == "AR" && !is.null(forecast_intervals())) {
      upper_ts <- ts(forecast_intervals()[,1], start = c(year_start_pred, quarter_start_pred), end = c(year_end(), quarter_end()), frequency = 4)
      lower_ts <- ts(forecast_intervals()[,3], start = c(year_start_pred, quarter_start_pred), end = c(year_end(), quarter_end()), frequency = 4)
      
      lines(upper_ts, col = "blue", lwd = 1.8)
      lines(lower_ts, col = "blue", lwd = 1.8)
    }
    
    legend("bottomleft", legend = c("True values", "Forecasts", "Upper bound of interval", "Lower bound of interval"), 
           col = c("black", "red", "blue", "blue"), lwd = 1.8, bty = "n")
  })
  
}
