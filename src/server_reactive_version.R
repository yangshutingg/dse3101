library(shiny)
library(ggplot2)
library(plotly)
source("src/backend_models.R")

server = function(input, output, session) {
  #data_used = get_data(input$end_quarter)
  rval_data_used = reactive({
    get_data(input$start_quarter, input$end_quarter)
  })
  rval_start_quarter = reactive({str_remove(input$start_quarter, ":")})
  rval_end_quarter = reactive({str_remove(input$end_quarter, ":")})
  rval_h = reactive({as.numeric(input$h)})
  
  
  #reactive exp for computing AR model errors 
  rval_AR_models = reactive({
    sapply(1:8, function(i){
      cv_rolling(data_full = rval_data_used(), Y_recent, p = i, h = rval_h())$errors[1]
    })
  })
  
  rank_ar = reactive({order(rval_AR_models())})
  
  best_ar_lag = reactive({rank_ar()[1]})
  
  benchmark_AR = reactive({
    test_rolling(data_full = rval_data_used(), Y_recent, p = best_ar_lag(), h = rval_h())
  })
  
  output$benchmark = renderText({
    paste0("The benchmark model is AR(", best_ar_lag(),").")
  })
  
  
  build_model = function(type, h, best_ar_lag) {
    if (type == "AR") {
      model = test_rolling(rval_data_used(), Y_recent, p = best_ar_lag, h = h)
    }
    else if (type == "ADL") {
      rval_rpc_used = reactive({
        get_data_rpc(input$start_quarter, input$end_quarter)
      })
      rpc_used = rval_rpc_used()
      spread_used = X2
      #cv for ADL
      x1_lags = c(0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
      x2_lags = c(0,1,2,3,4,0,1,2,3,4,0,1,2,3,4,0,1,2,3,4,0,1,2,3,4)
      combs = data.frame(x1_lags, x2_lags)
      ADL_errors = reactive({
        sapply(1:25, function(i){
          cv_rolling_adl(data_full = rval_data_used(),rpc_full = rpc_used,spread = spread_used, Y_recent,p_y = best_ar_lag, p_x1 = combs[i,1], p_x2 = combs[i,2], h = h)$errors[1]
        })
      })
      rank_adl = order(ADL_errors())
      
      best_adl_lag = combs[rank_adl[1],]
      
      #test using best ADL model
      model = test_rolling_adl(rval_data_used(), rpc_used, spread_used,Y_recent,p_y = best_ar_lag, p_x1 = best_adl_lag[,1], p_x2 = best_adl_lag[,2], h = h)
    }
    else if (type == "Simple Average") {
      model = ar_combined(rval_data_used(), h, test_rolling, Y_recent)
    }
    else if (type == "Granger-Ramanathan") {
      model = ar_gr_combined(rval_data_used(), h, test_rolling, Y_recent)
    }
  }
  
  model = reactive({build_model(input$model_type, rval_h(), best_ar_lag())})
  

  #plot for chosen model
  #Syntax: ts(object, start=startdate, end=enddate, freq=frequency (periods per year))
  
  output$plot = renderPlotly({
    
    start_quarter = rval_start_quarter()
    end_quarter = rval_end_quarter()
    data_used = rval_data_used()
    
    forecasts = model()$pred
    rmsfe = model()$errors[1]
    
    rval_year_end = reactive({
      ifelse(as.numeric(str_sub(end_quarter, start = 3, end = 4))>=97, as.numeric(str_sub(end_quarter, start = 3, end = 4))+1900, as.numeric(str_sub(end_quarter, start = 3, end = 4))+2000)})
    rval_quarter_end = reactive({as.numeric(str_sub(end_quarter, start = 6, end = 6))})
    year_end = rval_year_end()
    quarter_end = rval_quarter_end()
    
    rval_year_start = reactive({
      ifelse(as.numeric(str_sub(start_quarter, start = 3, end = 4))>=97, as.numeric(str_sub(start_quarter, start = 3, end = 4))+1900, as.numeric(str_sub(start_quarter, start = 3, end = 4))+2000)})
    rval_quarter_start = reactive({as.numeric(str_sub(start_quarter, start = 6, end = 6))})
    year_start = rval_year_start()
    quarter_start = rval_quarter_start()
    
    year_start_pred = year_start - 2
    quarter_start_pred = quarter_start
    last_obs = data_used %>% 
      select(last_col()) %>% 
      rename_with(.cols = 1, ~"gdp") %>%
      mutate(gdp = suppressWarnings(as.numeric(gdp))) %>%
      drop_na() %>%
      nrow()
    last_obs = ifelse(last_obs == 257, 256, last_obs)
    true_values = tail(Y_recent[1:last_obs+1,], num_quarters+8) #add some context before test window
    
    
    true_ts = ts(true_values, start = c(year_start_pred, quarter_start_pred), end = c(year_end, quarter_end), frequency = 4)
    forecast.ts = ts(forecasts, start = c(year_start, quarter_start), end = c(year_end, quarter_end), frequency = 4)
    #upper bound interval
    #lower bound interval
    
    rval_sig_level = reactive({
      as.numeric(str_remove(input$sig_level, "%"))/100
    })
    
    forecast_intervals = reactive({intervals(forecasts, rval_sig_level(), rmsfe)})
    upper.ts = ts(forecast_intervals()[,1], start = c(year_start, quarter_start), end = c(year_end, quarter_end), frequency = 4)
    lower.ts = ts(forecast_intervals()[,3], start = c(year_start, quarter_start), end = c(year_end, quarter_end), frequency = 4)
    
    xaxis_start = paste0(year_start_pred,"-", quarter_start_pred*3, "-01")
    xaxis_end = paste0(year_end,"-",quarter_end*3,"-01")
    
    time = seq(from=as.Date(xaxis_start),to=as.Date(xaxis_end),by="3 months")
    toplot = cbind.data.frame(true_values, time)
    toplot$forecast = c(rep(NA, 8), forecast.ts)
    toplot$upper = c(rep(NA, 8), upper.ts)
    toplot$lower = c(rep(NA, 8), lower.ts)
    
    p <- ggplot(data = toplot, mapping=aes(x = time)) +
      geom_line(mapping=aes(y=true_values,col="True Values")) +
      geom_line(mapping=aes(y=forecast,col="Forecasts")) +
      geom_line(mapping=aes(y=upper,col="Upper Confidence Interval"),linetype=3) + 
      geom_line(mapping=aes(y=lower,col="Lower Confidence Interval"),linetype=3) +
      geom_ribbon(aes(ymin=lower,ymax=upper), fill="mistyrose3", alpha=0.4) +
      labs(y = "GDP growth(%)") +
      scale_color_manual(values = c("True Values"="white", "Forecasts"="red", "Upper Confidence Bound"="green", "Lower Confidence Bound"="green"),
                         labels = c("true"="True Values", "forecast"="Forecasts", "upper"="Upper Confidence Bound", "lower"="Lower Confidence Bound")) +
      theme(legend.position="bottom",legend.text = element_text(size=15),legend.key.size = unit(1.5, 'cm')) +
      theme(legend.title=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_text(face="bold")) +
      theme(
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent',color="white"), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend panel
      ) +
      theme(text=element_text(color="white",size=15),axis.text=element_text(color="white")) +
      geom_hline(yintercept = 0,linetype='dashed', col = 'turquoise')
    p <- ggplotly(p) %>%
      layout(legend = list(orientation = 'h',font = list(size = 15),title = list(text = "<b> </b>")))
    
    suppressWarnings(print(p))
  })
  
  output$quarter_error_message <- renderText({
    start_quarter <- gsub(":Q", ".", input$start_quarter)
    end_quarter <- gsub(":Q", ".", input$end_quarter)
    
    if (as.numeric(start_quarter) > as.numeric(end_quarter)) {
      "The starting quarter must be before the ending quarter. Please select again."
    } else {
      ""
    }
  })
  
  stats_df = reactive({
    df <- data.frame(
      c("RMSFE", "MAE", "Signs Predicted Wrongly (%)", "Negative Signs Predicted Wrongly (%)"),
      c(
        round(benchmark_AR()$errors[1], 4),
        round(benchmark_AR()$errors[2], 4),
        round(benchmark_AR()$errors[3]*100, 2),
        round(benchmark_AR()$errors[4]*100, 2)
      ),
      c(
        round(model()$errors[1], 4),
        round(model()$errors[2], 4),
        round(model()$errors[3]*100, 2),
        round(model()$errors[4]*100, 2)
      )
    )
    colnames(df) <- c("Statistic", "Best AR Model (Benchmark)", "Your Chosen Model")
    df
  })
  
  output$stats_table <- renderTable({
    stats_df()}, digits = 4)
  
  output$display_h <- renderText({
    paste0("Plot for ", input$h, "-step Ahead Forecast")
  })
  
  output$your_chosen_model <- renderText({
    ifelse(input$model_type == "AR", paste0("Your chosen model is AR(",best_ar_lag(), ")."), 
           ifelse(input$model_type == "ADL", paste0("Your chosen model is ADL(", model()$lags[1], ",", model()$lags[2], ",", model()$lags[3], ")."), 
                  paste0("Your chosen model is ", input$model_type, ".", 
                         ifelse(input$model_type == "Granger-Ramanathan", paste0(" The constant is ", round(model()$weights[1],2), 
                                                                                 " and the weights assigned to the 8 AR models are ", 
                                                                                 round(model()$weights[2],2), ", ", round(model()$weights[3],2), ", ", 
                                                                                 round(model()$weights[4],2), ", ", round(model()$weights[5],2), ", ", 
                                                                                 round(model()$weights[6],2), ", ", round(model()$weights[7],2), ", ", 
                                                                                 round(model()$weights[8],2), ", ", round(model()$weights[9],2), " respectively."), ""))))
  })
  
  output$dm_test_result <- ({reactive({
    l1 = benchmark_AR()$abs_loss
    l2 = model()$abs_loss
    dm_stat = ifelse(input$model_type == "AR", NA,
                     ifelse(input$model_type == "Simple Average", dm_test(l1, l2, rval_h()),
                            ifelse(input$model_type == "Granger-Ramanathan", dm_test(l1, l2, rval_h()),
                                   ifelse(model()$lags[2]==0 & model()$lags[3]==0, NA, dm_test(l1, l2, rval_h())))))
    
    dm_prob = suppressWarnings(pt(-abs(dm_stat), num_quarters-rval_h()-1))
    hyp_test = ifelse(dm_prob<0.05, "can reject", "cannot reject")
    
    dm_test_line = ifelse(is.na(hyp_test), "", paste0("We ", hyp_test, " the null hypothesis of equal predictive ability as the t-statistic is ", round(dm_stat,2), "."))
    
    paste(dm_test_line)
  })
  })
}

