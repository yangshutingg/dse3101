library(shiny)
library(shinyWidgets)
library(shinythemes)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .main-container, .tab-content {
      flex-grow: 1;
      display: flex;
      flex-direction: column;

    }
    .tab-content>.active {
      display: flex;
      flex-direction: column;
      flex-grow: 1;
      display: flex;
      justify-content: center;
    }
    .shiny-text-output {
      font-family: 'Arial', sans-serif;
      font-size: 16px;
      flex-grow: 1;
      display: flex;
      flex-direction: column;
      justify-content: center;
    }

    
    .table{
      font-size:16px!important;
      font-family: 'Arial', sans-serif;
    }
    
    #stats_table {
      flex-grow: 1; 
      display: flex; 
      flex-direction: column; 
      justify-content: space-around;
      margin-bottom:20px;
    }
    .tab-content {
      border: 1px solid #ddd;
      border-radius: 0 4px 4px 4px;
      padding: 15px;
      max-height: 80vh;
      height: 80vh;
    }
    
    #plot {
      flex-grow: 1;
    }

    .forecast-title{
      font-size: 28px;
      font-weight: bold;
      text-align:center;
    }
    
    table>thead>tr>th, .table>thead>tr>th, table>tbody>tr>th, .table>tbody>tr>th, table>tfoot>tr>th, .table>tfoot>tr>th, table>thead>tr>td, .table>thead>tr>td, table>tbody>tr>td, .table>tbody>tr>td, table>tfoot>tr>td, .table>tfoot>tr>td { 
      border-color: aliceblue;
    }

    #pct_signs_wrong {
      flex-grow: 1;
      display: flex;
      flex-direction: column;
      justify-content: flex-end; 
    }
    
    #quarter_error_message{
    font-size: 30px;
    }
    
    /* Navigation Tabs CSS */
    .nav-tabs {
      border-bottom: 3px solid #222; /* Thicker bottom border for a strong line */
    }

    .nav-tabs > li > a {
      color: #ddd; /* Lighter text for contrast against dark background */
      font-family: 'Arial', sans-serif;
      font-size: 16px;
      border: none; /* Remove individual borders for a cleaner look */
      margin-right: 0.5rem;
      margin-bottom: -1px; /* Overlap the bottom border to avoid double borders */
      line-height: 1.5;
      border-radius: 0.5rem 0.5rem 0 0; /* More pronounced rounded corners at the top */
      padding: 12px 20px;
      transition: color 0.3s ease, background-color 0.3s ease; /* Smooth transition for color and background */
    }

    .nav-tabs > li > a:hover,
    .nav-tabs > li > a:focus {
      background-color: #555; /* Slightly lighter background on hover/focus */
      color: #fff; /* White text on hover/focus for better visibility */
      border: none;
    }

    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover,
    .nav-tabs > li.active > a:focus {
      color: #fff; /* White text for the active tab */
      background-color: darkcyan; /* Use a theme color for the active tab background */
      border: none;
      border-bottom-color: transparent;
      border-radius: 0.5rem 0.5rem 0 0; /* Preserve the rounded corners on top */
      box-shadow: 0 -2px 5px rgba(0,0,0,0.15); /* Subtle shadow for depth */
    }

  ")),
    tags$link(href = 'https://fonts.googleapis.com/css?family=Arial', rel = 'stylesheet', type = 'text/css')
  ),

  titlePanel("REAL-TIME GDP GROWTH FORECAST"),
  theme = shinytheme("superhero"),
  sidebarLayout(
    sidebarPanel(
      sliderTextInput("start_quarter",
                      "Starting Quarter",
                      choices = data$DATE,
                      selected = "1997:Q1",
                      from_min = "1997:Q1",
                      to_max = "2023:Q3"
      ),
      sliderTextInput("end_quarter",
                      "Ending Quarter",
                      choices = data$DATE,
                      selected = "1997:Q2",
                      from_min = "1997:Q2",
                      to_max = "2023:Q4"
      ),
      selectInput("h",
                  "Forecast Horizon",
                  choices = c(1, 2, 3, 4)
      ),
      selectInput("model_type",
                  "Model Type",
                  choices = c("AR", "ADL", "Simple Average", "Granger-Ramanathan")
      ),
      p("AR: regression model in which GDP growth rate is regressed against its previous values"),
      p("ADL: regression model in which GDP growth rate is regressed against its previous values & previous values of real personal consumption and treasury term spread"),
      p("Simple Average: combination of forecasts using equal weights"),
      p("Granger-Ramanathan: combination of forecasts using optimal weights"),
      selectInput("sig_level",
                  "Forecast Interval",
                  choices = c("50%", "68%", "80%", "90%")
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Benchmark Analysis",
                 h4(textOutput("benchmark")),
                 h4(textOutput("your_chosen_model")),
                 h4(tableOutput("stats_table")),
                 h4(textOutput("quarter_error_message")),
                 h4(textOutput("dm_test_result"))

        ),
        tabPanel("Forecast Graph",
                 h3(textOutput("display_h"), class="forecast-title"),  
                 h4(plotlyOutput("plot"))
        ),
       
      )
    )
  )
)

shinyApp(ui = ui, server = server)
