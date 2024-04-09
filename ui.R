library(shiny)
library(shinyWidgets)
library(shinythemes)

ui <- fluidPage(
  titlePanel("Benchmarking of Economic Models"),
  theme = shinytheme("readable"),
  sidebarLayout(
    sidebarPanel(
      sliderTextInput("start_quarter",
                      "Starting Quarter",
                      choices = data$DATE,
                      selected = "2003:Q2",
                      from_min = "2003:Q2",
                      to_max = "2023:Q3"
      ),
      sliderTextInput("end_quarter",
                      "Ending Quarter",
                      choices = data$DATE,
                      selected = "2003:Q3",
                      from_min = "2003:Q3",
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
      selectInput("sig_level",
                  "Forecast Interval",
                  choices = c("50%", "68%", "80%", "90%")
      )
    ),
  mainPanel(
    h4(tableOutput("stats_table")),
    h4(textOutput("quarter_error_message")),
    h4(verbatimTextOutput("benchmark_stats")),
    h4(verbatimTextOutput("chosen_model_stats")),
    h4(textOutput("pct_signs_wrong")),
    h4(plotOutput("plot"))
    )
  )
)

shinyApp(ui = ui, server = server)
