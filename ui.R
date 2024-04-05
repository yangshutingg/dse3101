library(shiny)
library(shinyWidgets)
library(shinythemes)

ui <- fluidPage(
  titlePanel("Benchmarking of economic models"),
  theme = shinytheme("readable"),
  sidebarLayout(
    sidebarPanel(
      sliderTextInput("start_quarter",
                      "Starting quarter",
                      choices = data$DATE,
                      selected = "2003:Q2",
                      from_min = "2003:Q2",
                      to_max = "2023:Q3"
      ),
      sliderTextInput("end_quarter",
                      "Ending quarter",
                      choices = data$DATE,
                      selected = "2003:Q3",
                      from_min = "2003:Q3",
                      to_max = "2023:Q4"
      ),
      selectInput("h",
                  "Forecast horizon",
                  choices = c(1, 2, 3, 4)
      ),
      selectInput("model_type",
                  "Model type",
                  choices = c("AR", "ADL", "Simple Average", "Granger-Ramanathan")
      ),
      selectInput("sig_level",
                  "Forecast interval",
                  choices = c("50%", "68%", "80%", "90%")
      )
    ),
  mainPanel(
    h4(verbatimTextOutput("benchmark_stats")),
    h4(verbatimTextOutput("chosen_model_stats")),
    h4(textOutput("pct_signs_wrong")),
    h4("Plot",plotOutput("plot"))
    )
  )
)
