library(shiny)
library(shinyWidgets)
library(shinythemes)

ui <- fluidPage(
  titlePanel("Benchmarking of economic models"),
  shinythemes::themeSelector(),
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
                  choices = c("AR", "ADL", "PCR")
      ),
      selectInput("window",
                  "Window type",
                  choices = c("Rolling", "Expanding")
      ),
      selectInput("sig_level",
                  "Forecast interval",
                  choices = c("50%", "68%", "80%", "90%")
      )
    ),
  mainPanel(
    tabsetPanel(
      tabPanel(textOutput("rmsfe")),
      tabPanel(textOutput("mae")),
      tabPanel(textOutput("pct_signs_wrong")),
      tabPanel(plotOutput("plot"))
      )
    )
  )
)