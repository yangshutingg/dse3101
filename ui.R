library(shiny)

ui <- fluidPage(
  selectInput("model_type",
              "Model type",
              choices = c("AR", "ADL", "PCR")),
  selectInput("h",
              "Forecast horizon",
              choices = c("1", "2", "3", "4")
  ),
  selectInput("sig_level",
              "Forecast interval",
              choices = c("50%", "68%", "80%", "90%")
  ),
  selectInput("h",
              "Forecast horizon",
              choices = c("1", "2", "3", "4")
  ),
  sliderInput("start_quarter",
              "Starting quarter",
              value = 1974, 
              min = 1974,
              max = 2023),
  sliderInput("test_window",
              "Test window", 
              value = 10,
              min = 10,
              max = 40),
  textOutput("rmsfe"),
  plotOutput("plot")
)
