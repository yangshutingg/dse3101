library(shiny)
library(shinyWidgets)

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
  sliderTextInput("end_quarter",
              "Ending quarter",
              choices = data$DATE,
              selected = "2003:Q2",
              from_min = "2003:Q2",
              to_max = "2023:Q4"),
  sliderInput("test_window",
              "Test window", 
              value = 10,
              min = 10,
              max = 40),
  textOutput("rmsfe"),
  plotOutput("plot")
)
