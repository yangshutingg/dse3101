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
  selectInput("window",
              "Window type",
              choices = c("Rolling", "Expanding")
  ),
  selectInput("sig_level",
              "Forecast interval",
              choices = c("50%", "68%", "80%", "90%")
  ),
  sliderTextInput("end_quarter",
              "Ending quarter",
              choices = data$DATE,
              selected = "2003:Q2",
              from_min = "2003:Q2",
              to_max = "2023:Q4",
              dragRange = TRUE),
  textOutput("rmsfe"),
  textOutput("mae"),
  textOutput("pct_signs_wrong"),
  plotOutput("plot")
)