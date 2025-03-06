library(shiny)
library(leaflet)

# Define UI for the application
shinyUI(fixedPage(
  titlePanel(
    div(
      "Data Visualization of Melbourne’s Lettable Floor Space by Type and Time", 
      style = "font-size: 30px; font-family: 'Arial', sans-serif; text-align: center;"
    )
  ),
  fluidRow(
    column(
      width = 6,
      div(
        style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",
        h4("Total Floor Area by Floor Type", style = "text-align: center;"),
        plotOutput("vis1Plot", height = "500px")
      )
    ),
    column(
      width = 6,
      div(
        style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",
        h4("Total Floor Area by Floor Type Interpretation", style = "text-align: center;"),
        uiOutput("vis1Text")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      div(
        style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",
        h4("Change in Lettable Floor Space Over Time", style = "text-align: center;"),
        plotOutput("vis2Plot", height = "500px")
      )
    ),
    column(
      width = 6,
      div(
        style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",
        h4("Change in Lettable Floor Space Over Time Interpretation", style = "text-align: center;"),
        uiOutput("vis2Text")
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      div(
        style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",
        h4("Lettable Floor Space Map", style = "text-align: center;"),
        leafletOutput("vis3Map", height = "600px")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      sliderInput("flr_space_range", 
                  "Lettable Floor Space Range (m2):", 
                  min = 0, max = 1000, value = c(0, 1000))
    ),
    column(
      width = 6,
      checkboxGroupInput("flr_type_filter", 
                         "Filter by Floor Type:", 
                         choices = NULL, 
                         selected = NULL)
    )
  ),
  fluidRow(
    column(
      width = 12,
      div(
        style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",
        h4("Lettable Floor Space Map Interpretation", style = "text-align: center;"),
        uiOutput("vis3Text")
      )
    )
  ),
  fluidRow(
    width = 12,
    div(
      style = "border: 1px solid #dcdcdc; padding: 15px; margin: 10px;",
      h4("Data Source Information", style = "text-align: center;"),
      uiOutput("dataDesc")
    )
  )
))