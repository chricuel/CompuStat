# Christian Cuéllar
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Distribución Exponencial"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Número de observaciones:",
                  min = 1,
                  max = 10000,
                  value = 1000
    ),  
    
    sliderInput("lambda",
                "Lambda:",
                min = 2,
                max = 100,
                value = 10)
    ),  
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      dataTableOutput("tabla")
    )
  )
))
