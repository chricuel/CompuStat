
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Importance Sampling"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("nsim",
                  "Numero de simulaciones:",
                  min = 100,
                  max = 50000,
                  value = 1000),
      
      numericInput("m",
                   "Parametro de la exponecial a estimar, m:",
                   min = 1,
                   max = 30,
                   value = 2),
      
      numericInput("lambda",
                   "Parametro de la exponecial de Importance Sampling, lambda:",
                   min = 1,
                   max = 30,
                   value = 1),
      
      numericInput("a",
                   "Parametro alpha de la distribución Beta, a:",
                   min = 1,
                   max = 30,
                   value = 1),
      
      numericInput("b",
                   "Parametro beta de la distribución Beta, b:",
                   min = 1,
                   max = 30,
                   value = 5)
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Estimaciones", plotOutput("distPlot"), plotOutput("distPlot2"), plotOutput("distPlot3")),
        tabPanel("Comparación de Errores",plotOutput("distPlot4"))
      )
    )
  )
))
