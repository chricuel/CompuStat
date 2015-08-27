# Christian Cuéllar
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(datasets)

shinyServer(function(input, output) {
  
  u <- reactive(runif(input$n))
  x <- reactive((1/input$lambda)*log(1/(1-u())))
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2]
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    hist(x(), breaks = 50, freq= FALSE, col = 'darkgray', border = 'white')
    lines(density(x()),col=2,lwd=2)
  })
  
  output$tabla <- renderDataTable({
    data <- as.data.frame(cbind(x(),u()))
    names(data) <- c("X","Acumulada")
    data  
  })

})
