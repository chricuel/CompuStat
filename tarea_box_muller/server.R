# Christian Cuéllar
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2]
    bins <- input$bins

    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    size<-bins
    
    u<-runif(size)
    v<-runif(size)
    
    w<-rep(0,size)
    y<-rep(0,size)
    
    for (i in 1:size){
      w[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i])
      y[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i])
    }
    
    #Gráfica de la Normal
    plot(density(c(w,y)))
    
  })

})
