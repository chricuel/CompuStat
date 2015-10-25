# Christian Cuéllar

library(shiny)

shinyServer(function(input, output) {
  valorreal <- reactive(1-exp(-2*input$m))
  real <- function(M) (1-exp(-2*M))
  vr <- reactive(real(input$m))
  
 # Error de Montecarlo Crudo con Intervalos de Confianza:
 # montecarlo <- function(nsim, alpha, M){
  #  U <- reactive(runif(nsim, 0, 2))
  #}

 # U <- reactive(runif(input$nsim, 0, 2))
#  phi <- reactive(input$m/(exp(U()*input$m)))
#  thetaf <- reactive(2*mean(phi()))
#  zalphamedios<-reactive(qnorm(1 -input$alpha/200,lower.tail=FALSE))
#  scuadradan <- reactive(var(phi()))
#  limsup<-reactive(thetaf()+zalphamedios()*((scuadradan()/input$nsim)^(1/2)))
#  liminf<-reactive(thetaf()-zalphamedios()*((scuadradan()/input$nsim)^(1/2)))
#  errormc <- reactive(valorreal()-thetaf())
#  reactive(list(est=thetaf(),limsup=limsup(),liminf=liminf(), errormc=errormc()))
  
# Montecarlo Crudo
  phi <- function(x, m) m/(exp(m*x))
  montecarlo <- function(nsim=100, alpha=0.05, m=1){
    U <- runif(nsim, 0, 2)
    thetaf<-2*mean(phi(U, m))
    zalphamedios<-qnorm(alpha/2,lower.tail=FALSE)
    scuadradan<-var(phi(U, m))
    limsup<-thetaf+zalphamedios*((scuadradan/nsim)^(1/2))
    liminf<-thetaf-zalphamedios*((scuadradan/nsim)^(1/2))
    errormc <- vr()-thetaf
    #   list(est=2*mean(phi(U, m)),limsup=limsup,liminf=liminf)
    list(est=2*mean(phi(U,m)),limsup=limsup,liminf=liminf, errormc=errormc)
  }
  
  
  
# Exponencial
  exponencial <- function(nsim=100, alpha=0.05, m=1, lambda=2){
    U2 <- runif(nsim, 0, 1)
    X2 <- -log(1-(1-exp(-2*lambda))*U2)/lambda
    denstrunc <- function(x, lambda) lambda*exp(-(lambda)*x)/(1-exp(-2*lambda))
    division <- function(x, lambda, m) phi(x, m)/denstrunc(x, lambda)
    thetafexp <-mean(division(X2, lambda, m))
    zalphamediosexp<-qnorm(alpha/2,lower.tail=FALSE)
    scuadradanexp<-var(division(X2, lambda, m))
    limsupexp<-thetafexp+zalphamediosexp*((scuadradanexp/nsim)^(1/2))
    liminfexp<-thetafexp-zalphamediosexp*((scuadradanexp/nsim)^(1/2))
    errorexp <- vr()-thetafexp
    list(estexp=mean(division(X2, lambda, m)),limsupexp=limsupexp,liminfexp=liminfexp, errorexp=errorexp)
  }

  # Beta
  beta <- function(nsim=100, alpha=0.05, m=1, a=1, b=5){
    B <- rbeta(nsim, a, b)
    Y <- 2*B
    fbeta <- function(x, a, b) dbeta(x/2, a, b)*0.5
    divisionbeta <- function(x, a, b) phi(x, m)/fbeta(x, a, b)
    thetabeta <-mean(divisionbeta(Y, a, b))
    zalphamediosbeta<-qnorm(alpha/2,lower.tail=FALSE)
    scuadradanbeta<-var(divisionbeta(Y, a, b))
    limsupbeta<-thetabeta+zalphamediosbeta*((scuadradanbeta/nsim)^(1/2))
    liminfbeta<-thetabeta-zalphamediosbeta*((scuadradanbeta/nsim)^(1/2))
    errorbeta <- vr()-thetabeta
    list(estbeta= mean(divisionbeta(Y, a, b)),limsupbeta=limsupbeta,liminfbeta=liminfbeta, errorbeta=errorbeta)
  }

  # Errores
  errores <- function(nsim=100, alpha=0.05, m=1, lambda=2, a=1, b=5){
    U <- runif(nsim, 0, 2)
    thetaf<-2*mean(phi(U, m))
    errormc <- vr()-thetaf
    
    U2 <- runif(nsim, 0, 1)
    X2 <- -log(1-(1-exp(-2*lambda))*U2)/lambda
    denstrunc <- function(x, lambda) lambda*exp(-(lambda)*x)/(1-exp(-2*lambda))
    division <- function(x, lambda, m) phi(x, m)/denstrunc(x, lambda)
    thetafexp <-mean(division(X2, lambda, m))
    errorexp <- vr()-thetafexp
    
    B <- rbeta(nsim, a, b)
    Y <- 2*B
    fbeta <- function(x, a, b) dbeta(x/2, a, b)*0.5
    divisionbeta <- function(x, a, b) phi(x, m)/fbeta(x, a, b)
    thetabeta <-mean(divisionbeta(Y, a, b))
    errorbeta <- vr()-thetabeta
    list(errormc=errormc, errorexp=errorexp, errorbeta=errorbeta)
  }
    
  output$distPlot <- renderPlot({
    
    N<-reactive(seq(100,input$nsim,100))
    data<-data.frame()
    for(i in N()){
      data<-rbind(data,montecarlo(i,0.05,input$m))
    }
    
    plot(data[ ,1],type="l", lwd=1.5, xlab="Numero de simulaciones (multiplicar por 100)", ylab="Valor de MC", main="Montecarlo Crudo con Intervalos de Confianza al 95%")
    lines(data[ ,2], lty=3, col="red")
    lines(data[ ,3], lty=3, col="blue")
    abline(h=vr(), col="forestgreen")
    
  
  })
  
  output$distPlot2 <- renderPlot({
    
    N<-reactive(seq(100,input$nsim,100))
    dataexp<-data.frame()
    for(i in N()){
      dataexp<-rbind(dataexp,exponencial(i,0.05,input$m, input$lambda))
    }
    
    plot(dataexp[ ,1],type="l", lwd=1.5, xlab="Numero de simulaciones (multiplicar por 100)", ylab="Valor de IS", main="Importance Sampling de la Exponencial con Intervalos de Confianza al 95%")
    lines(dataexp[ ,2], lty=3, col="red")
    lines(dataexp[ ,3], lty=3, col="blue")
    abline(h=vr(), col="forestgreen")
    
    
  })

  output$distPlot3 <- renderPlot({
    
    N<-reactive(seq(100,input$nsim,100))
    databeta<-data.frame()
    for(i in N()){
      databeta<-rbind(databeta,beta(i,0.05,input$m, input$a, input$b))
    }
    
    plot(databeta[ ,1],type="l", lwd=1.5, xlab="Numero de simulaciones (multiplicar por 100)", ylab="Valor de IS", main="Importance Sampling de la Beta con Intervalos de Confianza al 95%")
    lines(databeta[ ,2], lty=3, col="red")
    lines(databeta[ ,3], lty=3, col="blue")
    abline(h=vr(), col="forestgreen")
    
  })

  output$distPlot4 <- renderPlot({
    N<-reactive(seq(100,input$nsim,100))
    dataerrores<-data.frame()
    for(i in N()){
      dataerrores<-rbind(dataerrores,errores(i,0.05,input$m, input$lambda, input$a, input$b))
    }
    
    plot(dataerrores[ ,1],type="l", lwd=1.5, xlab="Numero de simulaciones (multiplicar por 100)", ylab="Error", main="Comparación de errores", sub="Verde = Montecarlo Crudo, Dorado = Exponencial, Rojo = Beta", col="forestgreen", ylim=c(-0.1,0.1))
    lines(dataerrores[ ,2], type="l", col="gold")
    lines(dataerrores[ ,3], type="l", col="red")
    abline(h=0)
  })
  
  
})
