#load libraries
library(shiny)
library(expm)
library(MCMCpack)
library(gplots)
library(gtools)
library(grDevices)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(reshape2)
library(grid)
library(mvtnorm)
library(stats)

ui <- fluidPage(

  titlePanel("Central Limit Theorem"),

  fluidRow(

    column(5,

      #drop-down to select a specific distribution
      selectInput("select", label = "Distribution",
                  choices = list("Expo(1)" = 1, "Pois(1)" = 2, "Unif(0,1)" = 3,
                                 "Gamma(2, 2)" = 4, "Beta(2, 2)" = 5,
                                 "Geom(1/2)" = 6,
                                 "Bin(10, 1/2)" = 7),
                  selected = 1)),

    column(5,

      #choose the number of sample means
      sliderInput("rvs",
                    "n = # of random variables",
                    min = 1,
                    max = 100,
                    value = 3)),
    column(2, actionButton("go", "Go!"))
  ),

    #show distribution/textual comments
    fluidRow(
      plotOutput("plot1"))
)




server <- function(input, output) {

  #Only run after changing a parameter
  observeEvent({
    input$select
    input$rvs
    input$go
    }, {

    #create the plot
    output$plot1 <- renderPlot({

      #define sample size/number of sample means
      samp = 1000
      n = input$rvs

      #sample according to the selected distribution
      #number of rows is the number of sample means,
      #   number of columns is the sample size
      #calculate the mean/sd of the approximate normal in both cases
      if(input$select == 1){
        matrix = matrix(rexp(n*samp), nrow = samp, ncol = n)
        mu = 1
        sd = 1/sqrt(n)
      }
      else if(input$select == 2){
        matrix = matrix(rpois(n*samp, 1), nrow = samp, ncol = n)
        mu = 1
        sd = 1/sqrt(n)
      }
      else if(input$select == 3){
        matrix = matrix(runif(n*samp), nrow = samp, ncol = n)
        mu = 1/2
        sd = sqrt(1/(12*n))
      }
      else if(input$select == 4){
        matrix = matrix(rgamma(n*samp, 2, 2), nrow = samp, ncol = n)
        mu = 1
        sd = sqrt(2/(n*2^2))
      }
      else if(input$select == 5){
        matrix = matrix(rbeta(n*samp, 2, 2), nrow = samp, ncol = n)
        mu = 2/(2 + 2)
        sd = sqrt(mu*(1 - mu)/(n*(2 + 2 + 1)))
      }
      else if(input$select == 6){
        matrix = matrix(rgeom(n*samp, 1/2), nrow = samp, ncol = n)
        mu = 1
        sd = sqrt((1/2)/(n/2^2))
      }
      else if(input$select == 7){
        matrix = matrix(rbinom(n*samp, 10, 1/2), nrow = samp, ncol = n)
        mu = 5
        sd = sqrt(10/(4*n))
      }

      #calculate the means from each sample
      means = rowMeans(matrix)

      #find the fit for the normal (estimate parameters)
      xfit = seq(from = min(means), to = max(means), by = .001)
      yfit = dnorm(xfit, mean = mu, sd = sd)

      #plot means, overlay Normal distribution
      plot(density(means), main = "", xlab = "",
           ylab = "Density", col = rgb(0, 1, 0, 3/4),
           lwd = 8, ylim = c(0, max(density(means)$y, yfit)))

      lines(xfit, yfit, col = rgb(0, 0, 1, 3/4), lwd = 8)

      legend("topright", legend = c("Sample Mean Density", "Estimated Normal Density"),
             lty=c(1,1), lwd=c(2.5,2.5),
             col=c(rgb(0, 1, 0, 3/4), rgb(0, 0, 1, 3/4)))
    })
  })
}



shinyApp(ui, server)


