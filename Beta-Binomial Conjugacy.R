#load libraries
library(mvtnorm)
library(matrixStats)
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
library(stats)


#define the ui
ui <- fluidPage(

  titlePanel("Beta-Binomial Conjugacy"),


  #various buttons
  fluidRow(

    #drop-down to select a specific distribution
    column(2, sliderInput("alpha",
                          "alpha",
                          min = 1,
                          max = 10,
                          value = 1)),
    column(2, sliderInput("beta",
                          "beta",
                          min = 1,
                          max = 10,
                          value = 1)),
    column(2, sliderInput("n",
                          "n = number of trials",
                          min = 5,
                          max = 100,
                          value = 50)),
    column(2, uiOutput("slider")),
    column(1, actionButton("go", "Go!"))
  ),


  fluidRow(column(6, plotOutput("plot1")),
           column(6, plotOutput("plot2"))),

  fluidRow(column(12, textOutput("text1")))
)


#define the server
server <- function(input, output) {

  #define the slider
  output$slider <- renderUI({
    sliderInput("x",
                "x = number of successes",
                min = 0,
                max = input$n,
                value = round(input$n/2), step = 1)})

  #run after we hit go
  observeEvent({
    input$go
    input$alpha
    input$beta
    input$n
    input$x
  },{

    #define number of sims
    sims = 1000

    #mark parameters
    alpha = input$alpha
    beta = input$beta
    n = input$n
    x = input$x

    #if we get an infeasible x, sample among feasible values
    if(x > n){
      x = sample(1:n, 1)
    }

    #draw prior p
    p.prior = rbeta(sims, alpha, beta)

    #define title for this plot
    title.prior = paste0("Prior: p ~ Beta(", alpha, ", ", beta, ")")

    #create prior plot
    output$plot1 <- renderPlot({
      hist(p.prior, main = title.prior, col = rgb(1,0,0,1/4),
           xlim = c(0, 1), xlab = "p")
    })

    #draw posterior plot
    p.posterior = rbeta(sims, alpha + x, beta + n - x)

    #define title for this plot
    title.posterior = paste0("Posterior: p|X = ", x, " ~ Beta(", alpha + x, ", ", beta + n - x, ")")

    #create posterior plot
    output$plot2 <- renderPlot({
      hist(p.posterior, main = title.posterior, col = rgb(0,0,1,1/4),
           xlim = c(0, 1), xlab = "p")
    })
  })
}


#run the app
shinyApp(ui, server)

