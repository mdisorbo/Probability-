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
library(shiny)


#define the ui
ui <- fluidPage(

  titlePanel("Permutations"),


  #various buttons
  fluidRow(

    #drop-down to select a specific distribution
    column(6, sliderInput("n",
                          "n",
                          min = 2,
                          max = 5,
                          value = 3)),

    column(6, radioButtons(inputId = "table", label = "Sampling Table",
                           choices = list("Order Matters, With Replacement" = 1,
                                          "Order Doesn't Matter, With Replacement" = 2,
                                          "Order Matters, Without Replacement" = 3,
                                          "Order Doesn't Matter, Without Replacement" = 4),
                           selected = 1))
  ),

  fluidRow(

    column(6, sliderInput("k",
                          "k*",
                          min = 1,
                          max = 3,
                          value = 2)),
    column(4, tableOutput("table1"))
  ),

  fluidRow(column(12, textOutput("text1")))
)


#define the server
server <- function(input, output) {



  #run after we hit go
  observeEvent({
    input$table
    input$n
    input$k
  },{

    #define input
    n = input$n
    k = input$k

    #generate permutations based on the selection
    #with replacement, order matters
    if(input$table == 1){
      perms = permutations(n, k, repeats.allowed = TRUE)
    }

    #with replacement, order doesn't matter
    if(input$table == 2){
      perms = permutations(n, k, repeats.allowed = TRUE)

      #sort the permutations; we want to take out copies
      perms = t(apply(perms, 1, function(x) sort(x)))

      #keep the unique orderings
      perms = unique(perms)
    }

    #without replacement, order matters
    if(input$table == 3){
      perms = permutations(n, k, repeats.allowed = FALSE)
    }

    #without replacement, order doesn't matter
    if(input$table == 4){
      perms = permutations(n, k, repeats.allowed = FALSE)

      #sort the permutations; we want to take out copies
      perms = t(apply(perms, 1, function(x) sort(x)))

      #keep the unique orderings
      perms = unique(perms)
    }

    #remove column and rownames
    colnames(perms) = NULL
    rownames(perms) = NULL

    #print the permutations
    output$table1 <- renderTable({

      #print out the perms nicely
      perms
    })
  })

  output$text1 <- renderText({
    "*If you select an infeasible k (such that k > n), k will be sampled
      randomly from all feasible values (1 to n)."
  })
}



#run the app
shinyApp(ui, server)







