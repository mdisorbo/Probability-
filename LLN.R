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

  titlePanel("Law of Large Numbers"),

  fluidRow(

    column(3,

           #drop-down to select a specific distribution
           selectInput("select", label = "Distribution",
                       choices = list("Expo(1)" = 1, "Pois(1)" = 2, "Unif(0,1)" = 3,
                                      "Gamma(2,2)" = 4, "Beta(2,2)" = 5,
                                      "Geom(1/2)" = 6,
                                      "Bin(10, 1/2)" = 7),
                       selected = 1)),

    column(3,

           #choose the number of sample means
           sliderInput("paths",
                       "# of paths",
                       min = 1,
                       max = 5,
                       value = 1)),
    column(3,

           #choose the number of sample means
           sliderInput("length",
                       "# of random variables",
                       min = 10,
                       max = 1000,
                       value = 100)),
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
    input$path
    input$length
    input$go
  }, {

    #create the plot
    output$plot1 <- renderPlot({

      #define path length and number of paths
      length = input$length
      paths = input$paths


      #sample according to the selected distribution
      #number of rows is the length of paths, number of columns
      #   is the number of paths
      if(input$select == 1){

        #generate the data matrix
        matrix = matrix(rexp(paths*length, 1), nrow = length, ncol = paths)

        #define the sample mean matrix
        matrix.means = matrix(0, nrow = length, ncol = paths)

        #fill in the matrix
        #create separate paths
        for(j in 1:paths){

          #fill in each path
          for(i in 1:length){

            #take a running mean
            matrix.means[i, j] = mean(matrix[1:i, j])
          }
        }

        #define the true mean
        true.mean = 1
      }

      else if(input$select == 2){

        #generate the data matrix
        matrix = matrix(rpois(paths*length, 1), nrow = length, ncol = paths)

        #define the sample mean matrix
        matrix.means = matrix(0, nrow = length, ncol = paths)

        #fill in the matrix
        #create separate paths
        for(j in 1:paths){

          #fill in each path
          for(i in 1:length){

            #take a running mean
            matrix.means[i, j] = mean(matrix[1:i, j])
          }
        }

        #define the true mean
        true.mean = 1
      }

      else if(input$select == 3){

        #generate the data matrix
        matrix = matrix(runif(paths*length), nrow = length, ncol = paths)

        #define the sample mean matrix
        matrix.means = matrix(0, nrow = length, ncol = paths)

        #fill in the matrix
        #create separate paths
        for(j in 1:paths){

          #fill in each path
          for(i in 1:length){

            #take a running mean
            matrix.means[i, j] = mean(matrix[1:i, j])
          }
        }

        #define the true mean
        true.mean = 1/2
      }

      else if(input$select == 4){

        #generate the data matrix
        matrix = matrix(rgamma(paths*length, 2, 2), nrow = length, ncol = paths)

        #define the sample mean matrix
        matrix.means = matrix(0, nrow = length, ncol = paths)

        #fill in the matrix
        #create separate paths
        for(j in 1:paths){

          #fill in each path
          for(i in 1:length){

            #take a running mean
            matrix.means[i, j] = mean(matrix[1:i, j])
          }
        }

        #define the true mean
        true.mean = 1
      }

      else if(input$select == 5){

        #generate the data matrix
        matrix = matrix(rbeta(paths*length, 2, 2), nrow = length, ncol = paths)

        #define the sample mean matrix
        matrix.means = matrix(0, nrow = length, ncol = paths)

        #fill in the matrix
        #create separate paths
        for(j in 1:paths){

          #fill in each path
          for(i in 1:length){

            #take a running mean
            matrix.means[i, j] = mean(matrix[1:i, j])
          }
        }

        #define the true mean
        true.mean = 1/2
      }

      else if(input$select == 6){

        #generate the data matrix
        matrix = matrix(rgeom(paths*length, 1/2), nrow = length, ncol = paths)

        #define the sample mean matrix
        matrix.means = matrix(0, nrow = length, ncol = paths)

        #fill in the matrix
        #create separate paths
        for(j in 1:paths){

          #fill in each path
          for(i in 1:length){

            #take a running mean
            matrix.means[i, j] = mean(matrix[1:i, j])
          }
        }

        #define the true mean
        true.mean = 1
      }

      else if(input$select == 7){

        #generate the data matrix
        matrix = matrix(rbinom(paths*length, 10, 1/2), nrow = length, ncol = paths)

        #define the sample mean matrix
        matrix.means = matrix(0, nrow = length, ncol = paths)

        #fill in the matrix
        #create separate paths
        for(j in 1:paths){

          #fill in each path
          for(i in 1:length){

            #take a running mean
            matrix.means[i, j] = mean(matrix[1:i, j])
          }
        }

        #define the true mean
        true.mean = 5
      }

      #plot the paths
      #define colors
      colors = c("firebrick3", "dodgerblue4",
                 rgb(1, 0, 0, 3/4), rgb(0, 1, 0, 3/4),
                 rgb(0, 0, 1, 3/4))

      for(i in 1:paths){

        #lay down first plot
        if(i == 1){
          plot(1:length, matrix.means[, i], main = "Running Mean",
               xlab = "# of Random Variables", ylab = "Mean", type = "l",
               lwd = 5, col = colors[i], ylim = c(min(matrix.means, true.mean), max(matrix.means, true.mean)))
        }

        #lay down a line
        if(i > 1){
          lines(1:length, (matrix.means[, i]), col = colors[i], lwd = 5)
        }
      }

      #plot the true mean
      abline(h = true.mean, col = "black", lty = 3, lwd = 3)

      #make a legend
      legend("topright", legend = c("True Mean"),
             lty=c(3), lwd=c(2.5),
             col= "black")
    })
  })
}



shinyApp(ui, server)


