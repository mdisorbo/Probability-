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

  titlePanel("Bivariate Normal"),

  fluidRow(column(12, textOutput("text2"))),


  #define parameters
  fluidRow(
    column(2,
           sliderInput("mu.x",
                       "E(X)",
                       min = -2,
                       max = 2,
                       value = 0,
                       ticks = FALSE,
                       step = 1/10)),
    column(2,
           sliderInput("mu.y",
                       "E(Y)",
                       min = -2,
                       max = 2,
                       value = 0,
                       ticks = FALSE,
                       step = 1/10)),
    column(2,
           sliderInput("var.x",
                       "Var(X)",
                       min = 0,
                       max = 4,
                       value = 1,
                       ticks = FALSE,
                       step = 1/10)),
    column(2,
           sliderInput("var.y",
                       "Var(Y)",
                       min = 0,
                       max = 4,
                       value = 1,
                       ticks = FALSE,
                       step = 1/10)),
    column(2,
           sliderInput("rho",
                       "rho = Corr(X, Y)",
                       min = -1,
                       max = 1,
                       value = 1/2,
                       ticks = FALSE,
                       step = 1/10)),
    column(2,
           actionButton("go",
                        "Go!"))),

  #plots
  fluidRow(
    column(6, plotOutput("plot1")),
    column(6, plotOutput("plot2"))
  ),

  fluidRow(
    column(6, plotOutput("plot3")),
    column(6, plotOutput("plot4"))
  )
)

server <- function(input, output) {


  #run after we make a change
  observeEvent({
    input$mu.x
    input$mu.y
    input$var.x
    input$var.y
    input$rho
    input$go
  },{

    #define the number of sims
    sims = 10000

    #pull off the parameters
    mu.x = input$mu.x
    mu.y = input$mu.y
    var.x = input$var.x
    var.y = input$var.y
    rho = input$rho

    #generate standard normals with rho = 1/2 (correlation)
    data = rmvnorm(sims, mean = c(mu.x, mu.y),
                   sigma = matrix(c(var.x,
                                    rho*sqrt(var.x)*sqrt(var.y),
                                    rho*sqrt(var.x)*sqrt(var.y),
                                    var.y),
                                  nrow = 2, ncol = 2))


    #grab the data
    X = data[, 1]
    Y = data[, 2]

    #round so we can see
    X = round(X, 1)
    Y = round(Y, 1)


    #empirical heatmap
    output$plot1 <- renderPlot({

      #create the empirical heatmap; new color so we can see
      data <- data.frame(X, Y)

      data = group_by(data, X, Y)
      data = summarize(data, density = n())
      data$density = data$density/sims


      ggplot(data = data, aes(X, Y)) +
        geom_tile(aes(fill = density), color = "black") +
        scale_fill_gradient(low = "black", high= "red", name = "Density") +
        ggtitle("Empirical Density of X and Y") +
        theme(plot.margin = unit(c(1.8,.5,1.75,1.55), "cm")) +
        theme(plot.title = element_text(family = "Trebuchet MS",
                                        color="#383838", face="bold", size=25, vjust = 2)) +
        theme(axis.title.x = element_text(family = "Trebuchet MS",
                                          color="#383838", face="bold", size=22, vjust = -1.5),
              axis.title.y = element_text(family = "Trebuchet MS",
                                          color="#383838", face="bold", size=22, vjust = 2)) +
        theme(axis.text.x = element_text(face="bold", color="#383838",
                                         size=14),
              axis.text.y = element_text(face="bold", color="#383838",
                                         size=14)) +
        scale_x_continuous(limits = c(-10, 10)) +
        scale_y_continuous(limits = c(-10, 10))
    })


    #analytical heatmap
    output$plot2 <- renderPlot({


      #only operate if we don't have the endpoint or 0
      if(abs(rho) != 1 && var.x != 0 && var.y != 0){

        #define the supports/all combinations
        X.a = seq(from = -10, to = 10, length.out = 100)
        Y.a = seq(from = -10, to = 10, length.out = 100)
        data = expand.grid(X.a = X.a, Y.a = Y.a)

        #calculate density
        data$density = apply(data, 1, function(x){

          #define the constants we need

          c = 1/(2*pi*sqrt(var.x)*sqrt(var.y)*sqrt(1 - rho^2))
          d = -1/(2*(1 - rho^2))
          return(c*exp(d*(((x[1] - mu.x)/sqrt(var.x))^2 -
                            2*rho*((x[1] - mu.x)/(sqrt(var.x)))*((x[2] -  mu.y)/sqrt(var.y)) +
                            ((x[2] - mu.y)/sqrt(var.y))^2)))



          #otherwise, return 0
          return(0)

        })



        #generate a heatmap
        ggplot(data = data, aes(X.a, Y.a)) +
          geom_tile(aes(fill = density), color = "black") +
          scale_fill_gradient(low = "black", high= "red", name = "Density") +
          ggtitle("Analytical Density of X and Y") +
          theme(plot.margin = unit(c(1.8,.5,1.75,1.55), "cm")) +
          theme(plot.title = element_text(family = "Trebuchet MS",
                                          color="#383838", face="bold", size=25, vjust = 2)) +
          theme(axis.title.x = element_text(family = "Trebuchet MS",
                                            color="#383838", face="bold", size=22, vjust = -1.5),
                axis.title.y = element_text(family = "Trebuchet MS",
                                            color="#383838", face="bold", size=22, vjust = 2)) +
          theme(axis.text.x = element_text(face="bold", color="#383838",
                                           size=14),
                axis.text.y = element_text(face="bold", color="#383838",
                                           size=14)) +
          scale_x_continuous(limits = c(-10, 10)) +
          scale_y_continuous(limits = c(-10, 10))
      }
    })

    #textual comments
    output$plot3 <- renderPlot({

      #make a title
      title.x = paste0("X ~ N(", mu.x, ", ", var.x, ")")

      #histogram
      hist(X, main = title.x, col = "gray", xlab = "x")
    })

    output$plot4 <- renderPlot({

      #make a title
      title.y = paste0("Y ~ N(", mu.y, ", ", var.y, ")")

      #histogram
      hist(Y, main = title.y, col = "gray", xlab = "y")
    })

    output$text2 <- renderText({
      "X and Y are Bivariate Normal with a correlation of rho. The Analytical Density is undefined when
      Var(X) or Var(Y) is 0, or when rho = 1 or -1."
    })
  })
}

shinyApp(ui, server)
