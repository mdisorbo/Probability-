library(shiny)

ui <- fluidPage(

  titlePanel("Gambler's Ruin"),

  #various buttons/slisder
  sidebarPanel(

    fluidRow(
      column(4, sliderInput("N",
                            "N",
                            min = 2,
                            max = 100,
                            value = 50,
                            ticks = FALSE)),
      column(4, uiOutput("slider")),
      column(4, sliderInput("p",
                            "p",
                            min = 0,
                            max = 1,
                            value = 1/2,
                            ticks = FALSE,
                            step = 1/10))

    ),
    fluidRow(
      column(3, actionButton("go", "Go!")),
      column(9, textOutput("text1")))
  ),

  #main plot
  mainPanel(
    column(12, plotOutput("plot1"), align = "center"), width = 8

  )
)


server <- function(input, output){

  output$slider <- renderUI({

    sliderInput("i",
                "i",
                min = 0,
                max = input$N,
                value = round(input$N/2),
                step = 1,
                ticks = FALSE)
  })


  #update every iteration
  observeEvent({
    input$N
    input$p
    input$i
    input$go
  },{

    #grab parameters
    N = input$N
    p = input$p
    i = input$i

    #find win probability
    #case where p = 0
    if(p == 0){
      p.win = 0
    }

    #find the probability of winning; two cases
    if(p != 1/2 && p != 0){
      p.win = (1 - ((1 - p)/p)^i)/(1 - ((1 - p)/p)^N)
    }
    if(p == 1/2){
      p.win = i/N
    }


    #usual case
    if(p.win <= .999 && p.win > .001){

      #update text
      output$text1 <- renderText({
        paste0("In general, there is probability ",
               round(p.win, 3),
               " of winning with these parameters.")
      })
    }

    #edge case 1
    if(p.win > .999){

      #update text
      output$text1 <- renderText({
        paste0("In general, there is probability >.999
               of winning with these parameters.")
      })
      }

    #edge case 2
    if(p.win < .001){
      #update text
      output$text1 <- renderText({
        paste0("In general, there is probability <.001
               of winning with these parameters.")
      })
      }


    #guaranteed win (p)
    if(p == 1){
      #update text
      output$text1 <- renderText({
        paste0("In general, there is probability
               1 of winning with these parameters.")
      })
      }

    #guaranteed loss (p)
    if(p == 0){
      #update text
      output$text1 <- renderText({
        paste0("In general, there is probability 0
               of winning with these parameters.")
      })
      }

    #guaranteed win override (i)
    if(i == N){
      #update text
      output$text1 <- renderText({
        paste0("In general, there is probability
               1 of winning with these parameters.")
      })
      }

    #guaranteed loss override (i)
    if(i == 0){
      #update text
      output$text1 <- renderText({
        paste0("In general, there is probability
               0 of winning with these parameters.")
      })
      }

    #initialize
    i = input$i
    N = input$N
    p = input$p

    #start the path
    path = i

    #run the loop until we hit 0 or 1
    while(path[length(path)] != 0 && path[length(path)] != N){

      #flip to see if we go up or down
      flip = runif(1)

      #go up
      if(flip <= p){
        path = c(path, path[length(path)] + 1)
      }

      #go down
      if(flip > p){
        path = c(path, path[length(path)] - 1)
      }
    }

    #plot

    output$plot1 <- renderPlot({

      #lost
      if(path[length(path)] < N){
        title = paste0("You lost! The game took ", length(path) - 1, " turns.")
      }

      #won
      if(path[length(path)] == N){
        title = paste0("You won! The game took ", length(path) - 1, " turns.")
      }

      plot(path, main = title,
           type = "l", col = "firebrick3", lwd = 3,
           ylim = c(0, N), xlab = "Round",
           ylab = "Winnings", xaxt = 'n',
           xlim = c(1, length(path)))
      abline(h = i, col = "gray30")
    })
    })
}

shinyApp(ui, server)
