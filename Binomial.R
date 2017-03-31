library(shiny)

ui <- fluidPage(

  titlePanel("Binomial"),

  #various buttons/slisder
  sidebarPanel(

    #parameters
    fluidRow(
      column(6, sliderInput("size",
                            "# of Draws",
                            min = 10,
                            max = 1000,
                            value = 100)),
      column(6, sliderInput("n",
                            "n",
                            min = 1,
                            max = 30,
                            value = 10))),

    fluidRow(
      column(6, sliderInput("p",
                            "p",
                            min = 0,
                            max = 1,
                            value = 1/2,
                            step = 1/100)),
      column(6, actionButton("go", "Go!"), offset = 1.5)
    )
  ),

  #main plot
  mainPanel(
    column(12, plotOutput("plot1"), align = "center"), width = 8
  )
)







server <- function(input, output){


  #Only run after changing a parameter
  observeEvent({
    input$size
    input$go
    input$n
    input$p
  }, {

    #create the plot
    output$plot1 <- renderPlot({

      #define the title
      title = paste0(input$size, " draws from X ~ Bin(", input$n, ", ", input$p, ")")

      #create the plot
      hist(rbinom(input$size, input$n, input$p),
           main = title, col = rgb(0, 0, 1, 1/4),
           xlab = "", freq = FALSE, xlim = c(0, input$n))
    })
  })
}


shinyApp(ui, server)
