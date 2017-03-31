library(shiny)

ui <- fluidPage(

  titlePanel("Negative Binomial"),

  #various buttons/slisder
  sidebarPanel(

    #parameters
    fluidRow(
      column(6, sliderInput("size",
                            "# of Draws",
                            min = 10,
                            max = 1000,
                            value = 100)),
      column(6, sliderInput("r",
                            "r",
                            min = 1,
                            max = 10,
                            value = 5))),

    fluidRow(
      column(6, sliderInput("p",
                            "p",
                            min = 1/100,
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
      title = paste0(input$size, " draws from X ~ NBinom(", input$r, ", ", input$p, ")")

      #generate data
      x = rnbinom(input$size, input$r, input$p)
      #create the plot
      hist(x, main = title, col = rgb(0, 1, 0, 1/4),
           xlab = "", freq = FALSE, xlim = c(0, max(x) + 1))
    })
  })
}

shinyApp(ui, server)
