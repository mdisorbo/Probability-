library(shiny)

ui <- fluidPage(

  titlePanel("Poisson"),

  #various buttons/slisder
  sidebarPanel(

    #parameters
    fluidRow(
      column(6, sliderInput("size",
                            "# of Draws",
                            min = 10,
                            max = 1000,
                            value = 100)),
      column(6, sliderInput("lambda",
                            "lambda",
                            min = 1,
                            max = 30,
                            value = 10))),

    fluidRow(
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
    input$lambda
  }, {

    #create the plot
    output$plot1 <- renderPlot({

      #define the title
      title = paste0(input$size, " draws from X ~ Pois(", input$lambda, ")")

      #create the plot
      hist(rpois(input$size, input$lambda),
           main = title, col = rgb(0, 1, 0, 1/4),
           xlab = "", freq = FALSE, xlim = c(0, input$lambda*2.5))
    })
  })
}

shinyApp(ui, server)
