library(shiny)

ui <- fluidPage(

  titlePanel("Gamma"),

  #various buttons/slisder
  sidebarPanel(

    #parameters
    fluidRow(
      column(6, sliderInput("size",
                            "# of Draws",
                            min = 10,
                            max = 1000,
                            value = 100)),
      column(6, sliderInput("alpha",
                            "alpha",
                            min = 1,
                            max = 10,
                            step = 1,
                            value = 2))),

    fluidRow(
      column(6, sliderInput("lambda",
                            "lambda",
                            min = 1/10,
                            max = 10,
                            value = 3,
                            step = 1/10)),
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
    input$alpha
    input$lambda
  }, {

    #create the plot
    output$plot1 <- renderPlot({

      #define the title
      title = paste0(input$size, " draws from X ~ Gamma(", input$alpha, ", ", input$lambda, ")")

      #create the plot
      hist(rgamma(input$size, input$alpha, input$lambda),
           main = title, col = rgb(0, 0, 1, 1/4),
           xlab = "", freq = FALSE)
    })
  })
}


shinyApp(ui, server)
