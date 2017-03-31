library(shiny)

ui <- fluidPage(

  titlePanel("Normal"),

  #various buttons/slisder
  sidebarPanel(

    #parameters
    fluidRow(
      column(6, sliderInput("size",
                            "# of Draws",
                            min = 10,
                            max = 1000,
                            value = 100)),
      column(6, sliderInput("mu",
                            "mu",
                            min = -5,
                            max = 5,
                            value = 0))),

    fluidRow(
      column(6, sliderInput("sigma",
                            "sigma",
                            min = 0,
                            max = 10,
                            value = 1,
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
    input$mu
    input$sigma
  }, {

    #create the plot
    output$plot1 <- renderPlot({

      #define the title
      title = paste0(input$size, " draws from X ~ N(", input$mu, ", ", input$sigma^2, ")")

      #create the plot
      hist(rnorm(input$size, input$mu, input$sigma),
           main = title, col = rgb(0, 0, 1, 1/4),
           xlab = "", freq = FALSE, xlim = c(input$mu - 3.5*input$sigma, input$mu + 3.5*input$sigma))
    })
  })
}


shinyApp(ui, server)
