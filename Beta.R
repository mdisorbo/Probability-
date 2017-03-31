library(shiny)

ui <- fluidPage(

  titlePanel("Beta"),

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
                            min = 0,
                            max = 10,
                            step = 1/10,
                            value = 5))),

    fluidRow(
      column(6, sliderInput("beta",
                            "beta",
                            min = 0,
                            max = 10,
                            value = 5,
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
    input$beta
  }, {

    #create the plot
    output$plot1 <- renderPlot({

      #define the title
      title = paste0(input$size, " draws from X ~ Beta(", input$alpha, ", ", input$beta, ")")

      #create the plot
      hist(rbeta(input$size, input$alpha, input$beta),
           main = title, col = rgb(0, 1, 0, 1/4),
           xlim = c(0, 1), xlab = "", freq = FALSE)
    })
  })
}

shinyApp(ui, server)
