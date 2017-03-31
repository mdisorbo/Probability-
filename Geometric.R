library(shiny)

ui <- fluidPage(

  titlePanel("Geometric"),

  #various buttons/slisder
  sidebarPanel(

    #parameters
    fluidRow(
      column(6, sliderInput("size",
                            "# of Draws",
                            min = 10,
                            max = 1000,
                            value = 100)),
      column(6, sliderInput("p",
                            "p",
                            min = 1/100,
                            max = 1,
                            value = 1/2,
                            step = 1/100))),

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
    input$p
  }, {

    #create the plot
    output$plot1 <- renderPlot({

      #define the title
      title = paste0(input$size, " draws from X ~ Geom(", input$p, ")")

      #create the plot
      hist(rgeom(input$size, input$p),
           main = title, col = rgb(1, 0, 0, 1/4),
           xlab = "", freq = FALSE, xlim = c(0, 4/input$p))
    })
  })
}


shinyApp(ui, server)
