library(shiny)

ui <- fluidPage(

  titlePanel("Hypergeometric"),

  #various buttons/slisder
  sidebarPanel(

    #parameters
    fluidRow(
      column(6, sliderInput("size",
                            "# of Draws",
                            min = 10,
                            max = 1000,
                            value = 100)),
      column(6, uiOutput("slider"))),

    fluidRow(
      column(4, sliderInput("w",
                            "w",
                            min = 1,
                            max = 10,
                            value = 1/2)),
      column(4, sliderInput("b",
                            "b",
                            min = 1,
                            max = 10,
                            value = 5)),
      column(4, actionButton("go", "Go!"), offset = 1.5)
    )
  ),

  #main plot
  mainPanel(
    column(12, plotOutput("plot1"), align = "center"), width = 8
  )
)


server <- function(input, output, session){


  #define the slider
  output$slider <- renderUI({
    sliderInput("n",
                "n",
                min = 1,
                max = input$b + input$w,
                value = input$w, step = 1)})


  observeEvent({
    input$go},
    {


      #create the plot
      output$plot1 <- renderPlot({



        #define the title, isolate reactions
        title <- isolate(paste0(input$size, " draws from X ~ Hyper(", input$w, ", ",
                                input$b, ", ", input$n, ")"))

        #define a starting point for data
        if(length(input$n) == 0){
          n = 100
        }

        if(length(input$n) > 0){
          n = input$n
        }

        #define the plot, isolate reactions
        data <- isolate(rhyper(input$size, input$w, input$b, n))

        #plot
        hist(data,
             main = title, col = rgb(0, 1, 0, 1/4),
             xlab = "", freq = FALSE, xlim = c(0, max(data)))

      })
    })
}



shinyApp(ui, server)
