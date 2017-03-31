#load libraries
library(grid)
library(gtools)
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
library(shinyjs)
library(V8)


#code to reset the page
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"


#define a function to draw the calendar
calendar <- function(){

  #clear the plot
  plot.new()

  #set wide margins
  par(mar=c(1/2,1/2,2,1/2))

  #lay the plot out
  plot(x = -1, y = -1, xlim = c(1/2, 20.5), ylim = c(1/2, 20.5),
       xaxs = "i", yaxs = "i",
       xaxt = 'n', yaxt = 'n', ylab = "", xlab = "",
       main = "")

  #draw a background
  rect(xleft = 1/2, xright = 20.5, ybottom = 1/2, ytop = 20.5,
       col = rgb(0, 0, 1, 1/4))

  #make the grid
  grid(20, 20, lty = 3, col = rgb(1, 0, 0, 1/4), lwd = 2)


  #black out excessive days
  rect(xleft = 1/2, ybottom = 1/2, xright = 20.5, ytop = 3/2,
       col = "gray4")
  rect(xleft = 5.5, ybottom = 1/2, xright = 20.5, ytop = 2.5,
       col = "gray4")

  #restore margins
  par(mar=c(5.1,4.1,4.1,2.1))
}


#define plotting function, out here
plotting <- function(days, i){

  #draw a new calendar on the first pass
  if(i == 1){
    calendar()
  }

  #unravel; find the x and y coordinates
  y.value = 20 - floor(days[i]/20)
  x.value = days[i]%%20 + 1

  #fix the corner case
  if(x.value == 6 && y.value == 2){
    x.value = 1
    y.value = 20
  }

  #if we have a match, color it in yellow
  if(as.numeric(table(days[1:i])[as.numeric(rownames(table(days[1:i]))) == days[i]]) > 1){
    rect(xleft = x.value - 1/2, xright = x.value + 1/2,
         ybottom = y.value - 1/2, ytop = y.value + 1/2,
         col = "firebrick1")
  }

  #plot the number of birthdays
  points(x.value, y.value,
         pch = as.character(as.numeric(table(days[1:i])[as.numeric(rownames(table(days[1:i]))) == days[i]])),
         col = "black", lwd = 8)
}



#define the ui
ui <- fluidPage(

  #code to refresh
  useShinyjs(),
  extendShinyjs(text = jsResetCode),

  titlePanel("The Birthday Problem"),

  #various buttons/slisder
  sidebarPanel(

    fluidRow(
      column(9, uiOutput("slider")),
      column(3, actionButton("end", "Reset"))
    ),

    textOutput("text1"),
    width = 4
  ),

  #main plot
  mainPanel(
    column(12, plotOutput("plot1"), align = "center"), width = 8
  )
)


#define the server
server <- function(input, output, session) {

  #define the slider
  output$slider <- renderUI({
    sliderInput("n",
                "Number of babies (press play)",
                min = 1,
                max = 365,
                value = 1,
                ticks = FALSE,
                animate = animationOptions(interval = 500,
                                           playButton = NULL,
                                           pauseButton = NULL))})


  #generate birthdays
  birthdays = sample(1:365, 365, replace = TRUE)

  #update every iteration
  observeEvent({
  input$n
  },{

  #update text
  output$text1 <- renderText({

    paste0("In general, there is a ",
           round(pbirthday(input$n), 2),
           " probability of a match with ", input$n, " people.")

  })



  #draw the calendar
  output$plot1 <- renderPlot({
    for(i in 1:input$n){
      plotting(birthdays, i)
    }
   })

  })

  #clear after we hit stop
  observeEvent({
    input$end
  },{

    #refresh page
    js$reset()

    #define the slider (start it over)
    output$slider <- renderUI({
      sliderInput("n",
                  "Number of babies",
                  min = 1,
                  max = 365,
                  value = 1,
                  ticks = FALSE,
                  animate = animationOptions(interval = 1000))})

    #draw the calendar
    output$plot1 <- renderPlot({
      #draw a new calendar
      calendar()
    })
  })
}


#run the app
shinyApp(ui, server)
