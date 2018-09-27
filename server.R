library(shiny)
library(DT)

source("ezvoigt.R")

server <- function(input, output, session) {
  
  observeEvent(input$file, {
    data <- reactive({read.csv(input$file$datapath)})
    output$table <- DT::renderDataTable(data())
    
    observeEvent(data(), {
      
      output$xaxis <- renderUI({
        selectInput("xaxis", "x-axis",
                    choices = colnames(data()))})
      
      output$Intensity <- renderUI({
        selectInput("Intensity", "Intensity", 
                    choices = colnames(data()))})
      
      x <- reactive({data()[, input$xaxis]})
      I <- reactive({data()[, input$Intensity]})
      
      df <- reactive({data.frame(x = x(), I = I())})
      
      x.min <- reactive({min(x())})
      x.max <- reactive({max(x())})
      
      
      output$x.min <- renderUI({
        numericInput("x.min", "Min", value = x.min(), min = x.min(), max = x.max())
      })
      
      output$x.max <- renderUI({
        numericInput("x.max", "Max", value = x.max(), min = x.min(), max = x.max())
      })
      
      df.range <- reactive({subset(df(), x >= x.min() && x <= x.max())})
      
      result <- reactive({
        Voigt.opt(x = df.range()$x, I = df.range()$I)
      })
      
      output$peak <- renderPlot({Voigt.plot(
        Voigt.df(x = df.range()$x, I = df.range()$I, result()$par)
      )})
      
      output$pbo.plot <- renderPlot({plot(result()$pbo)})

      
      
      
    })

   


  })
}

