library(shiny)
library(DT)

source("ezvoigt.R")

server <- function(input, output, session) {
  
  observeEvent(input$file, {
    
    print(input$file$name)
    
    data <- reactive({read.csv(input$file$datapath)})
    output$table <- DT::renderDataTable(data())
    
    observeEvent(data(), {
      
      output$xaxis <- renderUI({
        selectInput("xaxis", "x-axis",
                    choices = colnames(data()))})
      
      output$Intensity <- renderUI({
        selectInput("Intensity", "Intensity", 
                    choices = colnames(data()))})

      observeEvent(input$xaxis, {
        
        x <- reactive({data()[, input$xaxis]})
        I <- reactive({data()[, input$Intensity]})
        
        df <- reactive({data.frame(x = x(), I = I())})
        
        output$x.min <- renderUI({
          numericInput("x.min", "Min", value = min(x()), min = min(x()), max = max(x()))
        })
        
        output$x.max <- renderUI({
          numericInput("x.max", "Max", value = max(x()), min = min(x()), max = max(x()))
        })
        
        df.range <- reactive({subset(df(), x>=input$x.min & x<=input$x.max)})
        
        result <- reactive({
          Voigt.opt(x = df.range()$x, I = df.range()$I, 
                    maxit = input$maxit, s = input$s)
        })
        
        output$peak <- renderPlot({
          Voigt.plot(Voigt.df(x = df.range()$x, I = df.range()$I, result()$par)
        )})
        
        output$sum <- renderPrint(({summary(result())}))

      })
      
      
      
      
      
      
      
    })
    
    
    
    
  })
}