library(shiny)
library(DT)

source("ezvoigt.R")

server <- function(input, output, session) {
  
  observeEvent(input$file, {
    
    print(input$file$name)
    
    data <- reactive({read.profile(input$file$datapath)})
    output$table <- DT::renderDataTable(data())
    
    observeEvent(data(), {
      
      output$xaxis <- renderUI({
        selectInput("xaxis", "x-axis",
                    choices = colnames(data()),
                    selected = colnames(data())[1] )})
      
      output$Intensity <- renderUI({
        selectInput("Intensity", "Intensity", 
                    choices = colnames(data()),
                    selected = colnames(data())[2] )})

      observeEvent(input$xaxis, {
        
        x <- reactive({data()[, input$xaxis]})
        I <- reactive({data()[, input$Intensity]})
        
        df <- reactive({data.frame(x = x(), I = I())})
        
        output$x.min <- renderUI({
          numericInput("x.min", "Min", value = min(x()), min = min(x()), max = max(x()))
        })
        
        observeEvent(input$x.min, {
          output$x.max <- renderUI({
            numericInput("x.max", "Max", value = max(x()), min = min(x()), max = max(x()))
          })
          
          observeEvent(input$x.max, {
            
            output$peak.range <- renderUI({
              sliderInput("peak.range", "Peak x-axis range", 
                          min = input$x.min, max = input$x.max,
                          value = input$x.min + (input$x.max - input$x.min) * c(0.3, 0.7))
            })
            
            df.range <- reactive({subset(df(), x>=input$x.min & x<=input$x.max)})
            
            output$submit <- renderUI({
              actionButton("submit", "Analyze")
            })
            
            
            observeEvent(input$submit, {
              
              result <- reactive({
                Voigt.opt(x = df.range()$x, I = df.range()$I, 
                          maxit = input$maxit, s = input$s,
                          peak.range = input$peak.range,
                          method = input$opt.method)
              })
              
              output$peak <- renderPlot({
                Voigt.plot(Voigt.df(x = df.range()$x, I = df.range()$I, result()$par),
                           text.size =input$font.size)})
              
              output$Voigt.table <- DT::renderDataTable({
                
                #https://blog.rstudio.com/2015/06/24/dt-an-r-interface-to-the-datatables-library/
                datatable(table.Voigt.opt(result()), 
                          options = list(dom = "t")
                ) %>% 
                  formatRound(c("Voigt1", "Voigt2"), 3)
                
              })
              output$sum <- renderPrint({(result())})
              
            })
            
            
            
            
            
            
          })
        })
      })

    })

  })
}