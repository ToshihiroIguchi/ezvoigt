
file.input.accept <- c("text/csv", 
                       "text/comma-separated-values,text/plain", 
                       ".csv", ".ASC", ".int")


shinyUI(
  fluidPage(
    titlePanel("Peak separation by Voigt function"),
    sidebarLayout(
      sidebarPanel(
        
        fileInput("file", "File", accept = file.input.accept),
        
        tags$hr(),
        htmlOutput("xaxis"),
        htmlOutput("Intensity"),
        htmlOutput("x.min"),
        htmlOutput("x.max")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Table", DT::dataTableOutput("table")),
                    tabPanel("Peak", plotOutput("peak"), 
                             fluidRow(
                               column(4, verbatimTextOutput("sum")),
                               column(8, DT::dataTableOutput("Voigt.table"))
                             )),
                    tabPanel("Setting", 
                             h4("Particle Swarm Optimizer"),
                             numericInput("maxit", "The maximum number of iterations", 
                                          value = 500, min = 3),
                             numericInput("s", "The swarm size", 
                                          value = 20, min = 10)
                    )
                             
                    
        )
      )
    )
  )
)