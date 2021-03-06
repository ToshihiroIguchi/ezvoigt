
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
        
        fluidRow(
          column(6, htmlOutput("x.min")),
          column(6, htmlOutput("x.max"))
        ),

        htmlOutput("peak.range"),
        htmlOutput("submit")
        
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Table", DT::dataTableOutput("table")),
                    tabPanel("Peak", plotOutput("peak"), 
                             fluidRow(
                               column(6, verbatimTextOutput("sum")),
                               column(6, DT::dataTableOutput("Voigt.table"))
                             )),
                    tabPanel("Setting", 
                             h4("Number of optimizations"),
                             numericInput("opt.times", "Number of optimizations", 
                                          value = 5, min = 1),
                             tags$hr(),
                             
                             h4("Particle Swarm Optimizer"),
                             h5("Global optimization"),
                             numericInput("maxit", "The maximum number of iterations", 
                                          value = 50, min = 3),
                             numericInput("s", "The swarm size", 
                                          value = 20, min = 10),
                             
                             tags$hr(),
                             h4("General-purposec Optimization"),
                             h5("Loal optimization"),
                             
                             
                             selectInput("opt.method", 
                                         "method", 
                                         choices = c("Nelder-Mead", "BFGS", 
                                                     "CG", "L-BFGS-B", "SANN"),
                                         selected = "BFGS"),
                             
                             tags$hr(),
                             h4("Graph"),
                             sliderInput("font.size", "Font size", 
                                         value = 15,
                                         min = 5, max = 30)
                    )
        )
      )
    )
  )
)