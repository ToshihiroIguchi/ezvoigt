
file.input.accept <- c("text/csv", "text/comma-separated-values,text/plain", ".csv")

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
                    tabPanel("Peak", plotOutput("peak")),
                    tabPanel("Optim", plotOutput("pbo.plot"))
                    
        )
      )
    )
  )
)