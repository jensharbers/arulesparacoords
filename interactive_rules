library(shiny)
library(arules)

data("Groceries")
ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Arules Interactive Mining'),
    sidebarPanel(
      fileInput("basket","File Input",
                buttonLabel = "Browse...",
                placeholder = "No file selected",
                accept=".csv"),
      numericInput('support', 'Min support', 0.05, min = 0, max = 1, step = 0.05),
      numericInput('confidence', 'Min confidence',0.05, min = 0, max = 1,step = 0.05),
      numericInput('minlen', 'Min Rule length', 1, min = 1, max = 10,step = 1),
      numericInput('maxlen', 'Max Rule length', 5, min = 1, max = 10,step = 1),
      selectInput('target',"Choose Targets",choices=c("frequent itemsets","maximally frequent itemsets","generator frequent itemsets","closed frequent itemsets","rules","hyperedgesets"),selected="rules"),
      numericInput('maximumtime', 'Maximum Time [s]',10, min = 0, max = 20,step = 1)
    ),
    mainPanel(
      tableOutput('ruleset')
    )
  )
  
)

server <- function(input, output, session) {
  
  output$ruleset <- renderTable({
      rules <- apriori(Groceries, parameter = list(supp=input$support, 
                                                   conf=input$confidence, 
                                                   minlen=input$minlen,
                                                   maxlen=input$maxlen,
                                                   target=input$target,
                                                   maxtime=as.numeric(input$maximumtime)
                                                   ))
      
      DATAFRAME(rules)
      })
  output$basket <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath, header = input$header)
  })    
       
  
  }

shinyApp(ui, server)
