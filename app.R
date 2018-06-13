library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
               
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      uiOutput("Telefones"),
      

      uiOutput("dataInicio"),
      # Input: Select number of rows to display ----
      uiOutput("dataFim"), 
      actionButton("check", "Enviar"),
      
      fileInput("file2", "Choose CSV File",
                
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      uiOutput("Telefones2"),
      
      
      uiOutput("data")
      # Input: Select number of rows to display ----

      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  filedata1 <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = TRUE, sep = ";")
  })

  filedata2 <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath, header = TRUE, sep = ";")
  })
  
  
  output$Telefones <- renderUI({
    df <-filedata1()
    if (is.null(df)) return(NULL)
  
    items=names(df)
    names(items)=items
    selectInput("telefones", "Coluna de Telefone:", items)
  })
  
  output$Telefones2 <- renderUI({
    df2 <-filedata2()
    if (is.null(df2)) return(NULL)
    
    items=names(df2)
    names(items)=items
    selectInput("telefones2", "Coluna de Telefone:", items)
  })
    
    output$dataInicio <- renderUI({
      df <-filedata1()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      names(items)=items
      selectInput("datainicio", "Coluna de Data Inicial", items)
  
  })
    
    output$dataFim <- renderUI({
      df <-filedata1()
      if (is.null(df)) return(NULL)
      
      items=names(df)
      names(items)=items
      selectInput("datafim", "Coluna de Data Final", items)
      
    })
    
    output$data <- renderUI({
      df2 <-filedata2()
      if (is.null(df2)) return(NULL)
      
      items=names(df2)
      names(items)=items
      selectInput("data", "Coluna de Data", items)
      
    })
  observeEvent(input$check, output$contents <-
    renderTable({
    tabela <- read.csv(input$file1$datapath, header = TRUE, sep = ";")
    tabela$DATAINICIO <- as.Date.character(tabela[, names(tabela)[which(names(tabela) == input$datainicio)]], "%d/%m/%Y")
    tabela$DATAFIM <- as.Date.character(tabela[, names(tabela)[which(names(tabela) == input$datafim)]] ,"%d/%m/%Y")
    chamadas <- read.csv(input$file2$datapath, header = TRUE, sep = ";")
    chamadas$DATA <- as.Date.character(chamadas[, names(chamadas)[which(names(chamadas) == input$data)]], "%d/%m/%Y")
    ##
    telefones <- unique(chamadas[, names(chamadas)[which(names(chamadas) == input$telefones2)]])
    
    y <- data.frame()
    
    for (i in telefones){
      
      lig <- subset(chamadas, input$telefones2 == i)
      check <-  subset(tabela, input$telefones  == i)
      # RO == 1 indica que os periodos autorizados são os classificados com "1"
      check <- subset(check, RO == 1)
      vetor <- c()
      for (j in as.character(check$DATAINICIO[!is.na(check$DATAINICIO)])){
        vetor <- c(vetor, seq(as.Date(j), as.Date(subset(check$DATAFIM, check$DATAINICIO == j), "%d/%m/%Y")[1], by = "days"))
      }
      xx <- cbind(lig, as.numeric(lig$DATA) %in% vetor)
      y <- rbind(y, xx)    
    }
   print(y)
    ##
    
    
  })
    )
}

# Create Shiny app ----
shinyApp(ui, server)
