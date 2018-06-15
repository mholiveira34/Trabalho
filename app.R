library(shiny)
library(shinythemes)
options(shiny.maxRequestSize=30*1024^2) 

foradoprazo <- function(y, tabela, input){
  falsos <- subset(y, y[, ncol(y)] == FALSE)
  checknova <- subset(tabela, RO==1)
  x <- data.frame()
  for (i in c(1:nrow(falsos))){
    fone <- falsos[, which(names(falsos) == input$telefones2)][i]
    if(fone %in% checknova[, which(names(checknova) == input$telefones)]){
      grampos <-checknova[checknova[, which(names(checknova) == input$telefones)]  == fone, ]
      #grampos <- subset(grampos, RO == 1)
      d6dia <- grampos$DATAFIM +1
      
      if(falsos$DATA[i] %in% d6dia){x <- rbind(x, falsos[i, ])}
      
     
    }}
   return(x)
}

classif <- function(chamadas, tabela, input){
  tabela$DATAINICIO <- as.Date.character(tabela[, names(tabela)[which(names(tabela) == input$datainicio)]], "%d/%m/%Y")
  tabela$DATAFIM <- as.Date.character(tabela[, names(tabela)[which(names(tabela) == input$datafim)]] ,"%d/%m/%Y")
  chamadas$DATA <- as.Date.character(chamadas[, names(chamadas)[which(names(chamadas) == input$data)]], "%d/%m/%Y")
  telefones <- unique(chamadas[, which(names(chamadas) == input$telefones2)])
  y <- data.frame()
  for (i in telefones){
    
    lig <- chamadas[chamadas[, which(names(chamadas) == input$telefones2)] == i, ]
    
    check <- tabela[tabela[, which(names(tabela) == input$telefones)]  == i, ]
    # RO == 1 indica que os periodos autorizados são os classificados com "1"
    check <- subset(check, RO == 1)
   
    vetor <- c()
    for (j in as.character(check$DATAINICIO[!is.na(check$DATAINICIO)])){
      vetor <- c(vetor, seq(as.Date(j), as.Date(subset(check$DATAFIM, check$DATAINICIO == j), 
                                                "%d/%m/%Y")[1], by = "days"))
    }
    
    xx <- cbind(lig, ifelse(as.numeric(lig$DATA) %in% vetor==TRUE, 
                            "Autorizada", FALSE))
    y <- rbind(y, xx) 
  } 
  x <- foradoprazo(y, tabela, input)
  y[, ncol(y)] <- ifelse(y[, which(names(y) == input$ID)] %in% x[, which(names(x)==input$ID)],
                         "Fora do Prazo", y[, ncol(y)] )
  vetor <- c()
  for (i in y[, ncol(y)]){
    if( i == 1){
      vetor <- c(vetor, "Autorizada")
    }
    else{if(i == 2){
      vetor <- c(vetor, "Não Autorizada")
    }
    else{ vetor <- c(vetor, "Fora do Prazo")}}
    
  }
  y <- cbind(chamadas, vetor)
  colnames(y) <-  c(names(chamadas), "classificação")
  return(y)
}




# Define UI for data upload app ----
ui <- navbarPage( theme = shinytheme("united"), 
  
  # App title ----
"Classificação de Interceptação Telefônica",
  
  # Sidebar layout with input and output definitions ----
 tabPanel("Tabela",
    
    # Sidebar panel for inputs ----
    sidebarPanel( 
      
      # Input: Select a file ----
      fileInput("file1", "Escolha o arquivo com as datas de início e fim dos períodos de interceptação.",
                
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      uiOutput("Telefones"),
      
      
      uiOutput("dataInicio"),
      # Input: Select number of rows to display ----
      uiOutput("dataFim"), 
     
      
      fileInput("file2", "Escolha o arquivo com os dados das chamadas interceptadas",
                
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      uiOutput("Telefones2"),
      
      uiOutput("id"), 
      uiOutput("data"), 
      actionButton("check", "Enviar"), 
      downloadButton("download", "Download")
    
      # Input: Select number of rows to display ---
    
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(tabsetPanel(
      
      # Output: Data file ----
      tabPanel("Tabela Completa", value =1, 
               h3(textOutput("Tabela de Chamadas Classificadas")), 
               tableOutput("contents"), id = "tabselected"),
      
      tabPanel("Resumo", value = 1,
               h3(textOutput("Totais")), 
               verbatimTextOutput("resumo"), 
               h3(textOutput("Classificações por Telefone")),
               verbatimTextOutput("tabelaClass"))
      
    )
    )
    
  ), 
tabPanel("Gráficos", 
         sidebarPanel(
           h3(textOutput("Gráficossss")),
           uiOutput("seletor")), 
         mainPanel(
           
         ))
)

# Define server logic to read selected file ----
server <- function(input, output) {
  filedata1 <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = TRUE, sep = ";", stringsAsFactors = FALSE)
  })
  
  filedata2 <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath, header = TRUE, sep = ";", stringsAsFactors = FALSE)
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
  
  output$Seletor <- renderUI({
  req(y)
    items=unique(y[, input$telefones2])
    names(items)=items
    selectInput("seletor", "Selecione o Terminal", items)
    
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
  
  output$id <- renderUI({
    df2 <-filedata2()
    if (is.null(df2)) return(NULL)
    
    items=names(df2)
    names(items)=items
    selectInput("ID", "Coluna de ID", items)
    
  })
  
  observeEvent(input$check, {
    tab <- reactive({  tabela <- filedata1() #read.csv(input$file1$datapath, header = TRUE, sep = ";")
    
    chamadas <- filedata2() #read.csv(input$file2$datapath, header = TRUE, sep = ";")
    
    y <<- classif(chamadas, tabela, input)
    y
    })
    
    
    output$contents <- renderTable({ tab()} )
                  
                
    output$resumo <- renderPrint({
      tabela1 <- tab()
      summary(tabela1[, ncol(tabela1)])
    })
  
    output$tabelaClass <- renderPrint(table(y[, input$telefones2], y$classificação))
  
    output$download <-
      downloadHandler(
      filename = paste0("download_", Sys.Date(),".csv"), 
      content = function(file){
        write.csv2(y, file)}
     )})
}

# Create Shiny app ----
shinyApp(ui, server)
