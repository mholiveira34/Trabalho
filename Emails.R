require(XML)
require(RCurl)
require(readtext)
require(stringr)
library(DBI)
library(RMySQL)

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

MySQL. <- MySQL() 
#MySQLcon <- dbConnect(MySQL., user='root', password='root', 
 #                     dbname='teste')

#dbCreateTable(con, "tabela", tab)
x=1
i <- "1.eml"
TAB <- data.frame()
for (i in dir()){
#con <- dbConnect(MySQL., user='root', password='root', 
                 #dbname='teste')
  print(x)
  msg <- readtext(i)
  arquivo <- msg[1]
  msg <- msg[2]
  
  infos <- unlist(str_split(msg, pattern = "\n"))
  
  data <- infos[grep(infos, pattern = "Date:")][1]
  
  from <- str_replace_all(infos[grep(infos, pattern = "From:")][1], pattern = '"', replacement = "")
  
  to <- str_replace_all(ifelse(length(infos[grep(infos, pattern = "^To:")])==0, 
               "Indisponível", infos[grep(infos, pattern = "^To:")] ), pattern = '"', replacement = "")
  
  reply <- str_replace_all(ifelse(length(infos[grep(infos, pattern = "Reply-To:")])==0, 
                  "Indisponível", infos[grep(infos, 
                                             pattern = "Reply-To:")]), pattern = '"', replacement = "")
  copia <- str_replace_all(ifelse(length(infos[grep(infos, pattern = "Cc:")])==0, 
                  "Indisponível", infos[grep(infos, 
                                             pattern = "Cc:")]), pattern = '"', replacement = "")
  
  assunto <- str_replace_all(cleanFun(ifelse(length(infos[grep(infos, pattern = "Subject:")]) ==0, 
                    "Indisponível", infos[grep(infos, 
                                               pattern = "Subject:")])), pattern = '"', replacement = "")
  
  ipenvio <- ifelse(length(infos[grep(infos, pattern = "SenderIP:")])==0, 
                    "Indisponível", infos[grep(infos,
                                               pattern = "SenderIP:")])
  
  conteudo <- str_replace_all(cleanFun(ifelse(length(grep(msg, pattern = "printable"))==0, 
                     "Indisponível", unlist(str_split(msg, 
                                                      pattern = "printable"))[-1])), pattern = ";", replacement = ",")
  conteudo <- unlist(str_split(conteudo, pattern = "Content-Disposition: attachment"))[1]
  conteudo <- unlist(str_split(conteudo, pattern ="Content-Type: image/jpeg"))[1]
  conteudo <- str_replace_all(conteudo, pattern = '"', replacement = "")
  tab <- data.frame(data, from, to, reply, copia, assunto, ipenvio, paste(conteudo, collapse = "\n"), stringsAsFactors = FALSE)

  write.table(tab, file = "tabela.csv", sep = ";", append = TRUE, col.names = FALSE)
  x=x+1
   #query <-   sqlAppendTable(con, "tabela", tab)
   #dbSendQuery(con, query)
   #dbDisconnect(con)
}
a <- aa$V9[4618]
dbCreateTable(con, "tabela", tab)
query <-   sqlAppendTable(con, "tabela", TAB[1,])
Sys.sleep()
dbSendQuery(con, query)


