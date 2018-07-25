library(XML)
library(tm)
library(stringr)


#PEGA O TEMA DA BUSCA
chave1 <- 'bem+jurídico+e+administração+pública'
chave2 <- "penal+nao+militar"

tema <- URLencode(paste(c(chave1, chave2), collapse = "+e+"))
url <- paste0("http://www.stf.jus.br/portal/jurisprudencia/listarJurisprudencia.asp?s1=%28", tema, "%29&pagina=")
pagina <- htmlParse(paste0(url, 1, "&base=baseAcordaos"), 
                    encoding = "UTF-8")
NM <- sapply(xpathSApply(pagina, '//td[@align = "right"]'), xmlValue)
pgs <- as.numeric(paste0(as.vector(str_extract_all(NM, "[[:digit:]]")[[1]][-1]), collapse = ""))

#LE AS PAGINAS DE RESULTADOS
texto <- list()
for (i in c(1:pgs)){
  pagina <- htmlParse(paste0(url, i, "&base=baseAcordaos"), 
                      encoding = "UTF-8")
  print(i)
  Sys.sleep(1)
texto <- c(texto, xpathSApply(pagina, '//div[@class="processosJurisprudenciaAcordaos"]', saveXML) )
  }

#ESCREVE HTML DA BUSCA
write(unlist(texto), file = "lala.html")

#LÊ HTML DA BUSCA
x <- htmlParse("lala.html", encoding = "UTF-8")


titulos1 <- data.frame(t(sapply(xpathSApply(x, "/html/body/div/p/strong/input[2]"), xmlAttrs)), stringsAsFactors = FALSE)
titulos <-str_split(titulos1[, 3], " / ")
for (i in c(1:length(titulos))){
titulos[[i]] <- titulos[[i]][1]  
}

titulos <-unlist(titulos)

estado <- gsub(sapply(xpathSApply(x, "/html/body/div/p[1]/strong/text()[1]"), xmlValue), pattern = "  ", replacement = "")


tipo <- sapply(xpathSApply(x, "/html/body/div/p[1]/strong/text()[3]"), xmlValue)


ministro <- sapply(xpathSApply(x, "/html/body/div/p[1]/strong/text()[4]"), xmlValue)


data <- sapply(xpathSApply(x, "/html/body/div/p[1]/strong/text()[last()]"), xmlValue)


ementa <- sapply(xpathSApply(x, "/html/body/div/strong/div"), xmlValue)

#SELECIONA APENAS PARAGRAFOS DA EMENTA QUE MENCIONAM O ASSUNTO
ementa1 <- c()
for (i in c(1:length(ementa))){
frases <-  str_split(ementa[i], "[[:digit:]]\\. ")
frasesc <- c(frases[[1]][grep(tolower(frases[[1]]), pattern = "bem jurídico")], 
                 frases[[1]][grep(tolower(frases[[1]]), pattern = "administração")], 
             frases[[1]][grep(tolower(frases[[1]]), pattern = "juridic")], 
             frases[[1]][grep(tolower(frases[[1]]), pattern = "bens jurídicos")])
frasesc <- paste(frasesc, collapse = ", ")
if(length(frasesc)>0){
ementa1 <- c(ementa1, frasesc)}
else{
  ementa1 <- c(ementa1, "NULL")
}
}
decisão <- sapply(xpathSApply(x, "/html/body/div/div[1]"), xmlValue)



#CONSTRÓI TABELA 

tabela <- data.frame(titulos, data, tipo, estado, ministro, ementa1, decisão)
#TRATA
for (i in c(1:7)){
  tabela[, i] <- gsub(tabela[, i], pattern = " / ", replacement = "")
  tabela[, i] <- gsub(tabela[, i], pattern = "  ", replacement = "")
  tabela[, i] <- str_remove_all(tabela[, i], pattern = "\r")
  tabela[, i] <- str_remove_all(tabela[, i], pattern = "\n")
  tabela[, i] <- str_remove_all(tabela[, i], pattern = "\t")
  tabela[, i] <- str_remove_all(tabela[, i], pattern = "  ")
  tabela[, i] <- str_remove_all(tabela[, i], pattern = ";")
}
tabela[, 2] <- str_remove_all(tabela[, 2], "Julgamento:&nbsp")
tabela[, 2] <- str_remove_all(tabela[, 2], "&nbsp")

datas <- str_split(tabela$data, " Ó")
for (i in c(1:length(datas))){
  datas[[i]] <- datas[[i]][1]  
}
tabela$data <- str_remove_all(unlist(datas), pattern = " ")
library("xlsx")
#CRIA ARQUIVO DO EXCEL COM A TABELA.
write.xlsx(tabela, "bemjuridicoepenalstf.xlsx")




#############


###COLABORAÇÃO PREMIADA - STJ FAZER SELENIUM GRR
library(stringr)
library(RSelenium)
library(XML)

rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]

remDr$navigate("http://www.stj.jus.br/SCON/")
#pega o tema
tema <- '"administração pública" e "bem jurídico" e penal nao militar'
#chave2 <- "penal"
tema <- paste(chave1, chave2, sep = " e ")
#entra na pagina e pesquisa o tema
box <- remDr$findElement(using = 'xpath', "//input[@id = 'pesquisaLivre']")
box$sendKeysToElement(list(tema, key = "enter"))
acordaos <- remDr$findElement(using = "xpath", "/html/body/div/div[6]/div/div/div[3]/div[2]/div/div/div/div[3]/div[3]/span[2]/a")
acordaos$clickElement()
TABELA <- data.frame()
for (i in c(1:600)){
#pega os nomes e textos dos elementos da pagina
texto <- c()
infos <- remDr$findElements(using = "xpath", "//*[@class = 'docTexto'][1]")
for (i in c(1:length(infos))){
  texto <- c(texto, as.character(infos[[i]]$getElementText()))
}
nomes <- c()
infos2 <- remDr$findElements(using = "xpath", "//*[@class = 'docTitulo']")
for (i in c(1:length(infos2))){
  nomes <- c(nomes, as.character(infos2[[i]]$getElementText()))
}

tab <- data.frame(nomes,texto, stringsAsFactors = FALSE)

titulo <- tab[tab$nomes == "Processo", 2]
relator <- tab[tab$nomes == "Relator(a)", 2]
data <- tab[tab$nomes == "Data do Julgamento", 2]
conteudo <- tab[tab$nomes == "Ementa", 2]
decisao <- tab[tab$nomes == "Acórdão", 2]

id <- str_split(titulo, " / ")
estado <- c()
for (i in c(1:length(id))){
  estado[i] <- id[[i]][2]  
}
estado <- str_split(estado, "\n")


for (i in c(1:length(id))){
  id[[i]] <- id[[i]][1]  
}
id <- unlist(id)

tipo <- c()
for (i in c(1:length(estado))){
  tipo <- c(tipo, estado[[i]][2])
}
local <- c()
for (i in c(1:length(estado))){
  local<- c(local, estado[[i]][1])
}
tabela <- data.frame(id, tipo, local, relator, data, conteudo, decisao)
TABELA <- rbind(TABELA, tabela)
prox <- remDr$findElement(using = "xpath", "//a[@class = 'iconeProximaPagina']")
prox$clickElement()
}
conteudo1 <- c()
for (i in c(1:nrow(TABELA))){
  frases <-  str_split(TABELA$conteudo[i], "[[:digit:]]\\. ")
  frasesc <- c(frases[[1]][grep(tolower(frases[[1]]), pattern = "bem jurídico")], 
               frases[[1]][grep(tolower(frases[[1]]), pattern = "administração")], 
               frases[[1]][grep(tolower(frases[[1]]), pattern = "juridic")], 
               frases[[1]][grep(tolower(frases[[1]]), pattern = "bens jurídicos")])
  frasesc <- paste(frasesc, collapse = ", ")
  if(length(frasesc)>0){
    conteudo1 <- c(conteudo1, frasesc)}
  else{
    conteudo1 <- c(conteudo1, "NULL")
  }
}

TABELA$conteudo <- conteudo1

write.xlsx(TABELA, file = "bemjuridicoepenalstj.xlsx")
