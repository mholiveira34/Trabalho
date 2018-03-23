#########################################
#funções utilizadas
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

BigramTokenizer <- function(x) {
  RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 2))
}
#########################################
#pacotes utilizados
library(tm)
library(readtext)
library(pdftools)
library(tokenizers)
library(stringr)
library(RWeka)
library(XML)
###############################
#carrega arquivos word, cria um único vetor com todos.
cname <- file.path("~", "Publicano depoimentos/")   
getwd()


setwd("~/Publicano depoimentos")
docs <- c()
for (i in dir(cname)){
  x <- readtext(i, encoding = "UTF-8")
  print(i)
  docs <- rbind(docs, x)
}
load("Depoimentos.RData")
save(docs, file = "Depoimentos.RData")
summary(docs)

#Tratamento - Não remover pontuação antes de separar frases
docs$text <- tolower(docs$text) #CAIXA BAIXA    
#docs <- removePunctuation(docs)
docs$text <- rm_accent(docs$text) #REMOVE ACENTOS
docs$text <- removeNumbers(docs$text) #REMOVE NUMEROS 
docs$text <-  str_replace_all(docs$text,"[^[:graph:]]", " ") #REMOVE CARACTERES GRÁFICOS
docs$doc_id <- toupper(docs$doc_id) #PASSA OS NOMES DOS DOCUMENTOS PARA CAIXA ALTA

nome <- "orlando" #nome do cliente


#encontra documentos que citam o Nome

docsnome <- docs[unique(c(grep(docs$text, pattern = nome), grep(docs$text, pattern = "aranda"))), ] #nome e sobrenome


cit <- c()
str(docsnome)
#conta o número de vezes em que o Nome é citado em cada depoimento.
for (i in docsnome$text){
  termos <- tokenize_sentences(i)
  num <- length(unique(c(grep(termos[[1]], pattern = "orlando"), 
                         #grep(termos[[1]], pattern = "lovato"), 
                         grep(termos[[1]], pattern = "aranda")))) 
  num3 <- length(unique(c(grep(termos[[1]], pattern = "botelho"), grep(termos[[1]], pattern = "orlando antonio")) )) 
  print(num, num3)
  #num2 <- length(unique(grep(termos[[1]], pattern = "machado") ))
  cit <- c(cit, num-num3)
}

cit

docsnomecit <- cbind(docsnome, cit)
docsnomecit <- docsnomecit[-which(cit < 1), ] #remove da lista documentos sem citações (zeros e valores negativos)

#trata os nomes dos documentos
docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = "^(.*)-", replacement = "")

docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = "^\\s*", replacement = "")
docsnomecit$doc_id <- removePunctuation(docsnomecit$doc_id)
docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = "VIDEODEAUDIENCIAREU", replacement = "")
docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = "TESTEMUNHA", replacement = "")

docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = "DOCX", replacement = "")
docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = "DOC", replacement = "")
docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = "TA  ", replacement = "")

docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = " $", replacement = "")
docsnomecit$doc_id <- rm_accent(docsnomecit$doc_id)
docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = "SOUVA", replacement = "SOUZA")
docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = "LUIZ ANTONIO DE SOUZA", replacement = "LUIZ ANTONIO")
docsnomecit$doc_id <- removeNumbers(docsnomecit$doc_id)
docsnomecit$doc_id <- gsub(docsnomecit$doc_id, pattern = "\\sI$", replacement = "")


#cria tabela com citações por documento
citacoes <- data.frame(docsnomecit$cit, colnames <- docsnomecit$doc_id )

citac <- aggregate(citacoes[,1], list(citacoes[,2]), sum)
x <- citac$x[which(citac$x <=1)]
#agrupa citações únicas
citacadj <- rbind(citac[-which(citac$x <=1), ], data.frame(Group.1 = "OUTROS", x = sum(x)))

citacadj <- citacadj[-grep(citacadj$Group.1, pattern = "ORLANDO COELHO ARANDA"), ]








#########################################

frasesnome <- data.frame(docsnomecit$doc_id, docsnomecit$text)
result <- aggregate(frasesnome$docsnomecit.text, list(frasesnome$docsnomecit.doc_id) , data = frasesnome, paste, collapse = " ")
result <- cbind(result, citac$x)
result <- result[which(result$`citac$x`>1), ]
lista <- tokenize_sentences(result$x)
names(lista) <- result$Group.1
for (i in names(lista)){
  lista[[i]] <- lista[[i]][unique(c(grep(lista[[i]], pattern = "orlando", grep(lista[[i]], pattern = "aranda"))))]
  if(length(unique(c(grep(lista[[i]], pattern = "botelho"), grep(lista[[i]], pattern = "orlando antonio"))))> 0){
  lista[[i]] <- lista[[i]][-unique(c(grep(lista[[i]], pattern = "botelho"), grep(lista[[i]], pattern = "orlando antonio")))]
  }}

lista <- lapply(lista, FUN = paste, collapse = " ")
tab <- data.frame(names(lista), unlist(lista))
colnames(tab) <- c("doc_id", "text")
corpuscomp <- Corpus(DataframeSource(tab))
ndocs <- length(corpuscomp)
# Ignora palavras muito raras
minTermFreq <- ndocs * 0.01
# ignora palavras muito comuns
maxTermFreq <- ndocs * 0.95

dtm = DocumentTermMatrix(corpuscomp,
                         control = list( 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))
dtm.matrix = as.matrix(dtm)
inspect(dtm)
library(proxy)
distMatrix <- dist(dtm.matrix)
?dist
x11()
groups <- hclust(distMatrix, method="ward.D")
plot(groups, cex=0.9, hang=-1,  main = "Dendograma - Distâncias entre Citações", sub = "" , xlab = "", ylab = "Distância")
rect.hclust(groups, k=4)
################################################







#gráfico de frequência por depoimento
par(mar=c(18,4,2,2))
barplot(height = citacadj$x,
        horiz = FALSE, 
        names.arg = citacadj$Group.1,
        las = 2, 
        ylim = c(0, 42), 
        col = "skyblue", 
        main = "Citações por Depoimento - Orlando Coelho Aranda", 
        ylab = "número de citações")
x11()

par(mar=c(4,4,4,4))

library(igraph)
#rede de citações
links <- data.frame(citac$Group.1, "ORLANDO COELHO ARANDA", citac$x)
links <- subset(links, citac.x != 0) 
net <- graph_from_data_frame(d=links, vertices=links$citac.Group.1, directed=T) 
net <- simplify(net, remove.loops = T)
X11()
deg <- degree(net, mode="all")
V(net)$size <- deg*3
V(net)$size <- links$citac.x
plot(net, edge.arrow.size=.05, edge.curved=0,
     vertex.color="skyblue", vertex.frame.color="#999999",
     vertex.label.color="#000000",
     vertex.label.cex=.7, 
     vertex.label = ifelse(links$citac.x >= 5, names(V(net)), NA) ,#,layout=layout_with_lgl, 
     main = "Rede de Citações - Orlando Coelho Aranda", 
     sub = "Identificados depoimentos onde foi citado mais de 5 vezes") 



#separa os depoimentos que citam o Nome em frases e cria corpus     
docnovo <- c()
for (i in docsnomecit$docs){
docnovo <- paste(docnovo, i)}
frases <- tokenize_sentences(docnovo)

#seleciona apenas frases que contem o Nome

frasesnome <- frases[[1]][unique(c(grep(frases[[1]], pattern = "orlando"),
                                   grep(frases[[1]], pattern = "aranda")))]

frasesnome <- frasesnome[-grep(frasesnome, pattern = "orlando antonio")]#exclui frases sobre o SEGUNDO orlando
frasesnome <- frasesnome[-grep(frasesnome, pattern = "orlando botelho")]
frasesnome <- removeWords(frasesnome, words = tm::stopwords("portuguese"))
frasesnome <- removeWords(frasesnome, words = c("acusado", "nao", "acusada", "juiz", "ministerio publico", "senhor"))
frasesnome <- removePunctuation(frasesnome)
frasesnome <- noquote(frasesnome)
frasesnome <- str_replace_all(frasesnome,"[^[:graph:]]", " ") 
frasesnome <- str_replace_all(frasesnome,'"', " ") 

#núvem de palavras das frases que citam o Nome
library(wordcloud)
par(mar = c(10, 10, 10, 10))
X11()
wordcloud(frasesnome,
          max.words=200, random.order=FALSE,  
          colors=brewer.pal(6, "Dark2"), main = "Frases em que o Cliente é Citado")

corpus2 <- VCorpus(VectorSource(x = noquote(frasesnome)), #cada frase é um documento, todas citam o Nome
                   readerControl = list(language = "pt",
                                                                                load = TRUE))

mdoc <- TermDocumentMatrix(corpus2)
dtm <- DocumentTermMatrix(corpus2)

##########
ndocs <- length(corpus2)
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .9
dtm2 = DocumentTermMatrix(corpus2,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))
#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
#print(as.matrix(dtm))
write.csv((as.matrix(dtm)), "test.csv")
#head(sort(as.matrix(dtm)[18,], decreasing = TRUE), n=15)
dtm.matrix = as.matrix(dtm)
#wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 20)
inspect(dtm2)
dtm <- weightTfIdf(dtm2, normalize = TRUE)
dtm.matrix = as.matrix(dtm)
x11()
wordcloud(colnames(dtm.matrix), dtm.matrix[28, ], max.words = 200, colors=brewer.pal(6, "Dark2"), random.order = FALSE)
#inspect(dtm)
wordcloud(colnames(dtm.matrix), dtm.matrix[3, ], max.words = 200)


###############
m <- as.matrix(mdoc)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#palavras e suas frequências (entre frases que citam o Nome)
par(mar=c(5,8,2,2))
barplot(height = d$freq[1:30], names.arg = d$word[1:30], las = 1, horiz = TRUE)


mft <- findFreqTerms(dtm, lowfreq = 100)
#palavras associadas ao Nome
assocs <- findAssocs(dtm, "aranda", corlimit = 0.2)
assocs <- data.frame(palavra = names(assocs[["aranda"]]), value = assocs[["aranda"]], row.names=NULL)
noquote(assocs)
#gráfico de palavras associadas e seu nível de associação
barplot(height = assocs$value[1:30], 
        names.arg = assocs$palavra[1:30], 
        horiz = TRUE, 
        las = 1, 
        xlim = c(0:1),
        col = heat.colors(30, alpha=0.7), main = "Palavras Associadas a Orlando Coelho Aranda", 
        xlab = "Nível de Associação")
#núvem de palavras associadas e nível de associação
x11()
wordcloud(words = assocs$palavra, freq = assocs$value*100,
          random.order=FALSE, max.words = 100, 
          colors=brewer.pal(6, "Dark2"))



setwd("~/")
sent <- read.table("lexico_v3.0.txt",
                   header = FALSE,
                   sep = ",",
                   quote = "",
                   stringsAsFactors = FALSE)
names(sent) <- c("term", "class", "pol", "ann")
head(sent)


inter <- intersect(x = Terms(dtm),
                   y = sent$term)
length(inter)

lex <- merge(x = data.frame(term = inter,
                            stringsAsFactors = FALSE),
             y = sent,
             sort = FALSE)
str(lex)
m <- as.matrix(dtm)
m <- m[, lex$term]
nterm <- rowSums(m)
ncol(m) == nrow(lex)
all(colnames(m) == lex$term)
polaridades <- m %*% cbind(lex$pol)
#polaridades <- polaridades/nterm
polaridades <- data.frame(polaridades)

polaridades[which(abs(polaridades$polaridades) <0.05), ] <- 0
polaridades[which(abs(polaridades$polaridades) == "NaN"), ] <- 0
polaridades$Classificação <- ifelse(polaridades>0,"Positivo",
                                 ifelse(polaridades< 0,
                                       "Negativo",
                                        ifelse(polaridades==0,"Neutro",0)))


summary(polaridades$Classificação)
library(ggplot2)
x11()
qplot(polaridades, 
      data=polaridades, 
      geom="histogram", 
      col=I("Black"),
      binwidth = 1,
      fill=factor(polaridades))+xlab("Score") + ylab("Frequência") + ggtitle("Polaridades por Citações - Orlando Coelho Aranda")

qplot(factor(Classificação), 
      data=polaridades, geom="bar",
      fill=factor(Classificação))+xlab("Categorias") + ylab("Frequência") + ggtitle("Polaridades por Citações - Orlando Coelho Aranda")




deps <- data.frame(citac$Group.1[which(citac$x >20)], docsnome[which(citac$x >20)])

polaridades

frases1 <- tokenize_sentences(deps[2])

#seleciona apenas frases que contem o Nome
frasesnome1 <- frases1[[1]][unique(c(grep(frases1[[1]], pattern = "orlando"), grep(frases1[[1]], pattern = "aranda")))]
frasesnome1 <- frasesnome1[-grep(frasesnome1, pattern = "orlando botelho")] #exclui frases sobre o SEGUNDO orlando
frasesnome1 <- removeWords(frasesnome1, words = tm::stopwords("portuguese"))
frasesnome1 <- removeWords(frasesnome1, words = c("acusado", "acusada", "juiz", "ministerio publico", "senhor"))
frasesnome1 <- removePunctuation(frasesnome1)
frasesnome1 <- str_replace_all(frasesnome1,"[^[:graph:]]", " ") 

sd <- cbind(frasesnome, polaridades)

write.csv2(sd, "tabelaBR.csv")
#########################################################Continuar aqui

#################################################################################################
library(XML)
html <- htmlParse("index.html")
tabelas <- readHTMLTable(html)
x <- tabelas[[1]]
View(x)
x <- x[-c(1:9), ]
x <- subset(x, V2 != "TELEFONE")
links <- getHTMLLinks(html)

links <- links[grep(links, pattern = ".wav")]
final <- cbind(x, links)

docsnomecit$dir.cname.completa <- rbind(docsnomecit$dir.cname.completa, final)

grep(x$V4, pattern = "28/07/2007 16:23:25Â")
links <- c(links[1:2651], "NAO DISPONIVEL", links[2652:2934])

write.csv2(docsnomecit$dir.cname.completa, "BD Chamadas.csv")
