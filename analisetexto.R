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
  x <- readtext(i)
  print(i)
  docs <- c(docs, x[, 2])
}

summary(docs)
docst <- data.frame(dir(cname), docs)
#Tratamento - Não remover pontuação antes de separar frases
docst$docs <- tolower(docst$docs) #CAIXA BAIXA    
#docs <- removePunctuation(docs)
docst$docs <- rm_accent(docst$docs) #REMOVE ACENTOS
docst$docs <- removeNumbers(docst$docs) #REMOVE NUMEROS 
docst$docs <-  str_replace_all(docst$docs,"[^[:graph:]]", " ") #REMOVE CARACTERES GRÁFICOS
docst$dir.cname. <- toupper(docst$dir.cname.) #PASSA OS NOMES DOS DOCUMENTOS PARA CAIXA ALTA

nome <- "orlando" #nome do cliente

#encontra documentos que citam o Nome

docsnome <- docst[unique(c(grep(docst$docs, pattern = nome), grep(docst$docs, pattern = "aranda"))), ] #nome e sobrenome


cit <- c()
str(docsnome)
#conta o número de vezes em que o Nome é citado em cada depoimento.
for (i in docsnome$docs){
  termos <- tokenize_sentences(i)
  num <- length(unique(c(grep(termos[[1]], pattern = "orlando"), 
                         #grep(termos[[1]], pattern = "lovato"), 
                         grep(termos[[1]], pattern = "aranda")))) 
  num3 <- length(unique(grep(termos[[1]], pattern = "botelho") )) 
  print(num, num3)
  #num2 <- length(unique(grep(termos[[1]], pattern = "machado") ))
  #num3 <- length(unique(grep(termos[[1]], pattern = "orlando botelho") ))
  cit <- c(cit, num-num3)
}

cit

docsnomecit <- cbind(docsnome, cit)
docsnomecit <- docsnomecit[-which(cit < 1), ] #remove da lista documentos sem citações (zeros e valores negativos)

#trata os nomes dos documentos
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "^(.*)-", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = " $", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "^\\s*", replacement = "")
docsnomecit$dir.cname. <- removePunctuation(docsnomecit$dir.cname.)
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "VIDEODEAUDIENCIAREU", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "TESTEMUNHA", replacement = "")

docsnomecit$dir.cname. <- substr(docsnomecit$dir.cname.,1,nchar(docsnomecit$dir.cname.)-4)
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "III", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "TA  ", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "\\sI$", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "\\sII$", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "\\sIV$", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "\\sV$", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "\\sVI$", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "\\sVII$", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "\\s$", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "PARTE  E ", replacement = "")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "ACUSADO", replacement = "")

docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "SOUVA", replacement = "SOUZA")
docsnomecit$dir.cname. <- gsub(docsnomecit$dir.cname., pattern = "LUIZ ANTONIO DE SOUZA", replacement = "LUIZ ANTONIO")
docsnomecit$dir.cname. <- removeNumbers(docsnomecit$dir.cname.)
docsnomecit$dir.cname. <- rm_accent(docsnomecit$dir.cname.)

#cria tabela com citações por documento
citacoes <- data.frame(docsnomecit$cit, colnames <- docsnomecit$dir.cname. )

citac <- aggregate(citacoes[,1], list(citacoes[,2]), sum)
x <- citac$x[which(citac$x <=1)]
#agrupa citações únicas
citacadj <- rbind(citac[-which(citac$x <=1), ], data.frame(Group.1 = "OUTROS", x = sum(x)))

citacadj <- citacadj[-grep(citacadj$Group.1, pattern = "ANTONIO CARLOS LOVATO"), ]

#gráfico de frequência por depoimento
par(mar=c(20,4,2,2))
barplot(height = citacadj$x,
        horiz = FALSE, 
        names.arg = citacadj$Group.1,
        las = 2, 
        ylim = c(0, 100), 
        col = "skyblue", 
        main = "Citações por Depoimento - Antônio Carlos Lovato", 
        ylab = "número de citações")


par(mar=c(4,4,4,4))

library(igraph)
#rede de citações
links <- data.frame(citac$Group.1, "ANTONIO CARLOS LOVATO", citac$x)
links <- subset(links, citac.x != 0) 
net <- graph_from_data_frame(d=links, vertices=links$citac.Group.1, directed=T) 
net <- simplify(net, remove.loops = T)

deg <- degree(net, mode="all")
V(net)$size <- deg*3
V(net)$size <- links$citac.x
plot(net, edge.arrow.size=.05, edge.curved=0,
     vertex.color="skyblue", vertex.frame.color="#555555",
     vertex.label.color="black",
     vertex.label.cex=.7, 
     vertex.label = ifelse(links$citac.x >= 4, names(V(net)), NA) ,#layout=layout_with_lgl, 
     main = "Rede de Citações - Antônio Carlos Lovato", 
     sub = "Identificados depoimentos onde foi citado mais de 4 vezes") 



#separa os depoimentos que citam o Nome em frases e cria corpus     
docnovo <- c()
for (i in docsnomecit$docs){
docnovo <- paste(docnovo, i)}
frases <- tokenize_sentences(docnovo)

#seleciona apenas frases que contem o Nome

frasesnome <- frases[[1]][unique(c(grep(frases[[1]], pattern = "antonio carlos"),
                                   grep(frases[[1]], pattern = "lovato")))]

#frasesnome <- frasesnome[-grep(frasesnome, pattern = "orlando antonio")] #exclui frases sobre o SEGUNDO orlando
frasesnome <- removeWords(frasesnome, words = tm::stopwords("portuguese"))
frasesnome <- removeWords(frasesnome, words = c("acusado", "acusada", "juiz", "ministerio publico", "senhor"))
frasesnome <- removePunctuation(frasesnome)
frasesnome <- noquote(frasesnome)
frasesnome <- str_replace_all(frasesnome,"[^[:graph:]]", " ") 
frasesnome <- str_replace_all(frasesnome,'"', " ") 
gsub(as.character(frasesnome[175]), pattern =  "\"", replacement = ' ')
#núvem de palavras das frases que citam o Nome
library(wordcloud)

par(mar = c(10, 10, 10, 10))
wordcloud(frasesnome,
          max.words=200, random.order=FALSE,  
          colors=brewer.pal(6, "Dark2"))

corpus2 <- VCorpus(VectorSource(x = noquote(frasesnome)), #cada frase é um documento, todas citam o Nome
                   readerControl = list(language = "pt",
                                                                                load = TRUE))

mdoc <- TermDocumentMatrix(corpus2)
dtm <- DocumentTermMatrix(corpus2)
m <- as.matrix(mdoc)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#palavras e suas frequências (entre frases que citam o Nome)
par(mar=c(5,8,2,2))
barplot(height = d$freq[1:30], names.arg = d$word[1:30], las = 1, horiz = TRUE)


mft <- findFreqTerms(dtm, lowfreq = 100)
#palavras associadas ao Nome
assocs <- findAssocs(dtm, "lovato", corlimit = 0.2)
assocs <- data.frame(palavra = names(assocs[["lovato"]]), value = assocs[["lovato"]], row.names=NULL)
noquote(assocs)
#gráfico de palavras associadas e seu nível de associação
barplot(height = assocs$value[1:25], 
        names.arg = assocs$palavra[1:25], 
        horiz = TRUE, 
        las = 1, 
        xlim = c(0:1),
        col = heat.colors(26, alpha=0.7), main = "Palavras Associadas a José Luiz Favoreto Pereira", 
        xlab = "Nível de Associação")
#núvem de palavras associadas e nível de associação
x11()
wordcloud(words = assocs$palavra, freq = assocs$value*100,
          random.order=FALSE,  
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



library(ggplot2)
qplot(polaridades, 
      data=polaridades, 
      geom="histogram", 
      col=I("Black"),
      binwidth = 1,
      fill=factor(polaridades))+xlab("Score") + ylab("Frequência") + ggtitle("Polaridades por Citações - José luiz Favoreto Pereira")

qplot(factor(Classificação), 
      data=polaridades, geom="bar",
      fill=factor(Classificação))+xlab("Categorias") + ylab("Frequência") + ggtitle("Polaridades por Citações - José Luiz Favoreto Pereira")




deps <- data.frame(citac$Group.1[which(citac$x >20)], docsnome[which(citac$x >20)])



frases1 <- tokenize_sentences(deps[2])

#seleciona apenas frases que contem o Nome
frasesnome1 <- frases1[[1]][unique(c(grep(frases1[[1]], pattern = "orlando"), grep(frases1[[1]], pattern = "aranda")))]
frasesnome1 <- frasesnome1[-grep(frasesnome1, pattern = "orlando botelho")] #exclui frases sobre o SEGUNDO orlando
frasesnome1 <- removeWords(frasesnome1, words = tm::stopwords("portuguese"))
frasesnome1 <- removeWords(frasesnome1, words = c("acusado", "acusada", "juiz", "ministerio publico", "senhor"))
frasesnome1 <- removePunctuation(frasesnome1)
frasesnome1 <- str_replace_all(frasesnome1,"[^[:graph:]]", " ") 



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
