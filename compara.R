#comparação de despachos
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

library(tm)
library(readtext)
library(pdftools)
library(tokenizers)
library(stringr)
library(RWeka)
library(XML)

cname <- file.path("~", "DESPACHOS/")  
setwd(cname)
docs <- c()
for (i in dir(cname)){
  x <- readtext(i)
  print(i)
  docs <- rbind(docs, x)
}

save(docs, file = "despachos.RData")
docs[, 2] <- removePunctuation(docs[, 2])
docs[, 2] <- rm_accent(docs[,2])
docs[, 2] <- removeNumbers(docs[, 2])

corpus = Corpus(VectorSource(docs))
summary(corpus)
ndocs <- length(corpus)
# Ignora palavras muito raras
minTermFreq <- ndocs * 0.01
# ignora palavras muito comuns
maxTermFreq <- ndocs * 0.95

dtm = DocumentTermMatrix(corpus,
                         control = list( 
                           wordLengths=c(4, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           #stemming = T,
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))
#dtm <- dtm[, names(head(sort(colSums(as.matrix(dtm))), 400))]
#dtm <- dtm[, names(sort(colSums(as.matrix(dtm))))]
inspect(dtm)

dtm.matrix = as.matrix(dtm)

distMatrix <- dist(dtm.matrix, method="cosine")
head(distMatrix)
#hierarchical clustering method:
#Assign each document to its own (single member) cluster
#Find the pair of clusters that are closest to each other (dist) and merge them. So you now have one cluster less than before.
#Compute distances between the new cluster and each of the old clusters.
#Repeat steps 2 and 3 until you have a single cluster containing all documents.


##########
#DENDOGRAMA
x11()
groups <- hclust(distMatrix, method="ward.D")
plot(groups, cex=0.9, hang=-1,  main = "Dendograma - Distâncias entre despachos", sub = "" , xlab = "", ylab = "Distância")
rect.hclust(groups, k=4)
#############
load("~/DESPACHOS/despachos.RData")
grupo <- docs
grupo[, 2] <- stripWhitespace(grupo[, 2])

grupo[, 2] <- removeNumbers(grupo[, 2])
grupo[, 2] <- rm_accent(grupo[, 2])
grupo[, 2] <- tolower(grupo[, 2])
grupo[, 2] <- str_replace_all(grupo[, 2], pattern = ",", replacement = ".")
frases <-  tokenize_sentences(grupo[, 2])
frases <- str_split(frases, "[[:punct:]]")

for (i in c(1:24)){
  frases[[i]]<- removeNumbers( frases[[i]])
  frases[[i]] <- rm_accent( frases[[i]])
  frases[[i]] <- tolower( frases[[i]])
  frases[[i]] <- removePunctuation(frases[[i]])
  frases[[i]] <- str_replace_all(frases[[i]],"[^[:graph:]]", " ") 
  frases[[i]] <- str_replace_all(frases[[i]],"  ", " ") 
  frases[[i]] <- unique(frases[[i]])
  frases[[i]] <- str_replace_all(frases[[i]],pattern = "^\\s*", replacement = "") 
  frases[[i]] <- str_replace_all(frases[[i]],pattern = " $", replacement = "") 
  frases[[i]] <- frases[[i]][grep(frases[[i]], pattern = "\\s")]
  frases[[i]] <- frases[[i]][nchar(frases[[i]])>15]
}


dup <- list()
atual <- frases[[4]]
anterior <- frases[[17]]
for (i in c(1: length(atual))){
print(i)  
   dup<- c(dup, list(try(anterior[grep(pattern = paste("^",atual[i],"$", sep =""), anterior)])))
}

x <- dup[lapply(dup,length)>0]
save(dup, file = "duplicadas23.RData")

frases[[12]]

load("duplicadas11.RData")


      grep(frases[[13]], pattern = frases[[12]][89])
      
      

      dup <- list()
      atual <- frases[[13]]
      anterior <- frases[[12]]
      for (i in c(1: length(atual))){
        
        dup<- c(dup, list(try(grep(pattern = paste("^",atual[i],"$", sep =""), anterior))))
      }
      
      x <- dup[lapply(dup,length)>0]
      
      write.table(docs[1, 2], "teste.txt")
      