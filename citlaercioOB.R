autos <-  readtext("Autos combinados.pdf")
par <- tokenize_paragraphs()
frases <- tokenize_sentences(autos[, 2], lowercase = TRUE)
save(frases, file = "autosob.RData")
frases1 <- rm_accent(frases[[1]][1:47072])
frases2 <- rm_accent(frases[[1]][47073:94144])
frases3 <- rm_accent(frases[[1]][94145:141216])
frases4 <- rm_accent(frases[[1]][141217:188287])

frases1l <- frases1[grep(frases1, pattern = "laercio")]
frases2l <- frases2[grep(frases2, pattern = "laercio")]
frases3l <- frases3[grep(frases3, pattern = "laercio")]
frases4l <- frases4[grep(frases4, pattern = "laercio")]

fraseslaercio <- c(frases1l, frases2l, frases3l, frases4l)
save(fraseslaercio, file = "autosoblaercio.RData")
library(readtext)
library(tm)
library(SnowballC)
library(topicmodels)
library(wordcloud)
library(plyr)
library(lattice)
library(tokenizers)
prepanel.pareto <- function(x, y, ...) {
  yy <- y[, drop = TRUE]
  list(ylim = as.character(yy),
       yat = 1:nlevels(yy))
}

panel.pareto <- function(x, y, ...) {
  yy <- y[, drop = TRUE]
  panel.barchart(x, yy[order(yy)], ...)
}

cps <- VCorpus(VectorSource(fraseslaercio),
               readerControl = list(language = "pt"))
cps2 <- tm_map(cps, FUN = content_transformer(tolower))
cps2 <- tm_map(cps2, FUN = removePunctuation)
cps2 <- tm_map(cps2, FUN = removeNumbers)
cps2 <- tm_map(cps2, FUN = removeWords, words = stopwords("portuguese"))
cps2 <- tm_map(cps2, FUN = removeWords, words = tolower(rm_accent(read.table("nomeslaercio.txt", encoding = "UTF-8")[, 1])))
cps2 <- tm_map(cps2, FUN = stripWhitespace)
dtm <- DocumentTermMatrix(cps2)
# Termos de maior intensidade.
frq <- slam::colapply_simple_triplet_matrix(dtm, FUN = sum)
frq <- sort(frq, decreasing = TRUE)

# Gráfico de pareto.
barchart(head(frq, n = 60), xlim = c(0, NA))

lda <- LDA(x = dtm, k = 3)
terms(lda)
table(topics(lda))

words <- as.data.frame(t(lda@beta))
names(words) <- terms(lda)
words <- cbind(term = lda@terms, words, stringsAsFactors = FALSE)
str(words)
k <- 30
tops <- lapply(words[, terms(lda)],
               FUN = function(x) {
                 o <- head(order(x, decreasing = TRUE), n = k)
                 data.frame(term = words$term[o],
                            lprob = x[o])
               })
tops <- ldply(tops, .id = "topic")
str(tops)

barchart(term ~ lprob | topic,
         data = tops,
         ylab = "Termos mais frequentes em cada tópico",
         xlab = "log da probabilidade de cada termo no tópico",
         scales = "free",
         layout = c(NA, 1),
         prepanel = prepanel.pareto,
         panel = panel.pareto)
x11()
par(mfrow = c(1, length(terms(lda))))
sapply(terms(lda),
       FUN = function(t) {
         wordcloud(words = words$term,
                   freq = 4000 * exp(words[, t]),
                   max.words = 200,
                   random.order = FALSE,

                   colors = tail(brewer.pal(7, "Purples"), 3))
         title(main = t, line = -4)
       })


fraseslaercio <- removeNumbers((removePunctuation(fraseslaercio)))
fraseslaercio <- removeWords(fraseslaercio, words = tm::stopwords("pt"))
wordcloud(fraseslaercio,
          max.words=200, random.order=FALSE,  
          colors=brewer.pal(6, "Dark2"))

fraseslaerciosn <- removeWords(fraseslaercio, words = tolower(rm_accent(read.table("nomeslaercio.txt", encoding = "UTF-8")[, 1])))
wordcloud(fraseslaerciosn,
          max.words=200, random.order=FALSE,  
          colors=brewer.pal(6, "Dark2"))

corpus2 <- VCorpus(VectorSource(x = noquote(fraseslaerciosn)), #cada frase é um documento, todas citam o Nome
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
assocs <- findAssocs(dtm, "laercio", corlimit = 0.2)
assocs <- data.frame(palavra = names(assocs[["laercio"]]), value = assocs[["laercio"]], row.names=NULL)
barplot(height = assocs$value[1:25], 
        names.arg = assocs$palavra[1:25], 
        horiz = TRUE, 
        las = 1, 
        xlim = c(0:1),
        col = heat.colors(26, alpha=0.7), main = "Palavras Associadas a José Luiz Favoreto Pereira", 
        xlab = "Nível de Associação")


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