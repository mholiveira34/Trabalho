library(RTextTools)
library(e1071)

library(xlsx)
library(tm)
library(stringr)
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

?read.xlsx
docs <- c()
for (i in dir()){
  x <- read.xlsx(i, 1, encoding = "UTF-8")
  print(i)
  docs <- rbind(docs, x[, 2:3])
}
save(docs, file = "Base.RData")
docs <- data.frame(apply(docs, 2, FUN =  tolower))
docs <- data.frame(apply(docs, 2, removePunctuation))
docs[, 1] <- gsub(docs[, 1], pattern= "ã", replacement = "")
docs <- docs[!is.na(docs[, 2]), ]
docs <- docs[docs[, 2]!= "v", ]
docs$NA..1 <- droplevels(docs$NA..1)
docs <- data.frame(apply(docs, 2, rm_accent))
docs[, 1] <- removeWords(as.vector( docs[, 1]), words = c("juiz", "advogado", "acusado", "acusada"))

matrix= create_matrix(docs[,1], language="pt", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 


# build the data to specify response variable, training set, testing set.
container = create_container(matrix, as.factor(docs[,2]),
                             trainSize=1:4000, testSize=4001:6083,virgin=FALSE)
model1 = train_model(container, "MAXENT")
results = classify_model(container, model1)
# accuracy table

table(as.factor(docs[4001:6083, 2]), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.factor(docs[4001:6083, 2]), results[,"MAXENTROPY_LABEL"])
# recall accuracy

model2 = train_model(container, "RF")
results = classify_model(container, model2)
recall_accuracy(as.factor(docs[4001:6083, 2]), results[,"BAGGING"])
recall_accuracy(as.numeric(as.factor(docs[4001:6083, 2])), results[,"TREE_LABEL"])

recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])


recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"SVM_LABEL"])

table(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
