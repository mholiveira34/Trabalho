install.packages('readtext')
install.packages('stringr')
install.packages('tm')
install.packages('tokenizers')
require(tokenizers)
require(tm)
require(readtext)
require(stringr)

docs <- data.frame()
for (i in dir()){
  docs <- rbind(docs, readtext(i))
}

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

docs$text <- rm_accent(docs$text)
docs$text <- tolower(docs$text)

docsnome <- docs[unique(c(grep(docs$text, pattern = "jorge"), 
                           grep(docs$text, pattern = "aun"))), ] #nome e sobrenome

linhas <- tokenize_paragraphs(docsnome, paragraph_break = '\n')
tab <- data.frame()
for (i in 1:length(linhas)){ 
x <- unique(c(grep(linhas[[i]], pattern = "jorge"),
                                   grep(linhas[[i]], pattern = "aun")))
x <- unique(c(x-1, x, x+1))
y <- x[order(x)]
y <- y[y!=0]
tab <- rbind(tab, data.frame(rep(names(linhas)[i], length(y)), y, linhas[[i]][y]))

}
names(tab) <- c("documento", "parágrafos", "texto")
tab$documento <- rm_accent(tab$documento)
write.csv2(tab, "jorge.csv")
i=3




