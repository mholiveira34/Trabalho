library(XML)
docs <- list()
for (i in dir()){
  x <- htmlParse(i, encoding = "UTF-8")
  print(i)
  docs <- c(docs, x)
}



tab <- data.frame()

for (i in c(1:length(docs))){
tabelas <- readHTMLTable(docs[[i]])
for (k in tabelas){
  if(ncol(k)==9 & names(k)[6]=="DURAÇÃO"){
    tab <- rbind(tab, k)
  }
}
}

links <- c()
for (i in c(1:length(docs))){
links1 <- getHTMLLinks(docs[[i]])
links <- c(links, links1[grep(links1, pattern = ".html")])

}
for (i in c(1:length(links))){
  links[i] <- str_split(str_split(links[i], "_")[[1]][4], "\\.")[[1]][1]
}
links

tab <- cbind(tab, links)

wavs <- c()
for (i in c(1:length(docs))){
  wavs1 <- getHTMLLinks(docs[[i]])
  wavs <- c(wavs, wavs1[grep(wavs1, pattern = ".wav")])
  
}
for (i in c(1:length(wavs))){
  wavs[i] <- str_split(str_split(wavs[i], "_")[[1]][4], "\\.")[[1]][1]
}



tab$audio <- ifelse(tab$links %in% wavs, "ok", "sem audio")
links2

save(tab, file = "base.RData")
write.csv(tab, "base.csv")

data <- str_split(tab$`DATA/HORA INICIAL`, " ")
for (i in c(1:nrow(tab))){
  tab$hora[i] <- data[[i]][2]
}
