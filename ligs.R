
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

nomes <- read.table("nomes.csv", sep = ",")
nomes <- data.frame(rm_accent(nomes$V1))
nomes <- str_replace_all(nomes$rm_accent.nomes.V1., "\\([^)]*\\)", "")
nomes <- tolower(nomes)
nomes <- removeWords(nomes, "advogado")
nomes <- unique(nomes)
tab <- c()
for (i in c(1:188287)){
  print(i)
  for(j in nomes){
  if(length(grep(j, frases[[1]][i])) > 0){
    tab <- c(tab, frases[[1]][i])
  }
}
}
save(tab, file = "nomesob.RData")
tab <- unique(tab)
lig = data.frame()

for (i in c(1:length(tab))){
  print(i)
  for (j in nomes){
    if (str_detect(tab[i], j) == TRUE){
      lig <- rbind(lig, data.frame(tab[i], j, 0))}}}
save(lig, file = "ESTAMERDA.RData")


colnames(lig) <- c("frase", "nome", "0")
tabela <- aggregate(lig$nome, list(lig$frase), paste, collapse = ",")
?aggregate

tabela <- tabela[grep(tabela$x, pattern = ","), ]
library(splitstackshape)
?splitstackshape
?Stacked
write.csv2(tabela$x, file = "ligs.csv")
save(ligs, file = "conecções.RData")
?data.frame()
library(igraph)
 ligs <- data.frame(1,1)
 colnames(ligs) <- c("nome1", "nome2")
for (i in c(1:nrow(tabela))){
z <- str_split(tabela$x[i], pattern = ",")
  z <- as.vector(unlist(z))
 print(i)
  if(length(z) > 2){
    
    for (j in c(2: length(z))){
      ligs <- rbind(ligs, data.frame(nome1 = z[1], nome2 = z[j]))
    } 
  }else{ligs <- rbind(ligs, z)}
}
 
 ligs <- ligs[-1, ]
 
 library(igraph)
 #rede de citações
 links <- data.frame(unique())
 links <- subset(links, citac.x != 0) 
 net <- graph_from_data_frame(d=links, vertices=links$citac.Group.1, directed=T) 
 net <- simplify(net, remove.loops = T)
 X11()
 deg <- degree(net, mode="all")
 V(net)$size <- deg*3
 V(net)$size <- sqrt(links$citac.x)*4
 plot(net, edge.arrow.size=.05, edge.curved=0,
      vertex.color="skyblue", vertex.frame.color="#999999",
      vertex.label.color="#000000",
      vertex.label.cex=.7, 
      vertex.label = ifelse(links$citac.x >= 4, names(V(net)), NA) ,#,layout=layout_with_lgl, 
      main = "Rede de Citações - Antônio Carlos Lovato", 
      sub = "Identificados depoimentos onde foi citado 4 ou mais vezes") 
 