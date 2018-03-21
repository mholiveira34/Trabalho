tabela <- read.delim("Check_Grampo.txt", head = TRUE, sep = "\t", stringsAsFactors = FALSE)
str(tabela$Início)
chamadas <- read.delim("BD - Chamadas.txt", head = TRUE, sep = "\t", stringsAsFactors = FALSE)
head(chamadas)
library(tm)
chamadas$Terminal.2 <- removePunctuation(chamadas$Terminal.2)

#chamadas$Terminal. <- gsub("^55", x = chamadas$Terminal.do.Alvo, replacement = "")

tabela$Início <- as.Date.character(tabela$Início, "%d/%m/%Y")
tabela$Fim <- as.Date.character(tabela$Fim, "%d/%m/%Y")
chamadas$Data <- as.Date.character(chamadas$Data, "%d/%m/%Y")

telefones <- unique(chamadas$Terminal.2)
y <- data.frame()
for (i in telefones){
  lig <- subset(chamadas, Terminal.2 == i)
  check <-  subset(tabela, Telefone.2 == i)
  vetor <- c()
  for (j in as.character(check$Início[!is.na(check$Início)])){
    vetor <- c(vetor, seq(as.Date(j), as.Date(subset(check$Fim, check$Início == j), "%d/%m/%Y")[1], by = "days"))
    }
  xx <- cbind(lig, as.numeric(lig$Data) %in% vetor)
  y <- rbind(y, xx)             
  }
foradoprazo <- data.frame()
for (i in c(1:nrow(y))){
  if (y$`as.numeric(lig$Data) %in% vetor`[i] == FALSE){
    fone <- y$Terminal.2[i]
    grampos <- subset(tabela, Telefone.2 == fone)
    d6dia <- grampos$Fim +1
    if(y$Data[i] %in% d6dia){foradoprazo <- rbind(foradoprazo, y[i, ])}
  }
}


write.csv2(y, "chamadas2.csv")
write.csv2(foradoprazo, "foradoprazo.csv")
summary(y$`as.numeric(lig$Data) %in% vetor`)
