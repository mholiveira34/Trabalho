tabela <- read.delim("Check_Grampo.txt", head = TRUE, sep = "\t", stringsAsFactors = FALSE)
str(tabela$Início)
chamadas <- read.delim("naoencontradasOB.txt", head = TRUE, sep = "\t", stringsAsFactors = FALSE)
names(chamadas)
library(tm)
chamadas$Terminal.2 <- removePunctuation(chamadas$Terminal.2)
head(chamadas)
#chamadas$Terminal. <- gsub("^55", x = chamadas$Terminal.do.Alvo, replacement = "")

tabela$Início <- as.Date.character(tabela$Início, "%d/%m/%Y")
tabela$Fim <- as.Date.character(tabela$Fim, "%d/%m/%Y")
chamadas$Data <- as.Date.character(chamadas$Data, "%d/%m/%Y")
head(tabela)
telefones <- unique(chamadas$Terminal.2)
y <- data.frame()
for (i in telefones){
  lig <- subset(chamadas, Terminal.2 == i)
  check <-  subset(tabela, Telefone.2 == i)
  check <- subset(check, RO == 1)
  vetor <- c()
  for (j in as.character(check$Início[!is.na(check$Início)])){
    vetor <- c(vetor, seq(as.Date(j), as.Date(subset(check$Fim, check$Início == j), "%d/%m/%Y")[1], by = "days"))
    }
  xx <- cbind(lig, as.numeric(lig$Data) %in% vetor)
  y <- rbind(y, xx)             
  }
foradoprazo <- data.frame()
falsos <- subset(y, y$`as.numeric(lig$Data) %in% vetor` == FALSE)
checknova <- subset(tabela, RO==1)


for (i in c(1:nrow(falsos))){
    fone <- falsos$Terminal.2[i]
    if(fone %in% checknova$Telefone.2){
    grampos <- subset(checknova, Telefone.2 == fone)
    #grampos <- subset(grampos, RO == 1)
    d6dia <- grampos$Fim +1
    
    if(falsos$Data[i] %in% d6dia){foradoprazo <- rbind(foradoprazo, falsos[i, ])}
    
    print(i)
 }}
tabela$RO
fone <- 51999847168

write.csv2(y, "chamadasOK.csv")
write.csv2(foradoprazo, "foradoprazoOK.csv")
summary(y$`as.numeric(lig$Data) %in% vetor`)
