
chamadas <- read.delim("BASE.txt", stringsAsFactors = FALSE, sep = "\t")

sob <- read.delim("sobrepostasPS.csv", sep = ";")
chamadas <- chamadas[chamadas$DURAÇÃO != "00:00:00", ]
head(chamadas)
chamadas$Datahora <- as.POSIXct(paste(chamadas$DATA.INICIAL, chamadas$HORA.INICIAL, sep = " "), format = "%d/%m/%Y %H:%M:%S")

chamadas$Datahorafinal <- as.POSIXct(paste(chamadas$DATA.FINAL, " ", chamadas$HORA.FINAL), format = "%d/%m/%Y %H:%M:%S")

chamadas <- chamadas[!chamadas$id %in% sob$id, ]
telefones <- unique(chamadas$TELEFONE)

chamadas <- chamadas[!duplicated(chamadas[,c(1, 3, 4)]), ]
i <- telefones[2]
TAB <- data.frame()
for (i in telefones){
  tab <- subset(chamadas, TELEFONE == i)
  tab <- tab[order(tab$Datahora), ]
  vetor <- c()
  if(nrow(tab)>1){
  for (k in c(1:(nrow(tab)-1))){
    segundos <-  length(seq(tab$Datahorafinal[k], tab$Datahora[k+1], by = "secs"))
    if(segundos<=3){
      TAB <- rbind(TAB, tab[k, ], tab[k+1, ])
      TAB <- unique(TAB)
    }}
  }
}

write.csv2(sob, "sobrepostasPS.csv")
write.csv2(TAB, "consecutivas.csv")
