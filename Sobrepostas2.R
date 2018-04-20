chamadas <- read.delim("BD - Chamadas.txt", stringsAsFactors = FALSE)
#chamadas$Datahora <- as.POSIXct(paste(chamadas$DATA.INICIAL, " ", chamadas$HORA.INICIAL), format = "%d/%m/%Y %H:%M:%S")
chamadas$Data.Início <- as.POSIXct(chamadas$Data.Início, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
chamadas$Data.Fim <- as.POSIXct(chamadas$Data.Fim, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
#chamadas$Datahorafinal <- as.POSIXct(paste(chamadas$DATA.INICIAL, " ", chamadas$HORA.FINAL), format = "%d/%m/%Y %H:%M:%S")
#chamadas <- subset(chamadas, HORA.FINAL != "#VALOR!")




alvos <- unique(chamadas$Terminal.2)

sob <- data.frame()


for ( i in alvos){
  print(i)
  cham <- subset(chamadas, Terminal.2 == i)
  for (j in unique(cham$Data)){
    dat <- subset(cham, Data == j)
    if(nrow(dat)>1){
    for (k in c(1:(nrow(dat)-1))){
      #vetor <- seq((dat$HORA.INICIAL[k]), as.integer(dat$HORA.FINAL[k]), by = "secs")
      if(dat$Data.Início[k] < dat$Data.Início[k+1] & dat$Data.Início[k+1] < dat$Data.Fim[k]){
        sob <- rbind(sob, dat[k, ], dat[k+1, ])
       
      }}
      sob <- unique(sob)
    }
  }
}
save(sob, file = "sobrepostasob.RData")
int <- c()
v <- c(1:nrow(sob))
for (i in c(1:(length(v)-1))){
inter <- intersect(seq(sob$Data.Início[i], sob$Data.Fim[i], by = "secs"), 
          seq(sob$Data.Início[i+1], sob$Data.Fim[i+1], by = "secs"))
int <- c(int, length(inter))
}
sob <- cbind(sob, int)
max(int)
int <- int/60
inter <- int[int>0]
mean(inter)
median(inter)

plot(inter)
hist(inter, 1000, xlim = c(0, 10), col = "tomato")
?hist
plot(inter)
i = "Adriano"
as.numeric(dat$HORA.INICIAL[k])+as.numeric(dat$DURAÇÃO[k])
j = "05/06/2007"
k = 46

as.integer(chamadas$HORA.FINAL[3])

as.POSIXct(paste(dat$DATA.INICIAL[1], " ", dat$HORA.FINAL[1]), format = "%d/%m/%Y %H:%M:%S")

write.csv2(sob, "sobrepostasRA.csv")



##

madrugadas <- data.frame()
for (i in alvos){
  chamadasalvo <- subset(chamadas, Terminal.2 == i)
  for (k in unique(chamadasalvo$Data)){
    tab <- subset(chamadasalvo, Data == k)
    inf <- as.POSIXct(paste(k, "00:00:01", collapse = " "), format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
    sup <- as.POSIXct(paste(k, "06:00:00", collapse = " "), format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
    for (j in c(1:nrow(tab))){
      if (tab$Data.Início[j]> inf & tab$Data.Fim[j] < sup){
        madrugadas <- rbind(madrugadas, tab[j, ])
      }
      
    }
  }
}

##
for (i in c(1:nrow(madrugadas))){
  seg <- length(seq(madrugadas$Data.Início[i], madrugadas$Data.Fim[i], by = "secs"))
  madrugadas$segs[i] <- seg
}
hist(madrugadas$segs/60, 1000)
plot(madrugadas$segs/60)

soma <- c()
for (i in unique(madrugadas$Terminal.2)){
  soma <- c(soma, sum(subset(madrugadas, Terminal.2 == i)$segs))
  
}
tab <- data.frame(cbind(unique(madrugadas$Terminal.2), soma))

write.csv2(madrugadas, "MchamadasOB.csv")
