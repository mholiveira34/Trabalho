
write.csv2(tab, "base.csv")

chamadas <- read.delim("BASE.txt", stringsAsFactors = FALSE)

chamadas <- chamadas[chamadas$DURAÇÃO != "00:00:00", ]

chamadas$Datahora <- as.POSIXct(paste(chamadas$DATA.INICIAL, chamadas$HORA.INICIAL, sep = " "), format = "%d/%m/%Y %H:%M:%S")

chamadas$Datahorafinal <- as.POSIXct(paste(chamadas$DATA.FINAL, " ", chamadas$HORA.FINAL), format = "%d/%m/%Y %H:%M:%S")
head(chamadas)
#chamadas <- subset(chamadas, HORA.FINAL != "#VALOR!")
alvos <- unique(chamadas$TELEFONE)
sob <- data.frame()
i <- 1378203415
for ( i in alvos){
  print(i)
  cham <- subset(chamadas, TELEFONE == i)
  for (j in unique(cham$DATA.INICIAL)){
    dat <- subset(cham, DATA.INICIAL == j)
    dat <- dat[order(dat$Datahora), ]
    if(nrow(dat)>1){
    for (k in c(1:(nrow(dat)-1))){
      #vetor <- seq((dat$HORA.INICIAL[k]), as.integer(dat$HORA.FINAL[k]), by = "secs")
      if(dat$Datahora[k] < dat$Datahora[k+1] & dat$Datahora[k+1] < dat$Datahorafinal[k]){
        sob <- rbind(sob, dat[k, ], dat[k+1, ])
       
      }}
      sob <- unique(sob)
    }
  }
}

int <- c()
v <- c(1:nrow(sob))
for (i in c(1:(length(v)-1))){
inter <- intersect(seq(sob$Datahora[i], sob$Datahorafinal[i], by = "secs"), 
          seq(sob$Datahora[i+1], sob$Datahorafinal[i+1], by = "secs"))
int <- c(int, length(inter))
}
max(int)
i = 6808
mean(inter)
median(inter)
plot(int)
sob[which(int>1000)+1,]

sob[6218*2+1, ]
int <- int/60
inter <- int[int>0]
hist(inter, 10000, col = "tomato")
?hist
plot(inter)
i = "Adriano"
as.numeric(dat$HORA.INICIAL[k])+as.numeric(dat$DURAÇÃO[k])
j = "05/06/2007"
k = 46

as.integer(chamadas$HORA.FINAL[3])

as.POSIXct(paste(dat$DATA.INICIAL[1], " ", dat$HORA.FINAL[1]), format = "%d/%m/%Y %H:%M:%S")

write.csv2(sob, "sobrepostasPS.csv")
