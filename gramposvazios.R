grampos <- read.delim("gramposvazios.csv", sep = ";", head = TRUE)


tab <- data.frame()
for (i in c(1:nrow(grampos))){
  datas <- seq(as.Date.character(grampos$INÍCIO[i], "%d/%m/%Y"), as.Date.character(grampos$FIM[i], "%d/%m/%Y"), by = "days")
  tab <- rbind(tab, data.frame(grampos$TERMINAL[i], grampos$ALVO[i], datas))
}
tab$grampo <- "vazio"

write.csv2(tab, "vazios.csv")
grampos[i, ]
head(grampos)
?as.POSIXct
Sys.timezone()
