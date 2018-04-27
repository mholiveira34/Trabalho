
pagina <- htmlParse("http://www.stf.jus.br/portal/jurisprudencia/listarJurisprudencia.asp?s1=%28DELA%C7%C3O+E+PREMIADA%29&base=baseAcordaos&url=http://tinyurl.com/j8vp9no")

summary(pagina)
texto <- xpathSApply(pagina, '//div[@class="processosJurisprudenciaAcordaos"]')

##DELAÇÃO PREMIADA - STF

texto <- list()
for (i in c(1:5)){
  pagina <- htmlParse(paste0("http://www.stf.jus.br/portal/jurisprudencia/listarJurisprudencia.asp?s1=%28DELA%C7%C3O+E+PREMIADA%29&pagina=", 
                             i,
                             "&base=baseAcordaos&url=http://tinyurl.com/j8vp9no"), encoding = "UTF-8")
texto <- c(texto, xpathSApply(pagina, '//div[@class="processosJurisprudenciaAcordaos"]', saveXML) )
  }


write(unlist(texto), file = "stfDP.html")

###COLABORAÇÃO PREMIADA - STF
http://www.stf.jus.br/portal/jurisprudencia/listarJurisprudencia.asp?s1=%28COLABORA%C7%C3O+E+PREMIADA%29&pagina=2&base=baseAcordaos&url=http://tinyurl.com/ze892qu
texto <- list()
for (i in c(1:7)){
  pagina <- htmlParse(paste0("http://www.stf.jus.br/portal/jurisprudencia/listarJurisprudencia.asp?s1=%28COLABORA%C7%C3O+E+PREMIADA%29&pagina=", 
                             i,
                             "&base=baseAcordaos&url=http://tinyurl.com/ze892qu"), encoding = "UTF-8")
  texto <- c(texto, xpathSApply(pagina, '//div[@class="processosJurisprudenciaAcordaos"]', saveXML) )
}
sample(c(1:28540), 10)

write(unlist(texto), file = "stfCP.html")



###COLABORAÇÃO PREMIADA - STJ FAZER SELENIUM GRR

library(RSelenium)
library(XML)
rsDriver()
rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]


remDr$navigate("http://www.stj.jus.br/SCON/")

box <- remDr$findElement(using = 'xpath', "//input[@id = 'pesquisaLivre']")
box$sendKeysToElement(list("delação e premiada", key = "enter"))
acordaos <- remDr$findElement(using = "xpath", "/html/body/div/div[6]/div/div/div[3]/div[2]/div/div/div/div[3]/div[3]/span[2]/a")

acordaos$clickElement()

texto1 <- c()
for (i in c(1:20)){
texto <- c()
processo <- remDr$findElements(using = "xpath", "//div[@class = 'paragrafoBRS']")
for (i in c(1:length(processo))){
  texto <- c(texto, as.character(processo[[i]]$getElementText()))
}
texto1 <- c(texto1, texto)
prox <- remDr$findElement(using = "xpath", "//a[@class = 'iconeProximaPagina']")
prox$clickElement()
}
#resumopesquisa > div:nth-child(3) > span:nth-child(2)
?RSelenium
write(unlist(a), "teste.html")
texto1 <- data.frame(texto1)

texto1 <- data.frame(texto1)
write.csv2(texto1, "stjDP.csv")
