
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
rsDriver()
rD <- rsDriver(browser = "chrome")
remDr <- rD[["client"]]
remDr$navigate("https://login.folha.com.br/login?done=http%3A%2F%2Ffolhainvest.folha.uol.com.br%2Fcarteira&service=folhainvest")


remDr <- rsDriver()
remDr$open()

remDr$navigate("http://www.stj.jus.br/SCON/")


?RSelenium
