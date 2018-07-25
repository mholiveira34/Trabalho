#compila dashboard

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
library(rmarkdown)

rmarkdown::render("D:/User/Documents/CodigosR/DASHBOARDnoticias.Rmd", encoding = "UTF-8")

#enviar email

library(gmailr)
mime() %>%
  to(c("mariaholiveira34@gmail.com" 
       , "maria.oliveira@ichase.com.br", "antonio@ichase.com.br", "antonio118@icloud.com"
       )) %>%
  from("mariaholiveira34@gmail.com") %>%
  text_body(" ") -> text_msg

strwrap(as.character(text_msg))

text_msg %>%
  subject("Dashboard - Noticias") %>%
  attach_file("D:/User/Documents/CodigosR/DASHBOARDnoticias.html") -> file_attachment


send_message(file_attachment)

