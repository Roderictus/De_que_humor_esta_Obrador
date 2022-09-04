# https://lopezobrador.org.mx/
# https://programminghistorian.org/es/lecciones/analisis-de-sentimientos-r
# https://steviep42.github.io/webscraping/book/index.html#quick-rvest-tutorial
#  https://resulumit.com/teaching/scrp_workshop.html#21  #Scraping workshop 


#######     LIBRERIA      ##########  
library(rvest)
library(tidyverse)
library(tokenizers)
library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)

######      DATOS         ##########

#https://presidente.gob.mx/
#https://lopezobrador.org.mx/2022/09/02/version-estenografica-de-la-conferencia-de-prensa-matutina-del-presidente-andres-manuel-lopez-obrador-806/
#https://lopezobrador.org.mx/2022/08/31/version-estenografica-de-la-conferencia-de-prensa-matutina-del-presidente-andres-manuel-lopez-obrador-805/
#https://lopezobrador.org.mx/2022/09/02/version-estenografica-de-la-conferencia-de-prensa-matutina-del-presidente-andres-manuel-lopez-obrador-806/

## El sitio ordena las conferencias por día
## Vamos a minar unos cuantos links 
## probemos con un día, presumiblemente hay cientos
## la página funciona al menos desde 2019

temp <- read_html("https://lopezobrador.org.mx/2022/09/02")

links <- temp %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("version",., value = TRUE) # nos da cinco veces el valor de la misma estenográfica

# Asumimos que sólo hay una por día y que es el primer valor, lo que tiene sentido considerando
# que la misma página se va a repetir varias veces

matutina_fecha <- read_html(links[1])

fecha <- matutina_fecha %>% html_nodes("p") %>% html_text()

######    PRIMER PROCESAMIENTO  #########

fecha %>% tokenize_sentences()
fecha %>% tokenize_words()
fecha %>% tokenize_paragraphs()

fecha_todo <- paste(fecha, collapse = "\n")

palabras <- tokenize_words(fecha_todo) # 14,105
tabla <- table(palabras[[1]])
tabla <- data_frame(word = names(tabla), count = as.numeric(tabla))
tabla <- arrange(tabla, desc(count))

tabla
######    ANÁLISIS DE SENTIMIENTOS    ##########



### DEL TUTORIAL

base_url <- "https://raw.githubusercontent.com/programminghistorian/jekyll/gh-pages/assets/basic-text-processing-in-r/"
url <- sprintf("%s/sotu_text/236.txt", base_url)
texto <- paste(readLines(url), collapse = "\n")


sprintf("%s/sotu_text/236.txt", "texto/")

temppres <- bow(url = presidencia29) %>% scrape()


