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

temp <- read_html("https://lopezobrador.org.mx/2022/09/05")

links <- temp %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("version",., value = TRUE) # nos da cinco veces el valor de la misma estenográfica

# Asumimos que sólo hay una por día y que es el primer valor, lo que tiene sentido considerando
# que la misma página se va a repetir varias veces

matutina_fecha <- read_html(links[1])

fecha <- matutina_fecha %>% html_nodes("p") %>% html_text()

######    FUNCIÓN PARA OBTENER MAÑANERA #############

mananera <- function(fecha) {# el input es la fecha, aaaa/mm/dd
  temp <- paste("https://lopezobrador.org.mx/", fecha, sep = "") %>% 
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("version",., value = TRUE) 
  temp[1] %>% read_html() %>% 
    html_nodes("p") %>%
    html_text() %>%
    paste(collapse = " ")
    } # está función da las direcciones de las estenográficas web para la fecha introducida



mananera("2022/02/21")

temp <- read_html(paste("https://lopezobrador.org.mx/", "2022/02/21", sep = ""))

######    PRIMER PROCESAMIENTO  #########

fecha_todo <- paste(fecha, collapse = "\n")

palabras <- tokenize_words(fecha_todo) # 14,105
tabla <- table(palabras[[1]])
tabla <- data_frame(word = names(tabla), count = as.numeric(tabla))
tabla <- arrange(tabla, desc(count))

tabla
######    ANÁLISIS DE SENTIMIENTOS    ##########

# Palabras
texto_palabras <- get_tokens(fecha_todo)

sentimientos_df <- get_nrc_sentiment(texto_palabras, lang = "spanish")

head(sentimientos_df)

barplot(
  colSums(prop.table(sentimientos_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "¿De que humor está Obrador?",
  sub = "Mañanera del 5 de septiembre 2022",
  xlab="emociones", ylab = NULL)

palabras_tristeza <- texto_palabras[sentimientos_df$sadness> 0]

pal_trist_orden <- sort(table(unlist(palabras_tristeza)), decreasing = TRUE)
head(pal_trist_orden, n = 10)

length(pal_trist_orden)

nube_emociones_vector <- c(
  paste(texto_palabras[sentimientos_df$sadness> 0], collapse = " "),
  paste(texto_palabras[sentimientos_df$joy > 0], collapse = " "),
  paste(texto_palabras[sentimientos_df$anger > 0], collapse = " "),
  paste(texto_palabras[sentimientos_df$fear > 0], collapse = " "))

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))

nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)

colnames(nube_tdm) <- c('tristeza', 'felicidad', 'enfado', 'confianza')
head(nube_tdm)

comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = c("green", "red", "orange", "blue"),
                 title.size = 1, max.words = 100, scale = c(2.5, 1), rot.per = 0.4)
#####   Una nube normal de las palabras más utilizadas
#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

fecha_todo <- paste(fecha, collapse = "\ ")
docs <- Corpus(VectorSource(fecha_todo))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "‘")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
remover <- c("presidente", "andrés", "lópez","méxico", "manuel", "obrador", "‘", "entonces", "dos", "tres", "‘n’")
docs <- tm_map(docs, removeWords, remover) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(0905)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# Oraciones

