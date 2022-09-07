# https://lopezobrador.org.mx/
# https://programminghistorian.org/es/lecciones/analisis-de-sentimientos-r
# https://steviep42.github.io/webscraping/book/index.html#quick-rvest-tutorial
#  https://resulumit.com/teaching/scrp_workshop.html#21  #Scraping workshop 
# https://presidente.gob.mx/sala-de-prensa/
# https://rpubs.com/jboscomendoza/analisis_sentimientos_lexico_afinn

#######     LIBRERIA      ##########  
library(rvest)
library(tidyverse)
library(tokenizers)
library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)
#library("SnowballC")
######      DATOS         ##########

#https://presidente.gob.mx/
#https://lopezobrador.org.mx/2022/09/02/version-estenografica-de-la-conferencia-de-prensa-matutina-del-presidente-andres-manuel-lopez-obrador-806/
#https://lopezobrador.org.mx/2022/08/31/version-estenografica-de-la-conferencia-de-prensa-matutina-del-presidente-andres-manuel-lopez-obrador-805/
#https://lopezobrador.org.mx/2022/09/02/version-estenografica-de-la-conferencia-de-prensa-matutina-del-presidente-andres-manuel-lopez-obrador-806/

## El sitio ordena las conferencias por día

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
    } # está función da la direccion

############ FUNCIÓN PARA NUBE DE FREQUENCIA     #################
##  LIMPIEZA
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
remover <- c("presidente", "andrés", "lópez","méxico", "manuel", "obrador", "‘", "entonces", "dos", "tres", "‘n’")

nube_frequencia <- function(mananera) {
  v <- Corpus(VectorSource(mananera)) %>%
    tm_map(toSpace, "/") %>%
    tm_map(toSpace, "@") %>%
    tm_map(toSpace, "\\|") %>% 
    tm_map(toSpace, "‘") %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeNumbers) %>% 
    tm_map(removeWords, stopwords("spanish")) %>%
    tm_map(removeWords, remover) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    TermDocumentMatrix() %>%
    as.matrix() %>%
    rowSums() %>%
    sort(decreasing = TRUE)
  d<-data.frame(word = names(v), freq = v)
  #set.seed(123)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}
#############
# debe de funcionar en dos pasos
temp <- read_html("https://lopezobrador.org.mx/2022/09/05")

mananera <- mananera("2022/09/07")
nube_frequencia(mananera)
