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
library(tidytext)
library(lubridate)
library(zoo)
library(scales)
library(quanteda)

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

############ LIMPIEZA Y ORDEN     #################

##  LIMPIEZA
limpieza <- function(mananera) {
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  remover <- c("presidente","no","sí", "andrés", "lópez","méxico", "manuel", "obrador", "‘", "entonces", "dos", "tres", "‘n’")
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
  }

#############


wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



# debe de funcionar en dos pasos


############## nuevo análisis de sentimientos de la mañanera   #############
#descarga del lexico afinn
#download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
#              "lexico_afinn.en.es.csv")
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

# añadir fecha 
# Separar en tokens
#[ultima fecha]
AMLO <- mananera("2022/09/12") %>%  limpieza()
wordcloud(words = AMLO$word, freq = AMLO$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

head(AMLO)
length(unique(AMLO$word))
AMLO
sum(AMLO$freq)
temp<- mananera("2022/09/12")
length(temp[1])
head(temp)
library(stringr)
str_count(temp) # Para contar el número de palabras


mananera_afinn <- 
  AMLO %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = screen_name)  

afinn_mananera <- AMLO$word %>%
  tibble() %>% # esto nos lo convierte a df, que es el input para unnest_tokens
  #colnames() %>%
  unnest_tokens(input = ".", output = "Palabra") %>%
  inner_join(afinn, . , by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))

colnames(AMLO)[1] <- "Palabra"

AMLO_Sentimiento <- unique(inner_join(AMLO, afinn_mananera, by = "Palabra"))
AMLO_Sentimiento <- AMLO_Sentimiento %>% mutate(Total = Puntuacion * freq)

# Positivo
temp <- AMLO_Sentimiento %>% filter(Tipo == "Positiva") 
sum(temp$freq)

wordcloud(words = temp$Palabra, freq = temp$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Negativo
temp <- AMLO_Sentimiento %>% filter(Tipo == "Negativa") 
sum(temp$freq)




head(AMLO) # word
head(afinn_mananera) "Palabra"

# Separar en positivo y negativo



afinn_mananera %>% summarise(Puntuacion_mananera = mean(Puntuacion))
afinn_mananera %>% group_by(Tipo, Palabra) %>% count(Palabra, sort = T) %>% top_n(n = 10)


head(afinn_mananera)
mananera %>% 
  tokens() %>% 
  unnest_tokens()

unnest_tokens(as.table(mananera))

tuits_afinn <- 
  tuits %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = screen_name)

###########################################################
######      BAJAR Y GUARDAR MAÑANERAS POR UN MES    #######
###########################################################
library(lubridate)

yyyy/mm/dd

#vector con las direcciones de las estenográficas
#empezamos con agosto
Nombre_pagina <- "https://lopezobrador.org.mx/"
fecha <- "2022/07/31"
#trycatch

for (i in 1:200) {
  fecha <- ymd(fecha) 
  dir_pag <- paste(Nombre_pagina,
          (gsub(x = fecha, pattern = "-", replacement = "/")), sep = "")
  esteno <- read_html(dir_pag) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("version",., value = TRUE) %>%
    unique()
  write.csv(x = esteno, 
            file = file.path("Direcciones_Estenografica", paste("direcciones", fecha, sep = "_")))
  #temp <- temp %>% add_row(tibble_row(fecha = dir_pag,estenografica = esteno[1]))
  fecha <- as.Date(fecha) +1
}

file.path("Direcciones_Estenografica", paste("direcciones", fecha, sep = "_"))

temp <- read_html("https://lopezobrador.org.mx/2022/02/05") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("version",., value = TRUE) %>%
  unique()
unique(temp)
class(temp)

mananera <- function(fecha) {# el input es la fecha, aaaa/mm/dd
  temp <- paste("https://lopezobrador.org.mx/", fecha, sep = "") %>% 
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("version",., value = TRUE) 
  temp[1] %>% read_html() %>% 
    html_nodes("p") %>%
    html_text() %>%
    paste(collapse = " ") }

##############      Leer direcciones de mañanera    ######################



temp <-read.csv(file = "Direcciones_Estenografica/direcciones_2022-01-06")
temp[2][1,1]

length(list.files("Direcciones_Estenografica/")) #229


unir_text_files()

library(dplyr)

lapply(list.files("Direcciones_Estenografica/"),
       read.csv())


temp <- tibble("a")

temp2

grepl(pattern = "(\\d{3})", x = temp2$x)

for (i in 1:length(list.files("Direcciones_Estenografica/"))) {
  temp2 <-read.csv(paste("Direcciones_Estenografica/", 
                 list.files("Direcciones_Estenografica/"), sep = "")[i])
  temp <- bind_rows(as.tibble(temp2), temp) 
}

vector <- lapply(X = temp2, FUN = grep, pattern = "(\\-)([0-9]){3}(\\/)()")
length(temp2$x[vector$x])




class(vector$x)
class(as.vector(vector))

vector <- lapply(X = as.list(unique(temp$x)), FUN = str_detect, pattern = "*\\d{3}")

install.packages("htmlwidgets")
library(htmlwidgets)
quant <- function(rx) str_view_all(".a.aa.aaa", rx)

str_detect()