

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
  d <- data.frame(word = names(v), freq = v)
  #set.seed(123)
}