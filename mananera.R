
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

