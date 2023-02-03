# author: César Quezada
# date: Junio, 2021

# clean the workspace
rm(list = ls())
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

# Incrementar el espacio en memoria
# memory.limit()
# memory.size()
# memory.limit()
# Sys.info()
# object.size()

# R.version

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Cargamos las librerías  que vamos a necesitar
library(readxl) #  Para leer ficheros excel
library(tidyverse) #  Para las operaciones con datos (incluye dplyr y otras muy útiles)
library(syuzhet) #  Libreria para emociones
library(tidytext)
library(stringr) #  Para operar con datos de tipo String
library(stopwords) #  Para poder quitar las stopwords del texto
library(ggplot2) #  Librería de visualización gráfica
library(lubridate) #  Para el formateo de fechas y su visualización
library(scales) # Para trabajar con datos de coma
library(igraph) # Para el análisis de bigramas en formato de grafo
library(ggraph) # Librería para visualizar grafos
library(quanteda) # Para text mining más avanzado
library(topicmodels) #  Para hacer el clúster (Topic Modeling) de las palabras
library(cvTools) #  Para el cálculo del número de topics óptimo
library(reshape) #  Para la manipulación de datos
library(grid) # Para la representación en celdas de los datos
library(udpipe) # Para el análisis de sentimiento
library(syuzhet) #  Librería para emociones 
library(textdata) 
library(tm) 
library(tidyverse)
library(wordcloud)
library(tidyr)
library(widyr)
# devtools::install_github("ankitrohatgi/digitizeR")
# library('digitizeR')
library("tidyverse")
library("RColorBrewer")

# Carga de datos
# --------------
# --------------
palta_esp <- read.csv("productos/palta_esp.csv",encoding = "Latin1",sep = ";")
#palta_fr <- read.csv("productos/palta_fr.csv",encoding = "Latin1",sep = ";")
palta_uk <- read.csv("productos/palta_uk.csv",encoding = "Latin1",sep = ";")

names(palta_esp)

# Creando variables plaza y  producto
palta_esp$plaza <- "Espana"
#palta_fr$plaza <- "Francia"
palta_uk$plaza <- "Reino_Unido"
#palta <- rbind(palta_esp,palta_fr,palta_uk)
palta <- rbind(palta_esp,palta_uk)

rm(palta_esp,palta_fr,palta_uk)
palta$producto <- "palta"

names(palta) <- c("X","url","sentimiento","autor","texto","hashtags","impacto",
                  "impresiones","localizacion","fecha","hora","plaza","producto")


# (1)  LIMPIEZA DE DATOS
# ### #######################################

table(palta$plaza)

datos <- palta %>% 
  filter(plaza == "Espana")
table(datos$plaza)

# Vamos a hacer un poco de limpieza de texto
# ------------------------------------------
# Expresiones regulares
# ---------------------
datos$texto<-gsub("@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+", "", datos$texto)
datos$texto <- gsub("RT", "", datos$texto)
datos$texto <- gsub("https://", "", datos$texto)
datos$texto <- gsub("http://", "", datos$texto)

datos$texto <- tolower(datos$texto) # Se convierte todo el texto a min?sculas
datos$texto <- str_replace_all(datos$texto,"http\\S*", "") # Eliminaci?n de p?ginas web (palabras que empiezan por "http." seguidas de cualquier cosa que no sea un espacio)
datos$texto <- str_replace_all(datos$texto,"[[:punct:]]", " ") # Eliminaci?n de signos de puntuaci?n
datos$texto <- str_replace_all(datos$texto,"[[:digit:]]", " ") # Eliminaci?n de n?meros
datos$texto <- str_replace_all(datos$texto,"[\\s]+", " ") # Eliminaci?n de espacios en blanco m?ltiples
datos$texto <- chartr('áéíóúñ','aeioun', datos$texto)


# 1.1. Algunos análisis básicos
df <- tibble::rowid_to_column(datos, "ID")

# Palabras con más frecuencia de aparición: 1-grama y Bigrama
##################################################################

limpiar_tokenizar <- function(texto){
  nuevo_texto <- str_split(texto, " ")[[1]]
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Generamos limpiadores con mutate
# Una caracter?stica de las tibble es que pueden almacenar cualquier tipo de elemento
df <- df %>%
  mutate(texto_tokenizado = map(.x = texto, .f = limpiar_tokenizar)) # Crea nueva variables

# mostrando los hallazgos 
df %>% select(texto_tokenizado) %>% head() %>% pull()

df$texto_tokenizado[[2]]

# La funcion unnest_tokens() del paquete tidytext permite, entre otras cosas, 
# automatizar el proceso tokenizacion y almacenamiento en formato tidy en un unico paso.
df_tidy <- df %>% 
  select(-texto) %>% unnest()
# Renombramos la variable texto_tokenizado
df_tidy <- df_tidy %>% 
  dplyr::rename(token = texto_tokenizado)

# Frecuencia de palabras por tipo
# -------------------------------
df_tidy %>% group_by(plaza,token) %>% count(token) %>% group_by(plaza) %>%
  top_n(10,n) %>% arrange(plaza,desc(n)) %>% print(n=30)


# Eliminando Stop words
# ---------------------
# https://www.rdocumentation.org/packages/stopwords/versions/2.2
palabras_stopwords_es <- stopwords("es")
#palabras_stopwords_de <- stopwords("de")
#palabras_stopwords_fr <- stopwords("fr")
#palabras_stopwords_en <- stopwords("en")

dataf_stp <- as.data.frame(palabras_stopwords_es)
dim(dataf_stp)

palabrasstop <- c("quinoa","https","t.co","rt","a",
                  "quinua","<u+","><u+fe","	f>","	f><u+",
                  "<u+fe","><u+","e+","	b>","><u+a","ll",
                  "f>","+","u",">","<","b","f","c><u+fe",
                  "d>","c>","a>","f><u+","^^","d><u+","?",
                  "h","=","g","|","b>","e>","i?<u+","e><u+",
                  "a><u+fe",">??<u+",">t","||","c><u+",
                  "â<u+",">s","xd","	xs","ii","jajaja",
                  ">>","cc","l<u+","ba>","=>","$a$","fsf",
                  "qq","$e$","d<u+","tt","cs","iii","b><u+",
                  "th",">m","ab><u+fe","<u+f","bd>","www",
                  "n°","à","ac","cm","f>g","°c","b>ier","	ss",
                  "ab","aa><u+fe","d><u+fe","<u+fffd>","cd>",
                  "cb>","|d","bd><u+fe","ccc","iphone")

lista_stopwords <- c(palabras_stopwords_es,palabrasstop)
lista_stopwords

palabras_stopwords_fr

# Se filtran las stopwords
a <- dim(df_tidy)[1]
a
df_tidy <- df_tidy %>% 
  filter(!(token %in% lista_stopwords))
b <- dim(df_tidy)[1]
b
cat("Resto tidy: ",b/a)


# limpiando stopwords adicionales
stoped <- df_tidy %>% group_by(token) %>% summarise(n1 = n()) %>% 
     arrange(desc(n1))
view(stoped)

# Representacion grafica de las frecuencias
df_tidy %>% group_by(token) %>% summarise(n1 = n()) %>% 
  arrange(desc(n1)) %>% slice_max(order_by = n1, n = 10) %>%
  ggplot(aes(x = reorder(token,n1), y = n1)) +
  geom_col(show.legend = TRUE,fill = "blue") +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Palabras mas usadas",
       subtitle = "Stopwords retiradas", 
       x = "Palabra",
       y = "Número de veces usada")
#ggsave("4. figures/Q2.6. Freq_palabras_mas_usadas (palta - espana).png")

# Para generar arreglos de datos
# ------------------------------
# Vamos a seperarlo por mayor detalle
# ESPAÑA

#especifico1 <- datos[grepl("iphone", tolower(datos$texto)),]
especifico2 <- datos[grepl("pan", tolower(datos$texto)),]
especifico3 <- datos[grepl("hass", tolower(datos$texto)),]
especifico4 <- datos[(grepl("pan", tolower(datos$texto)))|grepl("hass", tolower(datos$texto)),]
especifico5 <- datos[(grepl("pan", tolower(datos$texto)))&grepl("hass", tolower(datos$texto)),]
especifico6 <- datos[grepl("organica", tolower(datos$texto)),]

dim(datos)
dim(especifico2)
dim(especifico3)
dim(especifico4)
dim(especifico5)
dim(especifico6)

library(SnowballC)
df_tidy_stem <- df_tidy
df_tidy_stem <- df_tidy_stem %>% mutate(stem = wordStem(token), languaje = "porter")


# (4) Analisis de bigramas:
# ### #######################################
# grafo de palabras que se combinan de 
# dos en dos para dar significado a los mensajes
# Nos interesa entender la relación entre palabras en una opinión.
# --- ---------------------
# --- ---------------------

# 1.2. Bigramas
# Nos creamos un lexicon de stopwords en español a medida para el caso del libro de Pedro Sánchez
lexiconSW<-lista_stopwords # usamos la lista de stopwords generada con anterioridad.
lexiconSW<-append(lexiconSW,c(""))

lexiconSW<-as.data.frame(lexiconSW)
names(lexiconSW)<-"word"
lexiconSW$word<-as.character(lexiconSW$word)

# ---------
df_bigrams <- df[,c("texto")]
df_bigrams <- as.data.frame(df_bigrams)
df_bigrams <- tibble::rowid_to_column(df_bigrams, "ID") #  Generamos un ID para cada frase
names(df_bigrams) <- c("ID","frase")
df_bigrams$frase <- as.character(df_bigrams$frase)

# A veces nos interesa entender la relacion entre palabras en una opini?n. 
review_bigrams <- df_bigrams %>%
  unnest_tokens(bigram, frase, token = "ngrams", n = 2) # separamos token 2 - grams

bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") # separamos word por bigrama

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% lexiconSW$word) %>%
  filter(!word2 %in% lexiconSW$word) # eliminamos  stop words por bigrama

bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE) # contamos la cantidad de words por bigrama

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") # count bigrams cleaning
bigrams_united %>%
  dplyr::count(bigram, sort = TRUE)


# Podemos visualizarlo también
# ----------------------------
review_subject <- df_bigrams %>% 
  unnest_tokens(word, frase) %>% 
  anti_join(lexiconSW)

my_stopwords <- data_frame(word = c(as.character(1:10)))
review_subject <- review_subject %>% 
  anti_join(my_stopwords)

title_word_pairs <- review_subject %>% 
  pairwise_count(word, ID, sort = TRUE, upper = FALSE)

# Nos generamos el listado de bigramas
# ------------------------------------
listadoBigramas<-title_word_pairs[which(title_word_pairs$n>25),]
set.seed(1234)
title_word_pairs %>%
  filter(n >= 25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Bigramas')


