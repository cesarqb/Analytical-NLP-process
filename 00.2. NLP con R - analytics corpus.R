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

# ============ # ============== # ==============
# ============ # ============== # ==============
calculartopics <- function(dtm) {
  K <- c(10, 20, 30, 40, 50, 60)
  results <- list()
  
  i = 1
  for (k in K){
    cat("\n\n\n##########\n ", k, "topics", "\n")
    res <- cvLDA(k, dtm)
    results[[i]] <- res
    i = i + 1
  }
  
  # Lo representamos visualmente
  df <- data.frame(
    k = rep(K, each=10),
    perp =  unlist(lapply(results, '[[', 'perplexity')),
    loglk = unlist(lapply(results, '[[', 'logLik')),
    stringsAsFactors=F)
  
  min(df$perp)
  df$ratio_perp <- df$perp / max(df$perp)
  df$ratio_lk <- df$loglk / min(df$loglk)
  
  df <- data.frame(cbind(
    aggregate(df$ratio_perp, by=list(df$k), FUN=mean),
    aggregate(df$ratio_perp, by=list(df$k), FUN=sd)$x,
    aggregate(df$ratio_lk, by=list(df$k), FUN=mean)$x,
    aggregate(df$ratio_lk, by=list(df$k), FUN=sd)$x),
    stringsAsFactors=F)
  names(df) <- c("k", "ratio_perp", "sd_perp", "ratio_lk", "sd_lk")
  
  pd <- melt(df[,c("k","ratio_perp", "ratio_lk")], id.vars="k")
  pd2 <- melt(df[,c("k","sd_perp", "sd_lk")], id.vars="k")
  pd$sd <- pd2$value
  levels(pd$variable) <- c("Perplexity", "LogLikelihood")
  
  p <- ggplot(pd, aes(x=k, y=value, linetype=variable))
  pq <- p + geom_line() + geom_point(aes(shape=variable),
                                     fill="white", shape=21, size=1.40) +
    geom_errorbar(aes(ymax=value+sd, ymin=value-sd), width=4) +
    scale_y_continuous("Ratio wrt worst value") +
    scale_x_continuous("Número de topics",
                       breaks=K) +
    theme_bw()
  pq
  
}

# Para iterar en la identificación del número de topics óptimo
cvLDA <- function(Ntopics,dtm,K=10) {
  folds<-cvFolds(nrow(dtm),K,1)
  perplex <- rep(NA,K)
  llk <- rep(NA,K)
  for(i in unique(folds$which)){
    cat(i, " ")
    which.test <- folds$subsets[folds$which==i]
    which.train <- {1:nrow(dtm)}[-which.test]
    dtm.train <- dtm[which.train,]
    dtm.test <- dtm[which.test,]
    lda.fit <- LDA(dtm.train, k=Ntopics, method="Gibbs",
                   control=list(verbose=50L, iter=100))
    perplex[i] <- perplexity(lda.fit,dtm.test)
    llk[i] <- logLik(lda.fit)
  }
  return(list(K=Ntopics,perplexity=perplex,logLik=llk))
}

# ============ # ============== # ==============
# ============ # ============== # ==============
# ============ # ============== # ==============





# Carga de datos
# --------------
# --------------
palta_esp <- read.csv("productos/palta_esp.csv",encoding = "Latin1",sep = ";")
palta_fr <- read.csv("productos/palta_fr.csv",encoding = "Latin1",sep = ";")
palta_uk <- read.csv("productos/palta_uk.csv",encoding = "Latin1",sep = ";")


# Creando variables plaza y  producto
palta_esp$plaza <- "Espana"
palta_fr$plaza <- "Francia"
palta_uk$plaza <- "Reino_Unido"
palta <- rbind(palta_esp,palta_fr,palta_uk)
#palta <- rbind(palta_esp,palta_uk)

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

# Eliminando Stop words
# ---------------------
palabras_stopwords_es <- stopwords("es")
#palabras_stopwords_de <- stopwords("de")
#palabras_stopwords_fr <- stopwords("fr")
#palabras_stopwords_en <- stopwords("en")

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

# Se filtran las stopwords
a <- dim(df_tidy)[1]
a
df_tidy <- df_tidy %>% 
  filter(!(token %in% lista_stopwords))
b <- dim(df_tidy)[1]
b
cat("Resto tidy: ",b/a)

# Análisis cluster
##############################################################


# (1er acercamiento) 
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
library("textmineR")

#create DTM
dtm <- CreateDtm(df_tidy$token, 
                 doc_names = df_tidy$ID, 
                 ngram_window = c(1, 2))

#explore the basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)

# Elimina las palabras que aparecen menos de 2 veces o en más de la mitad de 
# los documentos
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm


# Run LDA
# -------
text_dtm <- DocumentTermMatrix(dtm)
text_dtm

text_lda <- LDA(text_dtm, k = 2, method = "VEM", control = NULL)
text_lda

text_topics <- tidy(text_lda, matrix = "beta")
text_topics

# Graficamos los topics
text_top_terms <- text_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

text_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



#---------------------------------------------------------------------
#---------------------------------------------------------------------
corpus <- corpus(datos$texto)
cdfm <- dfm(corpus, remove=lista_stopwords)

# Quitamos palabras que solo salgan 1 vez
cdfm <- dfm_trim(cdfm, min_docfreq = 2, verbose=TRUE)

# Ahora lo exportamos a un formato para procesar los Topic Models.
dtm <- convert(cdfm, to="topicmodels")

# Calculamos ahora los topcis óptimos
calculartopics(dtm)
# Estimamos el LDA con el número óptimo de topics que nos haya salido
lda <- LDA(dtm, k = 5, method = "Gibbs", 
           control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))


# Obtenemos la palabra más representativa de cada topic (conjunto de palabras. Topic=cluster)
terms(lda)
# Las top n-palabras de cada topic
trms <- t(terms(lda, k=30))

# Sacamos los datos de cada topic
terminos <- tidy(lda, matrix = "beta")

# Sacamos los top 8 términos por cada topic
terminos %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)%>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
#ggsave("4. figures/Q5.5. clusterin de terminos.png")


# (6) Sentiment Analysis:
# ### #######################################
# Análisis del sentimiento, tanto positivo y negativos, así como por 
# emociones básicas de la persona (alegría, tristeza, miedo, rechazo, etc.)
# --- ---------------------
# --- ---------------------

# Vamos a utilizar 3 lexicon para la clasificación de sentimiento
#   afinn: signa palabras con una puntuaci?n que se extiende entre -5 y 5, con puntuaciones negativas que indican un sentimiento negativo y puntuaciones positivas que indican un sentimiento positivo.
#   bing: clasifica las palabras de forma binaria en categor?as positivas y negativas.
#   nrc: clasifica las palabras de forma binaria ("s?" / "no") en categor?as de (positivo o negativo, enojo, anticipaci?n, asco, miedo, alegr?a, tristeza, sorpresa y confianza). 

# Vamos a coger las palabras en español del diccionario NRC
nrc<- get_sentiment(datos$texto, method="nrc",lang="spanish")
#nrc<- get_sentiment(datos$texto, method="nrc",lang="french")
#nrc<- get_sentiment(datos$texto, method="nrc",lang="english")
table(nrc)

# Obtenemos las emociones
emotions <- get_nrc_sentiment(datos$texto,lang="spanish")
#emotions <- get_nrc_sentiment(datos$texto,lang="french")
#emotions <- get_nrc_sentiment(datos$texto,lang="english")
emotions

dataframefilters <- cbind(datos[c("X","texto")], as.data.frame(emotions))

dim(datafilter)
dim(dataframefilters)
dim(datos)

emo_bar = colSums(emotions)
emo_bar

# considerando la plaza que se desee analizar
# -------------------------------------------
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
#emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
emo_sum

ggplot(emo_sum, aes(factor(emotion), count, fill = emotion)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = paste0("Frecuencia de emociones"),
       subtitle = "Un mensaje puede provocar mas de una emocion",
       x = "Emocion",
       y = "Numero de mensajes") + theme(legend.position = "none")
#ggsave("4. figures/Q6.5. Sentimental_analisis (quinua-españa).png")





emo_sum2 <- emo_sum

emo_sum2 <- emo_sum2 %>% mutate(
  emotion2 = recode(emotion,"anger" = "negative",
                          "anticipation" = "neutral",
                          "disgust" = "negative",
                          "fear" = "neutral",
                          "joy" = "positive",
                          "sadness" = "negative",
                          "surprise" = "neutral",
                          "trust" = "positive",
                          "negative" = "negative",
                          "positive" = "positive"))

emo_sum2
table(emo_sum2$emotion2)

emo_sum3 <- emo_sum2 %>% group_by(emotion2) %>% summarise(n = sum(count))
names(emo_sum3) <- c("count","emotion")

emo_sum3$emotion = factor(emo_sum3$emotion, levels=emo_sum3$emotion[order(emo_sum3$count, decreasing = TRUE)])
emo_sum3

ggplot(emo_sum3, aes(factor(emotion), count, fill = emotion)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(title = paste0("Frecuencia de emociones"),
       subtitle = "Un mensaje puede provocar mas de una emocion",
       x = "Emocion",
       y = "Numero de mensajes") + theme(legend.position = "none")



# ---------
# afinn=get_sentiments("afinn") 
# bing=get_sentiments("bing")

# sentimientos <- get_sentiment_dictionary(dictionary = "syuzhet", language = "spanish")
# sentimientos

# table(afinn)
# sentimientos <- bing %>% 
#   mutate(valor = if_else(sentiment == "negative", -1, 1))

# afinn <- get_sentiment(datos$texto, method="afinn",lang="spanish")
# bing <- get_sentiment(datos$texto, method="bing",lang="spanish")

# table(afinn)


