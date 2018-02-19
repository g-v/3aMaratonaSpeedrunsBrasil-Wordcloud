# Script that loads data from a text file and generates some cool wordclouds
# written by petSM64

# Warranty:
# This software is provided 'as-is', without any express or implied warranty.


# inicializar
libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)
setwd("~/Dropbox/sgrb/wordcloud")

# Setar options
options(stringsAsFactors = FALSE)

# importar lista de emotes globais do Twitch + FFZ emotes da SGRB
#emotesTwitch <- read.table("emotesTwitch.txt", quote="\"")
emotesTwitch <- Corpus(DirSource("emotesTwitch"))

# importar lista de runners da SGRB
runners <- Corpus(DirSource("runners"))

# importar stopwords (termos inuteis) personalizado
stpw <- Corpus(DirSource("stopwords"))

# importar o texto da maratona (500KB, 20k linhas)
raw_text <- Corpus(DirSource("raw_text"))


# Funcao para limpar o texto
GetCorpus <-function(docs) {
  
  docs <- tm_map(docs, tolower) # tudo em minusculo
  docs <- tm_map(docs, removeNumbers) # remover numeros
  docs <- tm_map(docs, removePunctuation) # remover pontuacao
  docs <- tm_map(docs, removeWords, stopwords("portuguese")) # remover stopwords (aka palavras 
                                                       # sem significado proprio, ex: que, se, isso, etc)
  docs <- tm_map(docs, stemDocument, "portuguese") # pegar o radical das palavras (ex: soubesse -> saber)
  docs <- tm_map(docs, stripWhitespace) # retirar espacos em branco
  docs <- tm_map(docs, PlainTextDocument) # deixar em plain text
  return(docs)
  
}

# Funcao para processar texto dos emotes e runners
GetCorpusSimples <-function(docs) {
  docs <- tm_map(docs, tolower) # tudo em minusculo
  docs <- tm_map(docs, removePunctuation) # remover pontuacao
  docs <- tm_map(docs, stripWhitespace) # retirar espacos em branco
  docs <- tm_map(docs, PlainTextDocument) # deixar em plain text
  return(docs)
}

# processar o texto

# emotes
corpus_emotes <- GetCorpusSimples(emotesTwitch)

# geral
corpus_geral <- GetCorpus(raw_text)

# runners
corpus_runners <- GetCorpusSimples(runners)

# stopwords
corpus_stopwords <- GetCorpusSimples(stpw)


# aqui ja temos o texto processado, hora de separar em 1 e 2 grams
# exemplo de 1 gram: ventilador, carro, jato (qualquer palavra simples)
# exemplo de 2 gram: guarda-chuva, cotovelo ralado, sanduiche banheiro, speed run 
#                                                            (qualquer expressao composta por 2 palavras) 


# 1 gram
# iniciar libraries necessarias para esta etapa
library("rJava")
library("RWeka")
library("tm")
# Sets the default number of threads to use --- para nao bugar no OSX FeelsBadMan
options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
dtmEmotes <- DocumentTermMatrix(corpus_emotes, control= list(tokenize = BigramTokenizer))
dtmGeral <- DocumentTermMatrix(corpus_geral, control= list(tokenize = BigramTokenizer))
dtmRunners <- DocumentTermMatrix(corpus_runners, control= list(tokenize = BigramTokenizer))
dtmStpw <- DocumentTermMatrix(corpus_stopwords, control= list(tokenize = BigramTokenizer))

# tirar termos pouco usados
dtmGeral2 <- removeSparseTerms(dtmGeral, 0.8)


CalcularTermosMaisUsados <- function(dtm, numeroDePalavras) {
  freq <- colSums(as.matrix(dtm))
  ord <- order(freq)
  maisUsados <- freq[tail(ord, numeroDePalavras)]
  maisUsados <- rev(maisUsados)
  m <- as.matrix(maisUsados)
  return (m)
}

# pegar os termos mais usados
matrizGeral <- CalcularTermosMaisUsados(dtmGeral2, 400)

# pegar lista de runners
matrizRunners <- as.matrix(dtmRunners)
matrizRunners <- t(matrizRunners)

# pegar lista de emotes
matrizEmotes <- as.matrix(dtmEmotes)
matrizEmotes <- t(matrizEmotes)

# pegar lista de stopwords
matrizStpw <- as.matrix(dtmStpw)
matrizStpw <- t(matrizStpw)



 
## start: consertar nomes das colunas D:
matrizGeral <- cbind(Termos = rownames(matrizGeral), matrizGeral)
rownames(matrizGeral) <- NULL

matrizRunners <- cbind(Termos = rownames(matrizRunners), matrizRunners)
rownames(matrizRunners) <- NULL
colnames(matrizRunners) <- c("Termos", "lixo")
matrizRunners$lixo <- NULL



listaRunners <- as.data.frame(matrizRunners)
listaRunners <- t(listaRunners)
colnames(listaRunners) <- ("Termos")
listaGeral <- as.data.frame(matrizGeral)

matrizEmotes <- cbind(Termos = rownames(matrizEmotes), matrizEmotes)
rownames(matrizEmotes) <- NULL

listaEmotes <- as.data.frame(matrizEmotes)
colnames(listaEmotes) <- c("Termos", "a", "b")
listaEmotes$a <- NULL
listaEmotes$b <- NULL

listaStpw <- as.data.frame(matrizStpw)

listaStpw <- cbind(Termos = rownames(listaStpw), listaStpw)
rownames(listaStpw) <- NULL
colnames(listaStpw) <- c("Termos", "a", "b")
listaStpw$a <- NULL
listaStpw$b <- NULL
## end: consertar nomes das colunas D:

# hora de subtrair conjuntos --- HYPE

# anti join
library(dplyr)
listaSemRunners <- anti_join(listaGeral, listaRunners, by="Termos", copy = TRUE)
listaSemEmotes <- anti_join(listaSemRunners, listaEmotes, by="Termos", copy = TRUE)
listaSemiFinal <- anti_join(listaSemEmotes, listaStpw, by="Termos", copy = TRUE)

# flip
colnames(listaSemiFinal) <- c("Termos", "freq")
listaSemiFinal <- dplyr::arrange(listaSemiFinal, -row_number())

lista2 <- anti_join(listaGeral, listaStpw, by="Termos", copy = TRUE)
colnames(lista2) <- c("Termos", "freq")
lista2 <- dplyr::arrange(lista2, -row_number())


###
library(tm)
library(SnowballC)
library(wordcloud)

# hora de montar a wordcloud



pal <- brewer.pal(8,"Dark2")
png("wordcloud.png", width=1280,height=800)

listaSemiFinal$freq <- as.numeric(as.character(listaSemiFinal$freq))

wordcloud(listaSemiFinal$Termos, freq=(listaSemiFinal$freq), scale=c(8,.2), max.words=200, 
          random.order=FALSE, rot.per=.15, colors=pal)

dev.off()

####################
# wordcloud geral

library(tm)
library(SnowballC)
library(wordcloud)

pal <- brewer.pal(8,"Dark2")

png("wordcloud_geral.png", width=1280,height=800)

lista2$freq <- as.numeric(as.character(lista2$freq))

wordcloud(lista2$Termos, freq=(lista2$freq), scale=c(10,.8), max.words=200, 
          random.order=FALSE, random.color = TRUE, rot.per=.15, colors=pal)

dev.off()













