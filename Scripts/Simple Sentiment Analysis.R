#using the right libraries 
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)
library('plyr')
require("RCurl")
require("tidyverse")
library(ggmap)
library(grid)
library(rworldmap)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stopwords)
require(quanteda)
require(readtext)
require(quanteda.corpora)
require(newsmap)
require(spacyr)
require(tokenizers)
require(stringdist)
require(readr)
#Reading and assigning tweets text file
#data <- read.delim(file.choose())
#Setting the indicators
positive<-c()
negative<-c()
emotions <- data.frame(positive,negative)
#Choosing file and tokenize words
data<- tokenize(file.choose())
data1<-as.String(data)
#We need only the unique tokens:
tokens<-unique(tokenize_words(data1))
len<- length(tokens[[1]])
#Importing lexicons and matching strings:
positive_words_el <- read_csv("Greek Sentiment Lexicons/positive_words_el.txt")
m1<-amatch(tokens[[1]], positive_words[[1]] , nomatch = 0)
m1<-data.frame(count(m1))  
positive <- sum(count(m1)[[2]])-count(m1)[[2]][1]
negative_words_el <- read_csv("Greek Sentiment Lexicons/negative_words_el.txt")
m2<-amatch(tokens[[1]], negative_words[[1]] , nomatch = 0)
m2<-data.frame(count(m2))  
negative <- sum(count(m2)[[2]])-count(m2)[[2]][1]
mood_of_the_day<- data.frame(positive,negative)
print(positive)
print(negative)
# Descriptive Statitistics on frequency tables m1,  m2 
# get means for variables in data frame m1
sapply(m1, mean, na.rm=TRUE) 
# mean,median,25th and 75th quartiles,min,max
summary(m1)
# Tukey min,lower-hinge, median,upper-hinge,max
fivenum(m1)
# get means for variables in data frame m1
sapply(m1, mean, na.rm=TRUE) 
# mean,median,25th and 75th quartiles,min,max
summary(m2)
# Tukey min,lower-hinge, median,upper-hinge,max
fivenum(m2)
#Reading a Csv file
dataCsv <- read.csv(file.choose())
dataCsv1<-tokenize(as.String(dataCsv))
tokensCsv<-tokenize_words(dataCsv1)
len<- length(tokensCsv[[1]])