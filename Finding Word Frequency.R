#Setting Libraries for text - mining 
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
require(ggplot2)
data<- tweets[[5]]
#Load Text - tweets[[5]] is available after running Mining_Tweets_by_Trends is just an example
text<-Corpus(VectorSource(data))
#Text Transformation
text_clean<- tm_map(text, removePunctuation)
text_clean<- tm_map(text_clean, content_transformer(tolower))
text_clean<- tm_map(text_clean, removeNumbers)
text_clean<- tm_map(text_clean, stripWhitespace)
text_clean<- tm_map(text_clean, removeWord, stopwords("english"))
text_clean<- tm_map(text_clean, removeWord, stopwords("greek"))
greek_stop_words<-c("εκει","εχεις","αλλα","άλλα","τι","κατά","γιατι","γιατί","αλλά","ως","μέσα","ειχε","όπως","όλο","ο","α","β","γ","δ","ε","ζ","η","θ","ι","κ","λ","μ","ν","ξ","ο","π","ρ","σ","τ","υ","φ","χ","ψ","ω","a","b","c","d","e","f","να","ναι","μας","τετοιες","ήταν","ηταν","αυτο","ας","εγω","εχει","ή","η","εκεί","και","λίγο","λιγο","πάλι","μονο","απ","μόνο","αυτά","αυτή","αυτα","αυτη","εγώ","ούτε","υπάρχει","-","κάνει","στους","κάθε","πρέπει","τώρα","λέει","όχι","ήταν","amp","δύο","σαν","το","να","για","του","είναι","ειναι","στις","έχω","μετά","μη","κάτι","είσαι","πολύ","σήμερα","καλημέρα","όλα","ολα","όλοι","ολοι","όλες","ολες","πολυ","πολλή","πολλά","πολλη","πολλα","την","με","του","της","τα","που" , "δεν", "στο","είναι", "θα", "τον","σε","από","απο", "μου","στην","οι", "τους","μας","τη", "των", "στη","στα","τις", "ότι","οτι", "σου","στον","αλλά","μια", "τι","αν","σας","έχει","ένα","αυτό","δε","όταν","κι", "γιατί", "πως","πιο", "μην", "έχουν", "ρε","μόνο")
stop_words <- append(greek_stop_words , stopwords::stopwords(language = "en"))
text_clean <- tm_map(text_clean , removeWords , stop_words)
#Greek StopWords , Symbols
#Word Frequency
dtm<-TermDocumentMatrix(text_clean)
m <-as.matrix(dtm)
v<- sort(rowSums(m), decreasing = TRUE)
d<- data.frame(word = names(v), freq = v)
head(d,10)
#Plot Word frequencies
barplot(d[1:10,]$freq , las = 2 , names.arg = d[1:20,]$word , col = "lightblue" , main = "Most Frequent Words" , xlab = "frequency", ylab = "Word frequencies")