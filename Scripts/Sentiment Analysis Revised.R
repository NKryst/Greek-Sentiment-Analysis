#Loading Libraries
library(tokenizers)
library(readxl)
library(readr)
library(stringr)
library(stringdist)
library(tm)
library(bpa)
library(openxlsx)
library(dplyr)
#Sentiment Analysis
#Import Sentiment Lexicon
Greek_Lexicon <- read_excel("Greek Sentiment Lexicons/Fixed_Greek_Lexicon.xlsx")
#Greek_Lexicon <- read_excel("C:/Users/nickr/Desktop/Projects/R_Twitter/Twitter/Greek Sentiment Lexicons/Greek_Lexicon.xlsx")
#View(Greek_Lexicon)
Greek_Lexicon<-as.data.frame(Greek_Lexicon)
Greek_Lexicon$Anger1<-as.numeric(as.character(Greek_Lexicon$Anger1))
Greek_Lexicon$Anger2<-as.numeric(as.character(Greek_Lexicon$Anger2))
Greek_Lexicon$Anger3<-as.numeric(as.character(Greek_Lexicon$Anger3))
Greek_Lexicon$Anger4<-as.numeric(as.character(Greek_Lexicon$Anger4))
Greek_Lexicon$Disgust1<-as.numeric(as.character(Greek_Lexicon$Disgust1))
Greek_Lexicon$Disgust2<-as.numeric(as.character(Greek_Lexicon$Disgust2))
Greek_Lexicon$Disgust3<-as.numeric(as.character(Greek_Lexicon$Disgust3))
Greek_Lexicon$Disgust4<-as.numeric(as.character(Greek_Lexicon$Disgust4))
Greek_Lexicon$Fear1<-as.numeric(as.character(Greek_Lexicon$Fear1))
Greek_Lexicon$Fear2<-as.numeric(as.character(Greek_Lexicon$Fear2))
Greek_Lexicon$Fear3<-as.numeric(as.character(Greek_Lexicon$Fear3))
Greek_Lexicon$Fear4<-as.numeric(as.character(Greek_Lexicon$Fear4))
Greek_Lexicon$Happiness1<-as.numeric(as.character(Greek_Lexicon$Happiness1))
Greek_Lexicon$Happiness2<-as.numeric(as.character(Greek_Lexicon$Happiness2))
Greek_Lexicon$Happiness3<-as.numeric(as.character(Greek_Lexicon$Happiness3))
Greek_Lexicon$Happiness4<-as.numeric(as.character(Greek_Lexicon$Happiness4))
Greek_Lexicon$Sadness1<-as.numeric(as.character(Greek_Lexicon$Sadness1))
Greek_Lexicon$Sadness2<-as.numeric(as.character(Greek_Lexicon$Sadness2))
Greek_Lexicon$Sadness3<-as.numeric(as.character(Greek_Lexicon$Sadness3))
Greek_Lexicon$Sadness4<-as.numeric(as.character(Greek_Lexicon$Sadness4))
Greek_Lexicon$Surprise1<-as.numeric(as.character(Greek_Lexicon$Surprise1))
Greek_Lexicon$Surprise2<-as.numeric(as.character(Greek_Lexicon$Surprise2))
Greek_Lexicon$Surprise3<-as.numeric(as.character(Greek_Lexicon$Surprise3))
Greek_Lexicon$Surprise4<-as.numeric(as.character(Greek_Lexicon$Surprise4))
#View(Greek_Lexicon)
#Cleaning Vowels Greek_Lexicon
grclean1<-c()
grclean2<-c()
grclean3<-c()
grclean4<-c()
grclean5<-c()
grclean6<-c()
grclean7<-c()
grclean8<-c()
grclean9<-c()
grclean10<-c()
for (k in 1:length(Greek_Lexicon[[1]])) {
  grclean1[[k]]<-gsub("ά","α",Greek_Lexicon[[1]][k])
  grclean2[[k]]<-gsub("έ","ε",grclean1[[k]])
  grclean3[[k]]<-gsub("ή","η",grclean2[[k]])
  grclean4[[k]]<-gsub("ί","ι",grclean3[[k]])
  grclean5[[k]]<-gsub("ό","ο",grclean4[[k]])
  grclean6[[k]]<-gsub("ύ","υ",grclean5[[k]])
  grclean7[[k]]<-gsub("ώ","ω",grclean6[[k]])
  grclean8[[k]]<-gsub("ϋ","υ",grclean7[[k]])
  grclean9[[k]]<-gsub("ϊ","ι",grclean8[[k]])
  grclean10[[k]]<-gsub("ΐ","ι",grclean9[[k]])
}
#View(Greek_Lexicon)
#Loading Data 
#txt or csv
#data_csv<-read.csv(file.choose(),sep=",",skipNul = TRUE)
fileslist <- choose.files()
fileslist <-sort(fileslist)
mood<-matrix(nrow = length(fileslist), ncol = 7)

for(e in 1:length(fileslist)){
  data<-read_lines(fileslist[e], skip_empty_rows = TRUE, progress = TRUE)
  #Checking encoding of the document
  encoding_check<-stringi::stri_enc_detect(data)
  if(encoding_check[[1]][1,1]!="UTF-8" || encoding_check[[2]][1,1]!="UTF-8" ){
    #Changing encoding from ISO-8859-7 to UTF-8
    c<-encoding_check[[1]][1,1]
    #Renaming the file
    l<-substr(fileslist[e],nchar(fileslist[e])-19,nchar(fileslist[e]))
    b<-paste("fixed_encoding_UTF_8_",l,sep = "")
    file.create(b)
    #Fixing the encoding and writing a new file 
    writeLines(iconv(readLines(fileslist[e]), from ="ISO-8859-7" , to = "UTF8"),file(b, encoding="UTF-8"))
    data<-read_lines(b, skip_empty_rows = TRUE, progress = TRUE)
    writeLines(data,file(b))
  } else if (encoding_check == ""){
    next}
  #Setting Stop Words and Text cleaning
  greek_stop_words<-c("εκει","https","άλλους","άλλο","καν","ειστε","άλλη","κάποια","πάνω","κάτω","t.co","u","0001f92a","εχεις","αλλα","άλλα","τι","κατά","γιατι","γιατί","αλλά","ως","μέσα","ειχε","όπως","όλο","ο","α","β","γ","δ","ε","ζ","η","θ","ι","κ","λ","μ","ν","ξ","ο","π","ρ","σ","τ","υ","φ","χ","ψ","ω","a","b","c","d","e","f","να","ναι","μας","τετοιες","ήταν","ηταν","αυτο","ας","εγω","εχει","ή","η","εκεί","και","λίγο","λιγο","πάλι","μονο","απ","μόνο","αυτά","αυτή","αυτα","αυτη","εγώ","ούτε","υπάρχει","-","κάνει","στους","κάθε","πρέπει","τώρα","λέει","όχι","ήταν","amp","δύο","σαν","το","να","για","του","είναι","ειναι","στις","έχω","μετά","μη","κάτι","είσαι","πολύ","σήμερα","καλημέρα","όλα","ολα","όλοι","ολοι","όλες","ολες","πολυ","πολλή","πολλά","πολλη","πολλα","την","με","του","της","τα","που" , "στο","είναι", "θα", "τον","σε","από","απο", "μου","στην","οι", "τους","μας","τη", "των", "στη","στα","τις", "ότι","οτι", "σου","στον","αλλά","μια", "τι","αν","σας","έχει","ένα","αυτό","δε","όταν","κι", "γιατί", "πως","πιο", "μην", "έχουν", "ρε","μόνο")
  stop_words <- append(greek_stop_words , stopwords::stopwords(language = "en"))
  #removing urls
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  data<-gsub(pattern = url_pattern,"",data)
  #removing english strings
  data<-gsub("[a-z]|[A-Z]","",data)
  #removing emojis
  data<-gsub("<U|[$-_@.&+]|[0-9]|F|[0-9]>","",data)
  #Pops up a window to choose the txt file we want
  #Tokenization techniques
  tokens <-tokenize_words(data , lowercase = TRUE, stopwords = stop_words , strip_punct = TRUE , strip_numeric = TRUE)
  #Cleaning vowels tokens 
  clean1<-c()
  clean2<-c()
  clean3<-c()
  clean4<-c()
  clean5<-c()
  clean6<-c()
  clean7<-c()
  clean8<-c()
  clean9<-c()
  clean10<-c()
  clean1<-gsub("ά","α",unlist(tokens))# We unlist the tokens to "clean"
  clean2<-gsub("έ","ε",clean1)
  clean3<-gsub("ή","η",clean2)
  clean4<-gsub("ί","ι",clean3)
  clean5<-gsub("ό","ο",clean4)
  clean6<-gsub("ύ","υ",clean5)
  clean7<-gsub("ώ","ω",clean6)
  clean8<-gsub("ϋ","υ",clean7)
  clean9<-gsub("ϊ","ι",clean8)
  clean10<-gsub("ΐ","ι",clean9)
  count<- 0 
  clean_tokens<-tokens
  for(f in 1:length(clean_tokens)){
    for(g in 1:length(clean_tokens[[f]])){
      count<-count+1
      clean_tokens[[f]][g]<-clean10[count]
    }
  }
  #General
  #Setting Up Variables for Outputs of the code
  all_sentiments<-matrix(nrow = length(clean_tokens), ncol = 7)
  o<-c()
  s<-0
  z<-c()
  a<-0
  #Select the String Matching Technique
  str_match_meth<-"hamming"
  #Starting to string match for each tweet's tokens and assigning sentiment values according to Sentiment Lexi
  for (i in 1:length((clean_tokens)))   {
    o<-c()
    s<-0
    z<-c()
    a<-0
    #Pick a tweet
    y<-i
    for (p in 1:length(clean_tokens[[y]])) {
      a<-nchar(clean_tokens[[y]][p])
      if(is.na(a)!=TRUE){
        #String Matching Function amatch() returns the position of word in grclean10 which is the First Column of Greek Sentiment Lexi
        o[p]<-amatch(clean_tokens[[y]][p], grclean10, method = str_match_meth, nomatch = 0, nthread = 8)
      }
    }
    #Controlling the matches if there are null for each word
    m<-c()
    m<-which(o!=0,arr.ind = TRUE )
    if(length(m)==0){
      emotions <-matrix(0L,nrow=1, ncol=7)
    } else {
      emotions<-matrix(0L,nrow= length(m),ncol = 7)
      for (j in 1:length(m)) {
        t<-o[m[j]]
        emotions[j,1]<-max(Greek_Lexicon[t,14:17])
        emotions[j,2]<-max(Greek_Lexicon[t,18:21]) 
        emotions[j,3]<-max(Greek_Lexicon[t,22:25])
        emotions[j,4]<-max(Greek_Lexicon[t,26:29])
        emotions[j,5]<-max(Greek_Lexicon[t,30:33])
        emotions[j,6]<-max(Greek_Lexicon[t,34:37])
        emotions[j,7]<-max(Greek_Lexicon[t,10:13])
        emotions[is.na(emotions)]<-0
      }
      for(h in 1:length(m)){
        for(v in 1:6){
          if (emotions[h,v]<3&is.na(emotions[h,v])!=TRUE){
            emotions[h,v]<-0
          } else {
            next
        }
      }
    }
    
    emotions[is.na(emotions)]<-0
    only_emotions<-emotions[rowSums(is.na(emotions)) != ncol(emotions),]
      if(length(only_emotions)==7){all_sentiments[y,]<- only_emotions
  } else {
    all_sentiments[y,]<-c(colMeans(only_emotions))
  }
    }
  }
  column_names<- c("Anger","Disgust","Fear","Happiness","Sadness","Surprise","Polarity")
  neutral_rows<-which(all_sentiments==0 , arr.ind = TRUE)
  #Mood of the Day including empty rows
  mood_of_the_day<-c(colMeans(all_sentiments))
  #Mood of the Day excluding empty rows
  all_sentiments[is.na(all_sentiments)]<-0
  all_sentiments[all_sentiments == 0]<-NA
  only_sentiments<-all_sentiments[rowSums(is.na(all_sentiments)) != ncol(all_sentiments),]
  only_sentiments[is.na(only_sentiments)]<-0
  mood_of_the_day_only_sentiments<-matrix(colMeans(only_sentiments), nrow = 1, ncol =7)
  mood[e,]<-mood_of_the_day_only_sentiments
  mood<-as.data.frame(mood)
  names(mood)<-column_names
  all_sentiments<-(as.data.frame(all_sentiments))
  names(all_sentiments)<-column_names
  all_sentiments[is.na(all_sentiments)]<-0
}
title<-paste("data_",str_match_meth,"revised.csv",sep = "")
write.csv2(mood, file = title)
#saveWorkbook(wb = hi,file = excelName)
#excelName<-paste(Sys.Date(),"moodTwitter.xlsx",sep = "_")
#hi<-createWorkbook()
#addWorksheet(hi, l)
#addWorksheet(hi, paste("data",l,sep = "_"))
#writeData(hi, sheet = l , x =all_sentiments)
#writeData(hi, sheet = paste("data",l,sep = "_") , x =as.data.frame(data))