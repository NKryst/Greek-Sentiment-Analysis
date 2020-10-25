library(textclean)
#Replace full stop and comma
#data<-gsub("\\.","",data)
#data<-gsub("\\,","",data)
#Split sentence
#words<-strsplit(data," ")
#Calculate word frequencies
#Since we have used already the Sentiment Analysis scsript we have tokenized tweets
fileslist <- choose.files()
data<-read_lines(fileslist, skip_empty_rows = TRUE, progress = TRUE)
#Checking encoding of the document
encoding_check<-stringi::stri_enc_detect(data)
if(encoding_check[[1]][1,1]!="UTF-8"){
  #Changing encoding from ISO-8859-7 to UTF-8
  c<-encoding_check[[1]][1,1]
  #Renaming the file
  l<-substr(fileslist[e],nchar(fileslist[e])-13,nchar(fileslist[e]))
  b<-paste("fixed_encoding_UTF_8_",l,sep = "")
  file.create(b)
  #Fixing the encoding and writing a new file 
  writeLines(iconv(readLines(fileslist[e]), from ="ISO-8859-7" , to = "UTF8"),file(b, encoding="UTF-8"))
  data<-read_lines(b, skip_empty_rows = TRUE, progress = TRUE)
  writeLines(data,file(b))
}
#Setting Stop Words and Text cleaning
greek_stop_words<-c("εκει","https","άλλους","άλλο","άλλη","κάποια","πάνω","κάτω","t.co","u","0001f92a","εχεις","αλλα","άλλα","τι","κατά","γιατι","γιατί","αλλά","ως","μέσα","ειχε","όπως","όλο","ο","α","β","γ","δ","ε","ζ","η","θ","ι","κ","λ","μ","ν","ξ","ο","π","ρ","σ","τ","υ","φ","χ","ψ","ω","a","b","c","d","e","f","να","ναι","μας","τετοιες","ήταν","ηταν","αυτο","ας","εγω","εχει","ή","η","εκεί","και","λίγο","λιγο","πάλι","μονο","απ","μόνο","αυτά","αυτή","αυτα","αυτη","εγώ","ούτε","υπάρχει","-","κάνει","στους","κάθε","πρέπει","τώρα","λέει","όχι","ήταν","amp","δύο","σαν","το","να","για","του","είναι","ειναι","στις","έχω","μετά","μη","κάτι","είσαι","πολύ","σήμερα","καλημέρα","όλα","ολα","όλοι","ολοι","όλες","ολες","πολυ","πολλή","πολλά","πολλη","πολλα","την","με","του","της","τα","που" , "δεν", "στο","είναι", "θα", "τον","σε","από","απο", "μου","στην","οι", "τους","μας","τη", "των", "στη","στα","τις", "ότι","οτι", "σου","στον","αλλά","μια", "τι","αν","σας","έχει","ένα","αυτό","δε","όταν","κι", "γιατί", "πως","πιο", "μην", "έχουν", "ρε","μόνο")
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
words<-clean_tokens
words.freq<-as.data.frame(table(unlist(words)))
names(words.freq)<-c("strings","frequency")
final_words.freq<-words.freq[order(words.freq$frequency,decreasing = TRUE),]
View(final_words.freq)