#Run This 
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
library(tm)
# text mining library
#devtools::install_github("juliasilge/tidytext")
library(tidytext)
# plotting packages
#Getting the trends
#Autorization for Google Maps API
key = 'your key'
register_google(key)
#Autorization for Twitter's  API
## access token method: create token and save it as an environment variable
create_token(
  app = "Your App's Name",
  consumer_key = "Consumer Key",
  consumer_secret = "Consumer Secret",
  access_token = "Access Token",
  access_secret = "Access Secret")
#Get Trends
trends<- get_trends(woeid = 23424833)

#Setting The string word we want to find tweets and Word's Net

q<- trends[[1]][1]

#Mining Tweets

v <- search_tweets(q , n = 18000, retryonratelimit = TRUE, include_rts = FALSE)
#Cleaning Tweets getting text at first


tweets$stripped_text <- gsub("http.*","",  tweets$text)
tweets$stripped_text <- gsub("https.*","", tweets$stripped_text)

v_clean <- tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

v_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

# load list of stop words - from the tidytext package
#Or inserting the below code for Greek Stop Words
#Greek Stop Words
#greek_stop_words<-c("εκει","ως","μέσα","όπως","όλο","ο","α","β","γ","δ","ε","ζ","η","θ","ι","κ","λ","μ","ν","ξ","ο","π","ρ","σ","τ","υ","φ","χ","ψ","ω","a","b","c","d","e","f","να","ναι","μας","τετοιες","ήταν","ηταν","αυτο","ας","εγω","εχει","ή","η","εκεί","και","λίγο","λιγο","πάλι","μονο","απ","μόνο","αυτά","αυτή","αυτα","αυτη","εγώ","ούτε","υπάρχει","-","κάνει","στους","κάθε","πρέπει","τώρα","λέει","όχι","ήταν","amp","δύο","σαν","το","να","για","του","είναι","ειναι","στις","έχω","μετά","μη","κάτι","είσαι","πολύ","σήμερα","καλημέρα","όλα","ολα","όλοι","ολοι","όλες","ολες","πολυ","πολλή","πολλά","πολλη","πολλα","την","με","του","της","τα","που" , "δεν", "στο","είναι", "θα", "τον","σε","από","απο", "μου","στην","οι", "τους","μας","τη", "των", "στη","στα","τις", "ότι","οτι", "σου","στον","αλλά","μια", "τι","αν","σας","έχει","ένα","αυτό","δε","όταν","κι", "γιατί", "πως","πιο", "μην", "έχουν", "ρε","μόνο")
#Stop Words
#stop_words <- append(greek_stop_words , stopwords::stopwords(language = "en"))
#y=c()
#for(i in 1:length(stop_words)){y[i]='SMART'}
#stop_words = data.frame(stop_words,y)
#English Stop Words
data("stop_words")
# view first 6 words
head(stop_words)


nrow(v_clean)

cleaned_tweet_words <- v_clean %>%
  anti_join(stop_words)

nrow(cleaned_tweet_words)

cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

# library(devtools)
#install_github("dgrtwo/widyr")
library(widyr)

v_paired_words <- v %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

v_paired_words %>%
  count(paired_words, sort = TRUE)


library(tidyr)

v_separated_words <- v_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

v_filtered <- v_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
v_counts <- v_filtered %>%
  count(word1, word2, sort = TRUE)

g<-paste("Word Network: Tweets using the hashtag " , q , sep = "")

library(igraph)
library(ggraph)

# plot word network
v_counts %>%
  filter(n >= 24) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = g,
       subtitle = "Text mining twitter data ",
       x = "", y = "")
