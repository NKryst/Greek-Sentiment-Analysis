library(igraph)
library(dplyr)
library(tidyr)
library(tidytext)
library(tokenizers)
library(readxl)
library(readr)
library(stringr)
library(stringdist)
library(tm)
library(bpa)
library(openxlsx)
library(ggplot2)
library(networkD3)
library(glue)
library(cowplot)
library(magrittr)
library(plotly)
library(tidyverse)
library(widyr)
# Data Wrangling and Visualization
library(glue)
library(cowplot)
library(magrittr)
library(plotly)
library(tidyverse)
library(widyr)
# Date & Time Manipulation.
library(hms)
library(lubridate) 
# Text Mining
library(tidytext)
library(tm)
library(wordcloud)
# Network Analysis
library(igraph)
# Network Visualization (D3.js)
library(networkD3)
# Set notebook directory.

fileslist <- choose.files()
fileslist <-sort(fileslist)
mood<-matrix(nrow = length(fileslist), ncol = 7)

for(e in 1:length(fileslist)){
  data<-read_lines(fileslist[e], skip_empty_rows = TRUE, progress = TRUE)
  #Checking encoding of the document
  encoding_check<-stringi::stri_enc_detect(data)
  if(encoding_check[[1]][1,1]!="UTF-8" || encoding_check[[2]][1,1]!="UTF-8" || encoding_check[[3]][1,1]!="UTF-8"|| encoding_check[[4]][1,1]!="UTF-8" || encoding_check[[5]][1,1]!="UTF-8"){
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
  greek_stop_words<-c("εκει","δεν","https","άλλους","άλλο","καν","ειστε","άλλη","κάποια","πάνω","κάτω","t.co","u","0001f92a","εχεις","αλλα","άλλα","τι","κατά","γιατι","γιατί","αλλά","ως","μέσα","ειχε","όπως","όλο","ο","α","β","γ","δ","ε","ζ","η","θ","ι","κ","λ","μ","ν","ξ","ο","π","ρ","σ","τ","υ","φ","χ","ψ","ω","a","b","c","d","e","f","να","ναι","μας","τετοιες","ήταν","ηταν","αυτο","ας","εγω","εχει","ή","η","εκεί","και","λίγο","λιγο","πάλι","μονο","απ","μόνο","αυτά","αυτή","αυτα","αυτη","εγώ","ούτε","υπάρχει","-","κάνει","στους","κάθε","πρέπει","τώρα","λέει","όχι","ήταν","amp","δύο","σαν","το","να","για","του","είναι","ειναι","στις","έχω","μετά","μη","κάτι","είσαι","πολύ","σήμερα","καλημέρα","όλα","ολα","όλοι","ολοι","όλες","ολες","πολυ","πολλή","πολλά","πολλη","πολλα","την","με","του","της","τα","που" , "στο","είναι", "θα", "τον","σε","από","απο", "μου","στην","οι", "τους","μας","τη", "των", "στη","στα","τις", "ότι","οτι", "σου","στον","αλλά","μια", "τι","αν","σας","έχει","ένα","αυτό","δε","όταν","κι", "γιατί", "πως","πιο", "μην", "έχουν", "ρε","μόνο")
  stop.words <- append(greek_stop_words , stopwords::stopwords(language = "en"))
  #removing urls
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  data<-gsub(pattern = url_pattern,"",data)
  #removing english strings
  data<-gsub("[a-z]|[A-Z]","",data)
  #removing emojis
  data<-gsub("<U|[$-_@.&+]|[0-9]|F|[0-9]>","",data)
  # Ngram and Network Analysis
  data<-gsub("ά","α",data)
  data<-gsub("έ","ε",data)
  data<-gsub("ή","η",data)
  data<-gsub("ί","ι",data)
  data<-gsub("ό","ο",data)
  data<-gsub("ύ","υ",data)
  data<-gsub("ώ","ω",data)
  data<-gsub("ϋ","υ",data)
  data<-gsub("ϊ","ι",data)
  data<-gsub("ΐ","ι",data)
  df<- tibble(txt = data)
  bi.gram.words <- df %>% 
    unnest_tokens(
      input = txt, 
      output = bigram, 
      token = 'ngrams', 
      n = 2
    ) %>% 
    filter(! is.na(bigram))
  bi.gram.words %>% 
    select(bigram) %>% 
    head(10)
  bi.gram.words %<>% 
    separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
    filter(! word1 %in% stop.words) %>% 
    filter(! word2 %in% stop.words) %>% 
    filter(! is.na(word1)) %>% 
    filter(! is.na(word2)) 
  bi.gram.count <- bi.gram.words %>% 
    count(word1, word2, sort = TRUE) %>% 
    # We rename the weight column so that the 
    # associated network gets the weights (see below).
    rename(weight = n)
  
  bi.gram.count %>% head()
  bi.gram.count %>% 
    ggplot(mapping = aes(x = weight)) +
    theme_light() +
    geom_histogram() +
    labs(title = "Bigram Weight Distribution")
  bi.gram.count %>% 
    mutate(weight = log(weight + 1)) %>% 
    ggplot(mapping = aes(x = weight)) +
    theme_light() +
    geom_histogram() +
    labs(title = "Bigram log-Weight Distribution")
  #IMPORTANT : 
  threshold <- 10
  
  ScaleWeight <- function(x, lambda) {
    x / lambda
  }
  
  network <-  bi.gram.count %>%
    filter(weight > threshold) %>%
    mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>% 
    graph_from_data_frame(directed = FALSE)
  
  network
  
  is.weighted(network)
  
  plot(
    network, 
    vertex.size = 1,
    vertex.label.color = 'black', 
    vertex.label.cex = 0.7, 
    vertex.label.dist = 1,
    edge.color = 'gray', 
    main = 'Bigram Count Network', 
    sub = glue('Weight Threshold: {threshold}'), 
    alpha = 50
  )
  
  # Store the degree.
  V(network)$degree <- strength(graph = network)
  
  # Compute the weight shares.
  E(network)$width <- E(network)$weight/max(E(network)$weight)
  
  plot(
    network, 
    vertex.color = 'lightblue',
    # Scale node size by degree.
    vertex.size = 2*V(network)$degree,
    vertex.label.color = 'black', 
    vertex.label.cex = 0.6, 
    vertex.label.dist = 1.6,
    edge.color = 'gray', 
    # Set edge width proportional to the weight relative value.
    edge.width = 3*E(network)$width ,
    main = 'Bigram Count Network', 
    sub = glue('Weight Threshold: {threshold}'), 
    alpha = 50
  )
  
  clusters(graph = network)
  
  V(network)$cluster <- clusters(graph = network)$membership
  
  cc.network <- induced_subgraph(
    graph = network,
    vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
  )
  
  cc.network 
  
  V(cc.network)$degree <- strength(graph = cc.network)
  
  E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)
  
  plot(
    cc.network, 
    vertex.color = 'lightblue',
    # Scale node size by degree.
    vertex.size = 10*V(cc.network)$degree,
    vertex.label.color = 'black', 
    vertex.label.cex = 0.6, 
    vertex.label.dist = 1.6,
    edge.color = 'gray', 
    # Set edge width proportional to the weight relative value.
    edge.width = 3*E(cc.network)$width ,
    main = 'Bigram Count Network (Biggest Connected Component)', 
    sub = glue('Weiight Threshold: {threshold}'), 
    alpha = 50
  )
  
  # Treshold
  threshold <- 10
  
  network <-  bi.gram.count %>%
    filter(weight > threshold) %>%
    graph_from_data_frame(directed = FALSE)
  
  # Store the degree.
  V(network)$degree <- strength(graph = network)
  # Compute the weight shares.
  E(network)$width <- E(network)$weight/max(E(network)$weight)
  
  # Create networkD3 object.
  network.D3 <- igraph_to_networkD3(g = network)
  # Define node size.
  network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
  # Degine color group (I will explore this feature later).
  network.D3$nodes %<>% mutate(Group = 1)
  # Define edges width. 
  network.D3$links$Width <- 10*E(network)$width
  
  forceNetwork(
    Links = network.D3$links, 
    Nodes = network.D3$nodes, 
    Source = 'source', 
    Target = 'target',
    NodeID = 'name',
    Group = 'Group', 
    opacity = 0.9,
    Value = 'Width',
    Nodesize = 'Degree', 
    # We input a JavaScript function.
    linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
    fontSize = 12,
    zoom = TRUE, 
    opacityNoHover = 1
  )
  
  # Treshold
  threshold <- 10
  
  network <-  bi.gram.count %>%
    filter(weight > threshold) %>%
    graph_from_data_frame(directed = FALSE)
  
  # Store the degree.
  V(network)$degree <- strength(graph = network)
  # Compute the weight shares.
  E(network)$width <- E(network)$weight/max(E(network)$weight)
  
  # Create networkD3 object.
  network.D3 <- igraph_to_networkD3(g = network)
  # Define node size.
  network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
  # Degine color group (I will explore this feature later).
  network.D3$nodes %<>% mutate(Group = 1)
  # Define edges width. 
  network.D3$links$Width <- 10*E(network)$width
  
  forceNetwork(
    Links = network.D3$links, 
    Nodes = network.D3$nodes, 
    Source = 'source', 
    Target = 'target',
    NodeID = 'name',
    Group = 'Group', 
    opacity = 0.9,
    Value = 'Width',
    Nodesize = 'Degree', 
    # We input a JavaScript function.
    linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
    fontSize = 12,
    zoom = TRUE, 
    opacityNoHover = 1
  )
  # Now we are going to skipgrams section
  skip.window <- 2
  
  skip.gram.words <- df %>% 
    unnest_tokens(
      input = txt, 
      output = skipgram, 
      token = 'skip_ngrams', 
      n = skip.window
    ) %>% 
    filter(! is.na(skipgram))
  
  skip.gram.words %>% 
    select(skipgram) %>% 
    slice(10:20)
  
  skip.gram.words$num_words <- skip.gram.words$skipgram %>% 
    map_int(.f = ~ ngram::wordcount(.x))
  
  skip.gram.words %<>% filter(num_words == 2) %>% select(- num_words)
  
  skip.gram.words %<>% 
    separate(col = skipgram, into = c('word1', 'word2'), sep = ' ') %>% 
    filter(! word1 %in% stop.words) %>% 
    filter(! word2 %in% stop.words) %>% 
    filter(! is.na(word1)) %>% 
    filter(! is.na(word2)) 
  
  skip.gram.count <- skip.gram.words  %>% 
    count(word1, word2, sort = TRUE) %>% 
    rename(weight = n)
  
  skip.gram.count %>% head()
  #Visualization skipgram analysis
  # Treshold
  threshold <- 10
  
  network <-  skip.gram.count %>%
    filter(weight > threshold) %>%
    graph_from_data_frame(directed = FALSE)
  
  # Select biggest connected component.  
  V(network)$cluster <- clusters(graph = network)$membership
  
  cc.network <- induced_subgraph(
    graph = network,
    vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
  )
  
  # Store the degree.
  V(cc.network)$degree <- strength(graph = cc.network)
  # Compute the weight shares.
  E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)
  
  # Create networkD3 object.
  network.D3 <- igraph_to_networkD3(g = cc.network)
  # Define node size.
  network.D3$nodes %<>% mutate(Degree = (1E-2)*V(cc.network)$degree)
  # Degine color group (I will explore this feature later).
  network.D3$nodes %<>% mutate(Group = 1)
  # Define edges width. 
  network.D3$links$Width <- 10*E(cc.network)$width
  
  forceNetwork(
    Links = network.D3$links, 
    Nodes = network.D3$nodes, 
    Source = 'source', 
    Target = 'target',
    NodeID = 'name',
    Group = 'Group', 
    opacity = 0.9,
    Value = 'Width',
    Nodesize = 'Degree', 
    # We input a JavaScript function.
    linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
    fontSize = 12,
    zoom = TRUE, 
    opacityNoHover = 1
  )
  #Node Importance
  node.impo.df <- tibble(
    word = V(cc.network)$name,  
    degree = strength(graph = cc.network),
    closeness = closeness(graph = cc.network), 
    betweenness = betweenness(graph = cc.network)
  )
  #Degree Centrality
  node.impo.df %>% 
    arrange(- degree) %>%
    head(10)
  #Closeness Centrality 
  node.impo.df %>% 
    arrange(- closeness) %>%
    head(10)
  #Betweenness Centrality
  node.impo.df %>% 
    arrange(- betweenness) %>% 
    head(10)
  #Let us see the distribution of these centrality measures.
  plt.deg <- node.impo.df %>% 
    ggplot(mapping = aes(x = degree)) +
    theme_light() +
    geom_histogram(fill = 'blue', alpha = 0.8, bins = 30)
  
  plt.clo <- node.impo.df %>% 
    ggplot(mapping = aes(x = closeness)) +
    theme_light() +
    geom_histogram(fill = 'red', alpha = 0.8, bins = 30)
  
  plt.bet <- node.impo.df %>% 
    ggplot(mapping = aes(x = betweenness)) +
    theme_light() +
    geom_histogram(fill = 'green4', alpha = 0.8, bins = 30)
  
  plot_grid(
    ... = plt.deg, 
    plt.clo, 
    plt.bet, 
    ncol = 1, 
    align = 'v'
  )
  #Community Detection
  #We can try to find clusters within the network. We use the Louvain Method for community detection:
  comm.det.obj <- cluster_louvain(
    graph = cc.network, 
    weights = E(cc.network)$weight
  )
  
  comm.det.obj
  V(cc.network)$membership <- membership(comm.det.obj)
  # We use the membership label to color the nodes.
  network.D3$nodes$Group <- V(cc.network)$membership
  
  forceNetwork(
    Links = network.D3$links, 
    Nodes = network.D3$nodes, 
    Source = 'source', 
    Target = 'target',
    NodeID = 'name',
    Group = 'Group', 
    opacity = 0.9,
    Value = 'Width',
    Nodesize = 'Degree', 
    # We input a JavaScript function.
    linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
    fontSize = 12,
    zoom = TRUE, 
    opacityNoHover = 1
  )
  
  membership.df <- tibble(
    word = V(cc.network) %>% names(),
    cluster = V(cc.network)$membership
  )
  
  V(cc.network)$membership %>%
    unique %>% 
    sort %>% 
    map_chr(.f = function(cluster.id) {
      
      membership.df %>% 
        filter(cluster == cluster.id) %>% 
        # Get 15 at most 15 words per cluster.
        slice(1:15) %>% 
        pull(word) %>% 
        str_c(collapse = ', ')
      
    }) 
  #Correlation Phi Coeeficient 
  stopwords.df <- tibble(word = stop.words)
  
  words.df <- df %>% 
    unnest_tokens(input = txt, output = word) %>% 
    anti_join(y = stopwords.df, by = 'word')
  
  cor.words <- words.df %>% 
    group_by(word) %>% 
    filter(n() > 10) %>% 
    pairwise_cor(item = word, feature = ID) 

}
