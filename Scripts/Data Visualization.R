library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
mood_of_twitter <- read_excel("mood_of_twitter_revised.xlsx")
#View(mood_of_twitter)
#Setting the data
data_sentiment<-mood_of_twitter
colors<-data.frame("#FF0000","#32CD32","#000000", "#d1dd4a","#0000FF","#2243B6","#808000")
names(colors)<-c("Anger","Disgust","Fear","Happiness", "Sadness", "Surprise","Polarity")
#Missing Dates 
d <- as.Date(data_sentiment$Date)
date_range <- seq(min(d), max(d), by = 1) 
missing_dates<- date_range[!date_range %in% d] 
#Plot timeline data series with smoothing  
p <- ggplot(data = data_sentiment , aes(x = Date, y = Polarity))  
      geom_line(color = colors$Polarity, size = 1)
p + stat_smooth(color = colors$Polarity, fill = colors$Polarity,method = "loess")
  