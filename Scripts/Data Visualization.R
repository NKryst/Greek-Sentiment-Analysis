library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
#importing data
mood_of_twitter <- read_excel("mood_jw.xlsx")
View(mood_of_twitter)
colours<-data.frame("#FF0000","#32CD32","#000000", "#d1dd4a","#0000FF","#2243B6")
names(colours)<-c("Anger","Disgust","Fear","Happiness", "Sadness", "Surprise")
#Setting the data
data_sentiment<-mood_of_twitter
#Plot timeline data series with smoothing  
p <- ggplot(data = data_sentiment , aes(x = Date, y = Sadness))  
      geom_line(color = colours$Sadness[[1]], size = 1)
p + stat_smooth(color = colours$Sadness[[1]], fill = colours$Sadness[[1]],method = "loess")