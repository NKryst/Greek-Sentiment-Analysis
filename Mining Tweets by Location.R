#Mining Twitter by Location
#Installing rtweet via GitHub
#library(devtools)
#devtools::install_github("mkearney/rtweet")
#install.packages("rworldmap")
#using the right libraries
library(rtweet)
register_google(key)
#Autorization for Twitter's  API
## access token method: create token and save it as an environment variable
create_token(
  app = "Your App's Name Here",
  consumer_key = "Consumer Key",
  consumer_secret = "Consumer Secret",
  access_token = "Access Token",
  access_secret = "Access Secret")

#Coordinates 10 Greek Cities
Cities<- c("Athens", "Thessaloniki", "Patras", "Heraklion", "Larissa","Volos", "Rhodes", "Ioannina","Chania","Agrinio")
Latitude <- c(37.983810,40.640064,38.246639,35.338734,39.638779,39.360519,36.434052,39.674530,35.513828,38.624474)
Longtitude <- c(23.727539,22.944420,21.734573,25.144213,22.415979,22.945320,28.217638,20.840210,24.018038,21.409594)
Top10<- data.frame(Cities, Latitude,Longtitude)
a<-list.files(path = paste(getwd(),"/Tweets TXT", sep = ""))
last_day<-a[[length(a)-1]]
cat("Το τελευταίο αρχείο με tweets είναι το εξής:", last_day)
print(Top10$Cities)
print("Δώσε αριθμό Πόλης , Αριθμό ημερών(<=7) και Μέγεθος Ακτίνας για τη συλλογή tweets")
u<-scan()
coords<-paste(Top10$Latitude[u[1]],Top10$Longtitude[u[1]],paste(u[3],"mi",sep = ""),sep = ",")
#User is picking his days
  if(u[2]==1){cat("Σε 1 λεπτό θα έχει ολοκληρωθεί η συλλογή")
  } else {
    cat("Ο χρόνος που θα χρειαστεί είναι περίπου:", (u[2]-1)*15+1, "λεπτά",Sys.Date(), sep = " ")
  }
  for(i in 1:as.integer(u[2])){
    #Specifying dates of mining
    since<-Sys.Date()-u[2]+i-1
    until <- since+1
    #Mining Tweets
    tweets <- rtweet::search_tweets('lang:el' ,  n = 18000, retryonratelimit = TRUE, include_rts = FALSE , geocode = coords, since = since, until = until )
    #Storing Tweets
    date()
    since<-gsub("-",".",since)
    write(tweets[[5]], file = paste(since, ".txt", sep = ""))
    write.csv2(tweets[[3]] , file = paste("timestamps_",since, ".csv", sep = ""))
    write.csv2(tweets[[5]] , file = paste(since, ".csv", sep = ""))
  }
  rm(u)
