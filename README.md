# Greek-Sentiment-Analysis

Greek-Sentiment-Analysis Lexicon Based
The following scripts was used to:

- Extract Sentiment of Tweets written to Greek Language
- Perform Network Analysis at words
- Get the most frequent words
 Get the geographic locations of Tweet

 In order to achieve Sentiment Analysis in Greek Language I used the Greek Sentiment Lexicon which you can also find in this repository
 <https://github.com/MKLab-ITI/greek-sentiment-lexicon>

## What you can find in this repository

- _The Scripts_
- _Some Sample files_ :
    1. sentiment analysis data.xlsx (**Data**)
    2. greek_stop_words.csv (**Stop Words**)
    3. emojis.csv (Actually didn't use them yet :smirk:)
- >The graphs

## Using the scripts to

- [x] Establish Connections to Twitter's API
- [x] Data Selection - Data Gathering
- [x] Cleaning - Integration - Storage
- [x] Feature Extraction
- [x] Knowledge extraction
- [x] Visualization

## Some Graph to check

### Anger

<img src="https://github.com/NKryst/Greek-Sentiment-Analysis/blob/master/Files/Graphs/Anger.PNG" width="700" height="500"> |

### Disgust

<img src="https://github.com/NKryst/Greek-Sentiment-Analysis/blob/master/Files/Graphs/Disgust.PNG" width="700" height="500">

### Fear

<img src="https://github.com/NKryst/Greek-Sentiment-Analysis/blob/master/Files/Graphs/Fear.PNG" width="700" height="500">

### Happiness

<img src="https://github.com/NKryst/Greek-Sentiment-Analysis/blob/master/Files/Graphs/Happiness.PNG" width="700" height="500">

### Sadness

<img src="https://github.com/NKryst/Greek-Sentiment-Analysis/blob/master/Files/Graphs/Sadness.PNG" width="700" height="500">

### Surprise

<img src="https://github.com/NKryst/Greek-Sentiment-Analysis/blob/master/Files/Graphs/Surprise.PNG" width="800" height="600">

# What you can find in this repository

- _The Scripts_
- _Some Sample files_ :
    1. sentiment analysis data.xlsx (**Data**)
    2. greek_stop_words.csv (**Stop Words**)
    3. emojis.csv (Actually didn't use them yet :smirk:)
- _>The graphs_

# Update 14/2/2021

There were some several additions regarding the following:

- _GreeK Sentiment Lexicon_ : Updating the Polarity Values ( NAs --> 0 , Both --> [average of total values if average = 0 then Both --> 0 , average<0 Both -->-1 ,average>0 Both -->1] new name **Fixed Sentiment Lexicon
- _Emojis Sentiment Lexicon_
- _Sentiment Analysis Revised_ (script) : taking under consideration all the values provided by the annotators of Greek Sentiment Lexicon

>>>>>>> master
