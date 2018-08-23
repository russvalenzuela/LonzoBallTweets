  options(java.parameters = "-Xmx8g")
  
  library(tm)
  library(ggplot2)
  library(wordcloud)
  library(RColorBrewer)
  library(RSentiment)
  
  #Remove Emojis
  LonzoTweets$text <- iconv(LonzoTweets$text, "latin1", "ASCII", sub="")
  
  
  #Create Corpus for LA Tweets
  LonzoTweets_corpus <- Corpus(VectorSource(LonzoTweets$text))
  
  #convert to lower case
  LonzoTweets_corpus <- tm_map(LonzoTweets_corpus, content_transformer(tolower))
  
  #remove URLs
  removeURL <-function(x) gsub("http(s?)[^[:space:]]*","",x)
  LonzoTweets_corpus <- tm_map(LonzoTweets_corpus, content_transformer(removeURL))
  
  #remove Mentions
  removeMention <-function(x) gsub("@\\w+","",x)
  LonzoTweets_corpus <- tm_map(LonzoTweets_corpus, content_transformer(removeMention))
  
  #remove anything other than English letters or space
  removeNumPunct <-function(x) gsub("[^[:alpha:][:space:]]*","",x)
  LonzoTweets_corpus <- tm_map(LonzoTweets_corpus, content_transformer(removeNumPunct))
  
  #Create document term matrix
  additional_stopwords <- c("lonzo","ball","lakers","los","angeles","shot","like","just",
                            "balls", "nba", "video")
  
  tdm_Lonzo <- TermDocumentMatrix(LonzoTweets_corpus, control=list(removePunctuation=TRUE,
                                                                   stopwords=c(additional_stopwords,stopwords("english")),
                                                                   removeNumbers=TRUE, tolower=TRUE))
  
  #Obtain words and frequencies
  m_Lonzo <- as.matrix(tdm_Lonzo)
  word_freqs_Lonzo <- sort(rowSums(m_Lonzo), decreasing = TRUE)
  dm_Lonzo <- data.frame(word=names(word_freqs_Lonzo), freq=word_freqs_Lonzo)
  
  #Plot WordCloud
  wordcloud(dm_Lonzo$word,dm_Lonzo$freq, scale=c(4,.2), max.words=50, random.order = FALSE, colors=brewer.pal(5,"Purples"))
  
  #Sentiment Analysis
  sentiments_Lonzo <- calculate_total_presence_sentiment(LonzoTweets$text)
  
  #Seperating Categories from Counts
  sentiment_cats_Lonzo <-sentiments_Lonzo[c(TRUE,FALSE)]
  sentiment_counts_Lonzo <-as.numeric(sentiments_Lonzo[c(FALSE,TRUE)])
  
  #Converting to DataFrame
  sentiments_Lonzo_df <- data.frame(sentiment_cats_Lonzo,sentiment_counts_Lonzo)
  sentiments_Lonzo_df$sentiment_cats_Lonzo <- as.character(sentiment_cats_Lonzo)
  sentiments_Lonzo_df$sentiment_cats_Lonzo <- factor(sentiments_Lonzo_df$sentiment_cats_Lonzo,
                                                     levels = unique(sentiments_Lonzo_df$sentiment_cats_Lonzo))
  
  #Graphing Sentiment Counts
  ggplot(sentiments_Lonzo_df, aes(sentiment_cats_Lonzo,sentiment_counts_Lonzo)) +
    geom_bar(stat = "identity", fill="#FDB927") +
    labs(x="Emotion", y="Number of Tweets") +
    ggtitle("Lonzo Ball Tweet Sentiment")
  
  View(sentiments_Lonzo_df)