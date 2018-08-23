library(ggplot2)
library(readxl)

LonzoTweetStats <- read_excel("~/Desktop/LonzoTweetStats.xlsx")

qplot(LonzoTweetStats$posTweets, geom="histogram", binwidth=.75,
      main="Histogram for Postive Tweets", xlab="Postive Tweets",
      fill=I("#552583"))

qplot(LonzoTweetStats$negTweets, geom="histogram", binwidth=.75,
      main="Histogram for Negative Tweets", xlab="Negative Tweets",
      fill=I("#552583"))

qplot(LonzoTweetStats$vposTweets, geom="histogram", binwidth=.75,
      main="Histogram for Very Postive Tweets", xlab="Very Postive Tweets",
      fill=I("#552583"))

qplot(LonzoTweetStats$vnegTweets, geom="histogram", binwidth=.75,
      main="Histogram for Very Negative Tweets", xlab="Very Negative Tweets",
      fill=I("#552583"))

ggplot(LonzoTweetStats, aes(timeElapsed,posTweets, color=factor(opp))) + 
  geom_line() + theme(legend.title=element_blank()) +
  scale_colour_manual(values=c("#5091CD", "#FFCD34","#CE1141"), name="",
                      guide=guide_legend(nrow=4)) +
  xlab('Time Elpased') +
  ylab('Postive Tweets')

ggplot(LonzoTweetStats, aes(timeElapsed,negTweets, color=factor(opp))) + 
  geom_line() + theme(legend.title=element_blank()) +
  scale_colour_manual(values=c("#5091CD", "#FFCD34","#CE1141"), name="",
                      guide=guide_legend(nrow=4)) +
  xlab('Time Elpased') +
  ylab('Negative Tweets')

ggplot(LonzoTweetStats, aes(timeElapsed,vposTweets, color=factor(opp))) + 
  geom_line() + theme(legend.title=element_blank()) +
  scale_colour_manual(values=c("#5091CD", "#FFCD34","#CE1141"), name="",
                      guide=guide_legend(nrow=4)) +
  xlab('Time Elpased') +
  ylab('Very Postive Tweets')

ggplot(LonzoTweetStats, aes(timeElapsed,vnegTweets, color=factor(opp))) + 
  geom_line() + theme(legend.title=element_blank()) +
  scale_colour_manual(values=c("#5091CD", "#FFCD34","#CE1141"), name="",
                      guide=guide_legend(nrow=4)) +
  xlab('Time Elpased') +
  ylab('Very Negative Tweets')

