library(randomForest)

library(readxl)
LonzoTweetStats <- read_excel("~/Desktop/LonzoTweetStats.xlsx")

set.seed(2)

train=sample(1:nrow(LonzoTweetStats),54)

LonzoRF_pos <- randomForest(posTweets ~ onCourtCode + timeElapsed 
                            + PTS + AST + REB + STL + TO + BLK + pointDef + FGPCT + TPPCT,
                            data=LonzoTweetStats, subset = train, ntree=500)
LonzoRF_neg <- randomForest(negTweets ~ onCourtCode + timeElapsed 
                            + PTS + AST + REB + STL + TO + BLK + pointDef + FGPCT + TPPCT,
                            data=LonzoTweetStats, subset = train, ntree=500)
LonzoRF_vpos <- randomForest(vposTweets ~ onCourtCode + timeElapsed 
                            + PTS + AST + REB + STL + TO + BLK + pointDef + FGPCT + TPPCT,
                            data=LonzoTweetStats,subset = train, ntree=500)
LonzoRF_vneg <- randomForest(vnegTweets ~ onCourtCode + timeElapsed 
                            + PTS + AST + REB + STL + TO + BLK + pointDef + FGPCT + TPPCT,
                            data=LonzoTweetStats,subset = train, ntree=500)


print(LonzoRF_pos)
print(LonzoRF_neg)
print(LonzoRF_vpos)
print(LonzoRF_vneg)

varImpPlot(LonzoRF_pos, sort=TRUE)
plot(LonzoRF_pos)
varImpPlot(LonzoRF_neg, sort=TRUE)
plot(LonzoRF_neg)
varImpPlot(LonzoRF_vpos, sort=TRUE)
plot(LonzoRF_vpos)
varImpPlot(LonzoRF_vneg, sort=TRUE)
plot(LonzoRF_vneg)

oob.err_pos=double(11)
test.err_pos=double(11)

#Postive Tweets Errors
for(mtry in 1:11) 
{
  rf=randomForest(posTweets ~ onCourtCode + timeElapsed 
                  + PTS + AST + REB + STL + TO + BLK + pointDef + FGPCT + TPPCT, data=LonzoTweetStats,
                  subset = train, mtry=mtry, ntree=1000) 
  oob.err_pos[mtry] = rf$mse[500] #Error of all Trees fitted
  
  pred<-predict(rf,LonzoTweetStats[-train,]) #Predictions on Test Set for each Tree
  test.err_pos[mtry]= with(LonzoTweetStats[-train,], mean( (posTweets - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

matplot(1:mtry , cbind(oob.err_pos,test.err_pos), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

oob.err_neg=double(11)
test.err_neg=double(11)
title("Positive")

#Negative Tweets Errors
for(mtry in 1:11) 
{
  rf=randomForest(negTweets ~ onCourtCode + timeElapsed 
                  + PTS + AST + REB + STL + TO + BLK + pointDef + FGPCT + TPPCT, data=LonzoTweetStats,
                  subset = train, mtry=mtry, ntree=1000) 
  oob.err_neg[mtry] = rf$mse[500] #Error of all Trees fitted
  
  pred<-predict(rf,LonzoTweetStats[-train,]) #Predictions on Test Set for each Tree
  test.err_neg[mtry]= with(LonzoTweetStats[-train,], mean( (negTweets - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

matplot(1:mtry , cbind(oob.err_neg,test.err_neg), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
title("Negative")


oob.err_vpos=double(11)
test.err_vpos=double(11)

#Very Postive Tweets Errors
for(mtry in 1:11) 
{
  rf=randomForest(vposTweets ~ onCourtCode + timeElapsed 
                  + PTS + AST + REB + STL + TO + BLK + pointDef + FGPCT + TPPCT, data=LonzoTweetStats,
                  subset = train, mtry=mtry, ntree=1000) 
  oob.err_vpos[mtry] = rf$mse[500] #Error of all Trees fitted
  
  pred<-predict(rf,LonzoTweetStats[-train,]) #Predictions on Test Set for each Tree
  test.err_vpos[mtry]= with(LonzoTweetStats[-train,], mean( (vposTweets - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

matplot(1:mtry , cbind(oob.err_vpos,test.err_vpos), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
title("Very Positive")


oob.err_vneg=double(11)
test.err_vneg=double(11)

#Very Negative Tweets Errors
for(mtry in 1:11) 
{
  rf=randomForest(vnegTweets ~ onCourtCode + timeElapsed 
                  + PTS + AST + REB + STL + TO + BLK + pointDef + FGPCT + TPPCT, data=LonzoTweetStats,
                  subset = train, mtry=mtry, ntree=1000) 
  oob.err_vneg[mtry] = rf$mse[500] #Error of all Trees fitted
  
  pred<-predict(rf,LonzoTweetStats[-train,]) #Predictions on Test Set for each Tree
  test.err_vneg[mtry]= with(LonzoTweetStats[-train,], mean( (vnegTweets - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

matplot(1:mtry , cbind(oob.err_vneg,test.err_vneg), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
title("Very Negative")