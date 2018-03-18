require(caret) 
require(e1071) 
tweetdat <- read.csv('GOP_10p_Updated_Human.csv', as.is = T);
attach(eddat)

head(tweetdat)

tweetdat$class <- as.factor(tweetdat$class)

dm <- cbind(tweetdat["class"], scale(tweetdat[,c(1,2,4,6,7,8,9,10,11,13,15,16,17,18,19,20,21,5,12,14,22)]), tweetdat["ask_comment"])

head(dm)




start_time <- Sys.time()
Accuracies <- c(0.00)

sink('sinktest.txt')
for (i in seq(100))
{
  
  inTrain <- createDataPartition(dm$class, p = .80, list = FALSE)
  dm.navg <- avNNet(dm[c(2:23)], dm$class, subset = inTrain, size = 2, rang= 0.5, decay = 5e-6, maxit = 200, repeats = 50)
  
  Accuracies[i] <- confusionMatrix(dm$class[-inTrain], predict(dm.navg, dm[-inTrain,], type = "class"))$overall["Accuracy"]
}

sink()
summary(Accuracies)


acclen = length(na.omit(Accuracies))
plot(density(Accuracies), main = paste('Average Neural Network Model accuracies:',acclen, 'runs') )