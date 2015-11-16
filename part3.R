setwd("~/Dropbox/Data_Science/R Projects/email project/")
library(rpart)
library(ggplot2)
load('emailFrame.rda')
############################################### From Trevot
library(rattle)
library(rpart.plot)
library(RColorBrewer)
##############################################################
#set class
#function used to permuate and create 10 subgroups for 10 fold cross valdiation
setTest = function(dat, nset = 10){
  len = length(dat)
  permuate = sample(1:len)  
  split(permuate, 1:nset)
}


##############################################################
#the dataset has some structure (due to data collected from different folders)
#lets shuffle the data before starting our analysis.
shuffleRows = sample(1:nrow(emailFrame))
emailFrame = emailFrame[shuffleRows,] 

#now lets gets get a random subset of size = nrow(data Frame)/10
#and lets store those away in order to test our classification model later
#this allows us to not overfit our model in our data.

#shuffle data in order to permuate and store "test data"
shuffleSet = setTest(1:nrow(emailFrame))
TestIndex = shuffleSet[[1]]

#This function, will be used to create a logistic reression
#then use a decision tree to decide the cut off of the probability, 
#for classificaton purposes.
#this function will also test our overall result of our data, by returning
#the ratio of currect classification 
  #(1 = 100% of correct classification of data, 0 = 0% of correct classification of data)
LogTree = function(TestData, DatFrame){
  ###################################################################
  #logistic Regression:
  TrainFrame = na.omit(DatFrame[-TestData,])
  fitLog = glm(isSpam~., data = TrainFrame, family = binomial("logit"))
  
  #Decision Tree
  newFrame = data.frame(isSpam = TrainFrame$isSpam, fitValues = fitLog$fitted.values)
  decisionTree = rpart(isSpam~., data = newFrame,method = 'class')
  ###########################################################################
  #testing Data.frame
  TestFrame = na.omit(DatFrame[TestData,])
  newFrameTest = TestFrame[,-1]
  
  #predict using logistic Regression
  fitPredLog = predict(fitLog, newdata = newFrameTest, type = 'response')
  
  #Predict using classificaiton Tree
  fitPredData = data.frame(fitValues = fitPredLog)
  finalPredTree = predict(decisionTree, newdata = fitPredData, type = 'class')
  
  #Probability of correct Classification
  ProbClass = c(sum(finalPredTree == TestFrame$isSpam))/nrow(TestFrame)
  
  #confusion matrix
  ConfTable = table(real = TestFrame$isSpam, pred = finalPredTree) 
  
  return(list(ProbCorrect = ProbClass, ConfusionMatrix = ConfTable,
              LogisticModel = fitLog, DecisionTree = decisionTree,
              ModelTestData = TestFrame, ModelTrainData = TrainFrame,
              PredValuesTree = finalPredTree, PredValuesLog = fitPredData)) 
}


#lets remove the sampleTest data from that was created above, to test our model later
TrainSample = emailFrame[-TestIndex,]
#lets permuate and create 10 subgroups of similar size, that represnts in total
#every email, however each email can only be in one of these 10 subgroups.
#in order to have every email be used as a test value ONCE.

#in other words, 
  #Each 10 subgroups contains all contain unique values with respect to every subgroup.
  #Each value represents a specific email, and every email (not in testSubset)
  #will be in one and only one of these sub groups
newShuffleSet = setTest(1:nrow(TrainSample))
#we are doing 10 fold cross validation becasue we choose to create 10 subgroups
#in other words:
  #each subgroup (each with unique values from each other), will be used once as a testset
  #and the for each fold, in the 10 folds CV, the 9 other subgroups not being used as a 
  #test set, will be used as training data, to trian the logistic and Classification Tree.
  #For Classification
CV10Folds = lapply(newShuffleSet, function(x) LogTree(x, TrainSample))


#######################################################
#Collecting Correct Classification Ratio for each fold in 10 folds Cross Validation
#(1 = 100% Correct Classification, 0 = 0% correct Classification)
getCorrRatio = sapply(CV10Folds, function(x) x$ProbCorrect)
getCorrRatio
#Top will mean, best classifier. In other words, the model that classified the most
#number of emais

#getting top classifer of the 10 'different' models created from our 10 Folds CV
IndexTopProb = order(getCorrRatio, decreasing = T)[1]



#Get 'Top' Values:
#Correct Classification ratio (1 = 100%, 0 = 0%)
TopClassRatio = getCorrRatio[IndexTopProb]
TopClassRatio
#obtain Logistic Regression Model from our 'Top' classification
TopLogModel = CV10Folds[[IndexTopProb]]$LogisticModel
TopLogModel
#obtain 'top' Decision Tree
TopDecisionTree = CV10Folds[[IndexTopProb]]$DecisionTree
TopDecisionTree
#obtain real value (isSpam), from the training dataset for the 2 models 
#(logistic and decision tree) used to create 'top' classifier
TrueTrainValue = CV10Folds[[IndexTopProb]]$ModelTrainData$isSpam

#Confusion Matrix for the classification of the lowest misclassication dataset
ConfMatrixResult = CV10Folds[[IndexTopProb]]$ConfusionMatrix
ConfMatrixResult
############################################################
#Why did I use Logistic Regression
ggplotDataFrame = data.frame(logFitValues = TopLogModel$fitted.values,
                             realTrainValues = TrueTrainValue)


ggplotTheme = theme(axis.text = element_text(colour = 'black',face = c('bold'),size = 9),
                    title = element_text(face = 'bold',family = 'Helvetica'))
qq = ggplot(ggplotDataFrame, aes(1:nrow(ggplotDataFrame),logFitValues, 
                                 colour = realTrainValues))
qq+geom_point()+ggplotTheme+labs(title = 'Logistic Regression Fitted Values',
                                 y = 'Fitted Values', x= 'Email')


#######How Classification Tree would help?
#Plot Idea from  trevor Stephens (Data Scientist)
fancyRpartPlot(TopDecisionTree, main = 'Classification Tree: Spam vs. Ham Classification')
#########################################################################
#now lets actually use our TOP model to classify data not used in training or test set
#in the Cross validation test

#fitting the values of our testsample using Logistic Regression
TestSample = na.omit(emailFrame[TestIndex,])
NewTestSample = TestSample[,-1]
TestPredLog = predict(TopLogModel, newdata = NewTestSample, type = 'response')

#classifying our testsample using Decision Tree
FitTestFrame = data.frame(fitValues = TestPredLog)
TestPredTree = predict(TopDecisionTree, newdata = FitTestFrame, type = 'class')

#result:
#how well did our model perform?
#getting the ratio of sum(correct classification)
#over all sample in testSample
TestPerformance = sum(TestSample$isSpam ==  TestPredTree)/length(TestSample$isSpam)
TestPerformance
#Confusion Matrix, for correct classiication and miscalssification for TRUE and FALSE
#values in testSample
confMatrixTest = table(real = TestSample$isSpam, pred= TestPredTree)
confMatrixTest



############# Vizualize Confusion Matrix

#this function will allow us to change Freq of each eleemnt in confusion Matrix
#into a ratio, with respect to its peers (Spam or ham).
#For Example: 
  #sum(Correct classifying Spam in test sample) over number of spam email in test sample
#another example:
  #sum(missclassification of Ham email) over number of ham email in test sample.
ConfusionManipulation = function(confMat){
  ConfMat = data.frame(confMat)
  y1 = ConfMat$real == F
  y2 = ConfMat$real == T
  sumFALSE = sum(ConfMat$Freq[y1])
  sumTRUE = sum(ConfMat$Freq[y2])
  
  
  ConfMat$Freq[y1] = ConfMat$Freq[y1]/sumFALSE
  ConfMat$Freq[y2] = ConfMat$Freq[y2]/sumTRUE
  ConfMat
  
}
#Freq is no longer Freq, but a ratio (1 = 100%, 0 = 0%)
newConfMat = ConfusionManipulation(confMatrixTest)
ggplotTheme2 = theme(axis.text = element_text(colour = 'black',face = c('bold'),size = 9),
                    title = element_text(face = 'bold',family = 'Helvetica'),
                    axis.text.x = element_text(colour = 'red'))


#the Ratio will the Predicted Classificaiton (is the email Spam) from 0 to 1,
  #with respect to the Real Value (is the email Spam).
#in other words, for all email that the real value is Spam (TRUE), what is the proportion
#that the predicted classification was spam (TRUE) and 
#what is the proportion that the  predicted classification was ham (FALSE)

#so you compare the red intensity (how red is the box) veritically. 
pp = ggplot(newConfMat, aes(real, pred,fill = Freq))
pp +geom_tile()+scale_fill_gradient2(high = 'red')+ggplotTheme2+
  guides(fill=guide_legend(title="Ratio w.r.t Real"))+
  labs(title = "Confusion Matrix: Spam Classification Performance",
       x = 'Real Value (is the email Spam?)', y = 'Predicted Classification of email')
