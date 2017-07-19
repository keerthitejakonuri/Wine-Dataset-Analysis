install.packages("adabag")
install.packages("randomForest")
install.packages("rms")
install.packages("corrplot")


library(class)
library(adabag)
library(randomForest)
library(rms)
library(corrplot)
require(class)
require(rms)
require(randomForest)
require(adabag)
require(corrplot)


wineData<- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data',sep = ",");
dim(wineData);
names(wineData) = c("class","alcohol","malic_acid","ash","alcalinity","magnesium","total_phenols",
                    "flavanoids","nonflav_phenols","protho","color_intensity","hue","od280","proline");
rowsNum = nrow(wineData);
columnsNum = ncol(wineData);
classesInData = unique(wineData$class);
noOfClasses = length(classesInData);
thres = 0.1;

M <- cor(wineData)
corrplot(M,method = "number")

#Histogram plot of Class labels
hist(wineData$class,xlab="Class Values", border="blue", col="green");

#pre-processing, remove ash attribute as it is not correlated with class
wineData$ash=NULL;


#Function to find Accuracy
findAccuracy <- function(model, test_prov, c){
  accuracy=0;  
  
  if(c=='neuralnet' || c=='perceptron')
  {
    pred	<- compute(model,test_prov[,2:13])
    pred.scaled	<- pred$net.result
    real.values	<- (test_prov$class)
    accuracy <- (sum((real.values)==round(pred.scaled)))/length(real.values)*100
  }
  else
  {
    pred<-predict(model1,test_prov[,2:13],type = "class");
    accur <- sum(test_prov[,1]==pred)/length(pred)*100
  }
  
  return(accur)
}


#partioning the data into training and test
trainIndex	<- sample(1:nrow(wineData),	0.8 *	nrow(wineData))
train	<- wineData[trainIndex,	]
test	<- wineData[-trainIndex,	]
class = as.factor(train[,1])
train$class = factor(train$class)


createModelAndEval <- function(train1,test1)
{
  #logical regression
  n	<- names(train1)
  f	<- as.formula(paste("class	~",	paste(n[!n	%in%	"class"],	collapse	=	"	+	")))
  #model
  lrfit <-glm( class ~ ., data=train1,family = "quasibinomial")
  #prediction
  pred_value <- predict(lrfit,test1,type = c("response"));
  tabs <- table(test1[,1],pred_value)
  #accuracy
  accur <- sum(diag(tabs))/sum(tabs) * 100;
  print("accuracy of logical regression: ")
  print(accur);
  #finding precision
  dt_table <- table(test1[,1], pred_value); # build a table
  precision <- diag(dt_table) / rowSums(dt_table);
  macroPrecision = mean(precision)*100;
  print("precision of logical regression: ");
  print(macroPrecision);
  ######################################################
  
  #knn
  train_labels <- train1[1:nrow(train1), 1]
  test_labels <- test1[1:nrow(test1),1]
  pred_value <- knn(train = train1, test = test1,cl = train_labels, k=3)
  
  accur <- sum(test1[,1]==pred_value)/length(pred_value)*100
  print("accuracy of knn: ")
  print(accur);
  
  dt_table <- table(test1[,1], pred_value); # build a table
  precision <- diag(dt_table) / rowSums(dt_table);
  macroPrecision = mean(precision)*100;
  print("precision of knn: ");
  print(macroPrecision);
  #############
  
  
  
  #bagging
  train1$class = factor(train1$class)
  n	<- names(train1)
  f	<- as.formula(paste("class	~",	paste(n[!n	%in%	"class"],	collapse	=	"	+	")))
  bag_model = bagging(class ~., data = train1,mfinal=10,control=rpart.control(maxdepth=5, minsplit=15))
  pred_value <- predict(bag_model,test1)
  accur <- sum(test1[,1]==pred_value$class)/length(pred_value$class)*100
  print("accuracy of bagging: ")
  print(accur);
  
  dt_table <- table(test1[,1], pred_value$class); # build a table
  precision <- diag(dt_table) / rowSums(dt_table);
  macroPrecision = mean(precision)*100;
  print("precision of bagging: ");
  print(macroPrecision);
  ####
  
  
  #randomForest
  rf_model <- randomForest(class ~., data=train1, importance=TRUE,proximity=TRUE,maxnodes=6, ntree=40)
  pred_value <- predict(rf_model,test1)
  accur <- sum(test1[,1]==pred_value)/length(pred_value)*100
  print("accuracy of Random Forest: ")
  print(accur);
  
  dt_table <- table(test1[,1], pred_value); # build a table
  precision <- diag(dt_table) / rowSums(dt_table);
  macroPrecision = mean(precision)*100;
  print("precision of Random Forest: ");
  print(macroPrecision);
  ####
  
  
  #boosting
  boost_model <- bagging(class ~., data = train1,mfinal=10,boos=TRUE,control=rpart.control(maxdepth=5, minsplit=10))
  pred_value <- predict(boost_model,test1)
  accur <- sum(test1[,1]==pred_value$class)/length(pred_value$class)*100
  print("accuracy of boosting: ")
  print(accur);
  
  dt_table <- table(test1[,1], pred_value$class); # build a table
  precision <- diag(dt_table) / rowSums(dt_table);
  macroPrecision = mean(precision)*100;
  print("precision of boosting: ");
  print(macroPrecision);
  
  return;
}



n1=1
factor10val= nrow(wineData)/10;
for(i in 1:10)
{
  n2=i*floor(factor10val);
  #print(n1);
  #print(n2);
  test1	<- wineData[n1:n2,	]
  train1	<- wineData[-(n1:n2),	]
  class = as.factor(train1[,1])
  train1$class = factor(train1$class);
  createModelAndEval(train1,test1);
  print("--------------------");
  n1=n2+1;
}

