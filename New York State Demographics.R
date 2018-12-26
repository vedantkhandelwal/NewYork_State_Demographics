#####Reading the dataset
dataset<- read.delim("C:\\Users\\vedant khandelwal\\Downloads\\Project_4\\New_York.dat") 
data=dataset
str(dataset)
View(dataset)
##### there are 790 rows in our dataset
nrow(dataset)
#####this dataset contains huge outliers ,as there is a huge difference between mean and median so we need to treat these outliers 
summary(dataset)
#####dataset does not have any missing value 
sum(is.na(dataset$PLACE))
sum(is.na(dataset$TOT_POP))
sum(is.na(dataset$PCT_U18))
sum(is.na(dataset$PC_18_65))
sum(is.na(dataset$PCT_O65))
sum(is.na(dataset$MALE_FEM))
#####Visualization of data
library(mosaic)
library(manipulate)
mplot(dataset)
1
library(ggplot2) 
ggplot(data = ny, aes(y=ny$TOT_POP)) + geom_boxplot() + ggtitle("Boxplot of TOT_POP") 
ggplot(data = ny, aes(y=ny$PCT_U18)) + geom_boxplot() + ggtitle("Boxplot of PCT_U18") 
ggplot(data = ny, aes(y=ny$PC_18_65)) + geom_boxplot() + ggtitle("Boxplot of PC_18_65") 
ggplot(data = ny, aes(y=ny$PCT_O65)) + geom_boxplot() + ggtitle("Boxplot of PCT_O65") 
ggplot(data = ny, aes(y=ny$MALE_FEM)) + geom_boxplot() + ggtitle("Boxplot of MALE_FEM") 
library(psych) 
pairs.panels(ny[c("MALE_FEM","TOT_POP","PCT_U18","PC_18_65","PCT_O65")],method = "pearson",lm=TRUE, ellipses = FALSE) 
#####Treatment of Outliers
#####1st by removing them. 2nd by scaling them
#####1st By removing them

#####Total Population
summary(dataset$TOT_POP)
boxplot(dataset$TOT_POP,main="TOT_POP")$stats[c(1,5),]
#####Values which are less than 1000 and more than 19750 are outliers and they must be removed
library(sqldf)
#####not a single row is having value less than 1000
sqldf('select count(Tot_POP) from dataset where TOT_POP<1000')
#####81 rows contains value more than 19750
sqldf('select count(Tot_POP) from dataset where TOT_POP>19750')
#####removal of outlier values
dataset <- dataset[-which(dataset$TOT_POP>19750),] 
summary(dataset$TOT_POP) 
#####709 rows are left
nrow(dataset)

#####Population under 18
summary(dataset$PCT_U18)
boxplot(dataset$PCT_U18,main="PCT_U18")$stats[c(1,5),]
#####Values which are less than 15.1 and more than 33.5 are outliers and they must be removed
#####23 rows contains value less than 15.1
sqldf('select count(PCT_U18) from dataset where PCT_U18<15.1')
#####9 rows contains value more than 33.5
sqldf('select count(PCT_U18) from dataset where PCT_U18>33.5')
#####removal of outlier values
dataset <- dataset[-which(dataset$PCT_U18<15.1),]
dataset <- dataset[-which(dataset$PCT_U18>33.5),]
summary(dataset$PCT_U18) 
#####677 rows are left
nrow(dataset)

#####Population Between Age 18 and 65
summary(dataset$PC_18_65)
boxplot(dataset$PC_18_65,main="PC_18_65")$stats[c(1,5),]
#####Values which are less than 49.5 and more than 72.4 are outliers and they must be removed
#####2 rows contains value less than 49.5
sqldf('select count(PC_18_65) from dataset where PC_18_65<49.5')
#####4 rows contains value more than 33.5
sqldf('select count(PC_18_65) from dataset where PC_18_65>72.4')
#####removal of outlier values
dataset <- dataset[-which(dataset$PC_18_65<49.5),]
dataset <- dataset[-which(dataset$PC_18_65>72.4),]
summary(dataset$PCT_U18) 
#####671 rows are left
nrow(dataset)

#####Population Over age 65
summary(dataset$PCT_O65)
boxplot(dataset$PCT_O65,main="PCT_O65")$stats[c(1,5),]
#####Values which are less than 3.9 and more than 25.5 are outliers and they must be removed
#####not a single  row is having value less than 3.9
sqldf('select count(PCT_O65) from dataset where PCT_O65<3.9')
#####13 rows contains value more than 25.5
sqldf('select count(PCT_O65) from dataset where PCT_O65>25.5')
#####removal of outlier values
dataset <- dataset[-which(dataset$PCT_O65>25.5),]
summary(dataset$PCT_O65) 
#####658 rows are left
nrow(dataset)

#####Male ratio per 100 female
summary(dataset$MALE_FEM)
boxplot(dataset$MALE_FEM,main="MALE_FEM")$stats[c(1,5),]
#####Values which are less than 70.9 and more than 106.3 are outliers and they must be removed
#####2 rows is having value less than 70.9
sqldf('select count(MALE_FEM) from dataset where MALE_FEM<70.9')
#####6 rows contains value more than 106.3
sqldf('select count(MALE_FEM) from dataset where MALE_FEM>106.3')
#####removal of outlier values
dataset <- dataset[-which(dataset$MALE_FEM<70.9),]
dataset <- dataset[-which(dataset$MALE_FEM>106.3),]
summary(dataset$MALE_FEM) 
#####650 rows are left
nrow(dataset)

library(GGally)
ggcorr(dataset)
dataset <- dataset[,2:6] 
library(caTools) 
set.seed(123) 
#####Split of dataset into training dataset and test dataset 
split <- sample.split(dataset$MALE_FEM,SplitRatio = 0.8) 
training_set <- subset(dataset, split==TRUE) 
nrow(training_set) 
test_set <- subset(dataset, split == FALSE) 
nrow(test_set)
training_set[,2:5] <- scale(training_set[,2:5]) 
test_set[,2:5] <- scale(test_set[,2:5]) 

#####Model building using all variables
model1 <- lm(MALE_FEM ~ .,data = training_set) 
summary(model1) 
sigma(model1)/mean(training_set$MALE_FEM)*100 
y_pred1 = predict(model1,newdata=test_set) 
cor(y_pred1,test_set$MALE_FEM)*cor(y_pred1,test_set$MALE_FEM) 

##### Backward Elimination:Came to conclusion of removing Population over 65
step(model1,direction = 'backward')
model2=lm(formula = MALE_FEM ~ TOT_POP + PCT_U18 + PC_18_65, data = training_set)
summary(model2)
sigma(model2)/mean(training_set$MALE_FEM)*100 
y_pred2 = predict(model2,newdata=test_set) 
cor(y_pred2,test_set$MALE_FEM)*cor(y_pred2,test_set$MALE_FEM)
residuals = test_set$MALE_FEM-y_pred2 
ggplot() + aes(residuals)+ geom_histogram(binwidth=1, colour="black", fill="white") + 
  geom_density(aes(y=1*..count..)) +  
  ggtitle("Overlay Histogram of Residuals") +  
  xlab("Residuals") + 
  ylab("") 

# Homoscedasticity 
ggplot(data=NULL,aes(x=y_pred2,y=residuals))+geom_point() + 
  ggtitle("Predicted Values VS Residuals") + 
  ylab("Residuals") +  
  xlab("Predicted Values") 

#####Model building Excluding Total Population all variables
model3 <- lm(MALE_FEM ~PCT_U18 + PC_18_65 + PCT_O65,data = training_set) 
summary(model3) 
sigma(model3)/mean(training_set$MALE_FEM)*100 
y_pred3 = predict(model3,newdata=test_set) 
cor(y_pred3,test_set$MALE_FEM)*cor(y_pred3,test_set$MALE_FEM) 
