# install.packages('mlbench')
library(mlbench)
setwd('G:/DSP/LogisticRegressionPractice')
getwd()
data(BreastCancer, package = 'mlbench')
head(BreastCancer)

colSums(is.na(bc))  

bc = BreastCancer[complete.cases(BreastCancer),]
  
colSums(is.na(bc))  #No NA rows

dim(bc)
head(bc)
str(bc)   #Showing all the factor variables


bc = bc[,-1]    #Remove id column

#We need to convert all factors into numeric values except class as its dependent varible


for(i in 1:9)
{
  bc[,i] = as.numeric(as.character(bc[,i]))
  
}

str(bc)   #Showing all the numeric variables and class as factor as required


bc$Class <- ifelse(bc$Class == "malignant", 1, 0) #Converting into 0 & 1
bc$Class <- factor(bc$Class, levels = c(0, 1))


str(bc) 

table(bc$Class)

# install.packages('caret')
library(caret)

trainDataIndex = createDataPartition(bc$Class, p=0.7, list = F)

trainData = bc[trainDataIndex, ]
testData = bc[-trainDataIndex, ]

table(trainData$Class)

'%ni%' <- Negate('%in%')  # define 'not in' function

#There is approximately 2 times more benign samples.
#So lets downsample it using the downSample function from caret package.



# In Down sampling, the majority class is randomly down sampled to be of the same size as the smaller class.
# That means, when creating the training dataset,
# the rows with the benign Class will be picked fewer times during the random sampling.


#Downsampling:

set.seed(100)
down_train = downSample(x = trainData[, colnames(trainData) %ni% 'Class'],
                        y = trainData$Class)

table(down_train$Class)
head(down_train)  #Notice all the class are 0
tail(down_train)  #Notice all the class are 1


#Upsampling:

set.seed(100)

up_train = upSample(x = trainData[, colnames(trainData) %ni% 'Class'],
                    y = trainData$Class)

table(up_train$Class)
head(up_train)
tail(up_train)

# Similarly, in UpSampling, rows from the minority class, that is,
# malignant is repeatedly sampled over and over till it reaches the same size as the majority class (benign).


# Build Logit Model:

logit = glm(Class ~ ., family = 'binomial', data = down_train)

summary(logit)

install.packages('VIF')

library(car)
library(fmsb)



barplot(vif(logit))

steplogit = stepAIC(logit, direction = 'both')

summary(steplogit)


cor(trainData$Cl.thickness, trainData$Cell.shape)

# Predicting on test data
pred = predict(logit, testData)

y_pred_num = ifelse(pred>0.5, 1,0)
y_pred = factor(y_pred_num, levels = c(0,1))
y_act = testData$Class

actual_preds2 = data.frame(cbind(testData$Class, y_pred))  
cor(actual_preds2)

