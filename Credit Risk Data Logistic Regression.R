#####################################################################
####################### Classificaion Case Study ###################
#####################################################################

#In case of a classification problem:  the target variable is a categorical in  nature.
#We'll be doing Logistic Regression and Classification decision tree. 


# Step-1
# Identify the Problem Statement, What are you trying to solve?



# Step-2
# Identify the Target variable in the data, What value will be predicted?


# Step-3
#Loading the raw Data
CreditRiskData=read.csv('D:/Classroom/Stats + R/Logistic Regression Datasets/Material/CreditRiskData.csv',na.strings=c(""," ","NA","NULL"))
View(CreditRiskData)
dim(CreditRiskData)
class(CreditRiskData)
names(CreditRiskData)


# Step-4
# Exploring the dataset
#gives the descriptive statistics of the data
#what kind of variables are there?
str(CreditRiskData)
head(CreditRiskData,10)





###check if all the categorical variables are factor or not

factor_cols=c("GoodCredit","checkingstatus1" ,"history3","purpose4","savings6","employ7","installment8","status9","others10","residence11","property12" 
              ,"otherplans14","housing15","cards16","job17","liable18","tele19","foreign20")

for (cat_cols in factor_cols){
  CreditRiskData[ , cat_cols]=as.factor(CreditRiskData[ , cat_cols])
}

str(CreditRiskData)
############################################################

# Step-5
# Whether it is a Regression problem or Classification?
#Classification target variable is GoodCredit



# Step-6
# Checking and treating missing values

# Checking missing values
colSums(is.na(CreditRiskData))
#There is no missing values





# Step-8
# Explore each "Potential" predictor for distribution and Quality
############################################################

# Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("duration2","amount5","age13")

#Splitting the plot window into many parts
par(mfrow=c(2,2))

# library to generate professional colors
library(RColorBrewer) 

# looping to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(CreditRiskData[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}


############################################################
# Exploring MULTIPLE CATEGORICAL features
ColsForBar=c("GoodCredit","checkingstatus1" ,"history3","purpose4","savings6","employ7","installment8","status9","others10","residence11","property12" 
             ,"otherplans14","housing15","cards16","job17","liable18","tele19","foreign20")

#Splitting the plot window into many parts
par(mfrow=c(3,6))
# looping to create the Bar-Plots for each column

for (ColumnName in ColsForBar){
  barplot(table(CreditRiskData[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Paired"))
}


############################################################

# Step-9
# Visual Relationship between target variable and predictors

##for classification- dependent:categorical and predictor: categorical/continuous
# Categorical Vs Continuous --- Box Plot
# Categorical Vs Categorical -- Grouped Bar chart

############################################################
# Categorical Vs Continuous Visual analysis: Boxplot


par(mfrow=c(1,1))
boxplot(duration2~GoodCredit, data = CreditRiskData, col=brewer.pal(8,"Paired"))
boxplot(amount5~GoodCredit, data = CreditRiskData, col=brewer.pal(8,"Paired"))
boxplot(age13~GoodCredit, data = CreditRiskData, col=brewer.pal(8,"Paired"))








############################################################
############################################################
# Categorical Vs Categorical Visual analysis: Grouped Bar chart




ColsForBar=c( "checkingstatus1" ,"history3","purpose4","savings6","employ7","installment8","status9","others10","residence11","property12" 
              ,"otherplans14","housing15","cards16","job17","liable18","tele19","foreign20")
par(mfrow=c(3,6))
for (bar_cols in ColsForBar)
  {
  CrossTabResult= table(CreditRiskData[,c('GoodCredit',bar_cols)])
  print(paste("The Chi-squared test for",bar_cols))
  print(chisq.test(CrossTabResult))
  
  barplot(CrossTabResult,beside=T,col=c('Red','Green'),main=paste('Grouped Bar plot between GoodCredit and',bar_cols))
  
}

# Step-9
# Statistical Relationship between target variable (Categorical) and predictors

# Categorical Vs Continuous --- ANOVA
# Categorical Vs Categorical -- Chi-square test


# Continuous Vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)
# H0: Variables are NOT correlated
# Small P-Value <5%--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)
summary(aov(duration2~GoodCredit, data = CreditRiskData))
summary(aov(amount5~GoodCredit, data = CreditRiskData))
summary(aov(age13~GoodCredit, data = CreditRiskData))




#### Categorical Vs Categorical relationship strength: Chi-Square test
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)


##It takes crosstabulation as the input and gives you the result
options(scipen = 999)
ColsForBar=c( "checkingstatus1" ,"history3","purpose4","savings6","employ7","installment8","status9","others10","residence11","property12" 
              ,"otherplans14","housing15","cards16","job17","liable18","tele19","foreign20")
par(mfrow=c(3,6))
for (bar_cols in ColsForBar)
{
  CrossTabResult= table(CreditRiskData[,c('GoodCredit',bar_cols)])
  print(paste("The Chi-squared test for",bar_cols))
  print(chisq.test(CrossTabResult))
  
  
}
#removing useless columns based on p values
UselessColumns=c("installment8","residence11", 
                 "cards16","job17","liable18","tele19")
CreditRiskData[, UselessColumns]=NULL








############################################################



############################################################
# Step-10
InputData=CreditRiskData

# Specifying the Target Variable
TargetVariableName='GoodCredit'

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=InputData[, c(TargetVariableName)]
str(TargetVariable)

# Here I am Selecting all other columns as Predictors apart from target variable
#but based on EDA, you'll have to choose those columns which are important

PredictorVariables=InputData[, !names(InputData) %in% TargetVariableName]
str(PredictorVariables)


DataForML=data.frame(TargetVariable,PredictorVariables)
##make sure you look at the structure before running any classification algo
str(DataForML)
head(DataForML)

# Step-12
#############################################################################################
# Sampling | Splitting data into 70% for training 30% for testing
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)



#############################################################################################
#############################################################################################
# Creating Predictive models on training data to check the accuracy on test data
###### Logistic Regression #######

##we are predicting  based on all other variables
##glm() is used for wide variety of modeling activities. Logistic regression
#is one of the models that you can create using glm()
##in order to tell glm() that you have to perform logistic regression,
#you have to say family= 'binomial"


#Deviance is a measure of goodness of fit of a generalized linear model.
#Or rather, it's a measure of badness of fit: higher numbers indicate worse fit.
#The null deviance shows how well the response variable is predicted by a model 
#that includes only the intercept
nrow(DataForMLTrain)


#residual deviance has to be lesser than null deviance to have a good model




##Unlike R-squared- this AIC value is relative
#as you run different models you see how the AIC value is changing
#lower it is, better is the model

###if you see that AIC has increased then the model won't be effective
##you want the least possible value of AIC
#and finalize the model with least AIC



LR_Model_1=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                 duration2+
                 I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                 I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+I(purpose4=="A45")+I(purpose4=="A46")+I(purpose4=="A49")
               + amount5+
                 I(savings6=="A62")+ I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                 I(employ7=="A72")+I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                 I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+
                 I(property12=="A122")+I(property12=="A123")+I(property12=="A124")+
                 age13+
                 I(otherplans14=="A142")+I(otherplans14=="A143")+
                 I(housing15=="A152")+I(housing15=="A153")+
                 I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_1)

LR_Model_2=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                 duration2+
                 I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                 I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+I(purpose4=="A45")+I(purpose4=="A49")
               + amount5+
                 I(savings6=="A62")+ I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                 I(employ7=="A72")+I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                 I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+
                 I(property12=="A122")+I(property12=="A123")+I(property12=="A124")+
                 age13+
                 I(otherplans14=="A142")+I(otherplans14=="A143")+
                 I(housing15=="A152")+I(housing15=="A153")+
                 I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_2)


LR_Model_3=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                 duration2+
                 I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                 I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+I(purpose4=="A45")+I(purpose4=="A49")
               + amount5+
                 I(savings6=="A62")+ I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                 I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                 I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+
                 I(property12=="A122")+I(property12=="A123")+I(property12=="A124")+
                 age13+
                 I(otherplans14=="A142")+I(otherplans14=="A143")+
                 I(housing15=="A152")+I(housing15=="A153")+
                 I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_3)



LR_Model_4=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                 duration2+
                 I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                 I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+I(purpose4=="A45")+I(purpose4=="A49")
               + amount5+
                 I(savings6=="A62")+ I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                 I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                 I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+
                 I(property12=="A123")+I(property12=="A124")+
                 age13+
                 I(otherplans14=="A142")+I(otherplans14=="A143")+
                 I(housing15=="A152")+I(housing15=="A153")+
                 I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_4)

LR_Model_5=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                 duration2+
                 I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                 I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+I(purpose4=="A45")+I(purpose4=="A49")
               + amount5+
                 I(savings6=="A62")+ I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                 I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                 I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 I(others10=="A102")+I(others10=="A103")+
                 I(property12=="A123")+I(property12=="A124")+
                 age13+
                 I(otherplans14=="A143")+
                 I(housing15=="A152")+I(housing15=="A153")+
                 I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_5)


LR_Model_6=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                 duration2+
                 I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                 I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+I(purpose4=="A45")+I(purpose4=="A49")
               + amount5+
                 I(savings6=="A62")+ I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                 I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                 I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 
                 I(property12=="A123")+I(property12=="A124")+
                 age13+
                 I(otherplans14=="A143")+
                 I(housing15=="A152")+I(housing15=="A153")+
                 I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_6)


LR_Model_7=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                 duration2+
                 I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                 I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+I(purpose4=="A45")+I(purpose4=="A49")
               + amount5+
                 I(savings6=="A62")+ I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                 I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                 I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 
                 I(property12=="A123")+I(property12=="A124")+
                 age13+
                 I(otherplans14=="A143")+
                 
                 I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_7)


LR_Model_8=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                 duration2+
                 I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                 I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+I(purpose4=="A45")+I(purpose4=="A49")
               + amount5+
                 I(savings6=="A62")+ I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                 I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                 I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 
                 
                 age13+
                 I(otherplans14=="A143")+
                 
                 I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_8)


LR_Model_9=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                 duration2+
                 I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                 I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+I(purpose4=="A45")+I(purpose4=="A49")
               + amount5+
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                 I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                 I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 
                 
                 age13+
                 I(otherplans14=="A143")+
                 
                 I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_9)

LR_Model_10=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                 duration2+
                 I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                 I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A44")+I(purpose4=="A45")+I(purpose4=="A49")
               + amount5+
                 I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                 I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                 I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                 
                 
                 age13+
                 I(otherplans14=="A143")+
                 
                 I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_10)

LR_Model_11=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A45")+I(purpose4=="A49")
                + amount5+
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A73")+I(employ7=="A74")+I(employ7=="A75")+
                  I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                  
                  
                  age13+
                  I(otherplans14=="A143")+
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_11)

LR_Model_12=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A45")+I(purpose4=="A49")
                + amount5+
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A73")+I(employ7=="A74")+
                  I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                  
                  
                  age13+
                  I(otherplans14=="A143")+
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_12)

LR_Model_13=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A45")+I(purpose4=="A49")+
              
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A73")+I(employ7=="A74")+
                  I(status9=="A92")+I(status9=="A93")+I(status9=="A94")+
                  
                  
                  age13+
                  I(otherplans14=="A143")+
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_13)

LR_Model_14=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A45")+I(purpose4=="A49")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A73")+I(employ7=="A74")+
                  I(status9=="A93")+I(status9=="A94")+
                  
                  
                  age13+
                  I(otherplans14=="A143")+
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_14)


LR_Model_15=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A49")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A73")+I(employ7=="A74")+
                  I(status9=="A93")+I(status9=="A94")+
                  
                  
                  age13+
                  I(otherplans14=="A143")+
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_15)


LR_Model_16=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A410")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A49")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A73")+I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  age13+
                  I(otherplans14=="A143")+
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_16)


LR_Model_17=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A49")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A73")+I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  age13+
                  I(otherplans14=="A143")+
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_17)

LR_Model_18=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3== "A31")+(history3=="A32")+(history3=="A33")+(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A49")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A73")+I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  age13+
                  
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_18)


LR_Model_19=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  (history3=="A32")+(history3=="A33")+(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A49")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A73")+I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  age13+
                  
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_19)



LR_Model_20=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A49")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  age13+
                  
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_20)



LR_Model_21=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A42")+I(purpose4=="A43")+I(purpose4=="A49")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  
                  
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_21)

LR_Model_22=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A43")+I(purpose4=="A49")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  
                  
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_22)


LR_Model_23=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A13")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A43")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  
                  
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_23)



LR_Model_24=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A43")+
                  
                  I(savings6=="A63")+ I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  
                  
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_24)


LR_Model_25=glm(TargetVariable ~ I(checkingstatus1=="A12")+I(checkingstatus1=="A14")+
                  duration2+
                  I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A43")+
                  
                  I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  
                  
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_25)


LR_Model_26=glm(TargetVariable ~ I(checkingstatus1=="A14")+
                  duration2+
                  I(history3=="A32")+I(history3=="A33")+I(history3=="A34")+
                  I(purpose4=="A41")+I(purpose4=="A43")+
                  
                  I(savings6=="A64")+ I(savings6=="A65")+
                  I(employ7=="A74")+
                  I(status9=="A93")+
                  
                  
                  
                  
                  
                  I(foreign20=="A202"),data=DataForMLTrain, family='binomial')

summary(LR_Model_26)

nrow(DataForMLTrain)







# Checking Accuracy of model on Testing data
PredictionProb=predict(LR_Model_26,DataForMLTest,type = "response")
PredictionProb
install.packages('caret', dependencies = TRUE)
library(caret)

IterationData=data.frame(
  Threshold=numeric(0),
  Accuracy=numeric(0)
)


thresholds=seq(0.5,0.65,0.01)
for(i in thresholds){
  
  DataForMLTest$Prediction=ifelse(PredictionProb>i, 1, 0)
  DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
  AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")
  IterationData=rbind(IterationData,data.frame(
    Threshold=i,
    Accuracy=round(100 * AccuracyResults[['overall']][1])))
  
}


IterationData

DataForMLTest$Prediction=ifelse(PredictionProb>0.61, 1, 0)
DataForMLTest$Prediction=as.factor(DataForMLTest$Prediction)
head(DataForMLTest)


AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")


# Since AccuracyResults is a list of multiple items, fetching useful components only

AccuracyResults[['table']]
AccuracyResults[['byClass']]
AccuracyResults[['overall']][1]

print(paste('### Overall Accuracy of Logistic Reg Model is: ', round(100 * AccuracyResults[['overall']][1]) , '%'))


#############################################################################################
###### Ctree Decision Tree #######
library(party)






startTime=Sys.time()
DT_Model=ctree( TargetVariable~ . , data=DataForMLTrain)
DT_Model
plot(DT_Model)

endTime=Sys.time()
endTime-startTime


# Checking Accuracy of model on Testing data
DataForMLTest$Prediction =predict(DT_Model, DataForMLTest)
head(DataForMLTest)

# Creating the Confusion Matrix to calculate overall accuracy, precision and recall on TESTING data
library(caret)
AccuracyResults=confusionMatrix(DataForMLTest$Prediction, DataForMLTest$TargetVariable, mode = "prec_recall")

# Since AccuracyResults is a list of multiple items, fetching useful components only
AccuracyResults[['table']]
AccuracyResults[['byClass']]

print(paste('### Overall Accuracy of Ctree Model is: ', round(100 * AccuracyResults[['overall']][1]) , '%'))






