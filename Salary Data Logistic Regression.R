
#####################################################################
####################### Classificaion Case Study ###################
#####################################################################

#In case of a classification problem:  the target variable is a categorical in  nature.
#We'll be doing Logistic Regression and Classification decision tree. 


# Step-1
# Identify the Problem Statement, What are you trying to solve?
#here the ask is:



# Step-2
# Identify the Target variable in the data, What value will be predicted?


# Step-3
#Loading the raw Data

SalaryData = read.csv('D:/Classroom//Stats + R/Logistic Regression Datasets/Material/SalaryData.csv',na.strings=c(""," ","NA","NULL"))

View(SalaryData)
dim(SalaryData)
class(SalaryData)
names(SalaryData)


# Step-4
# Exploring the dataset
#gives the descriptive statistics of the data
#what kind of variables are there?
str(SalaryData)
head(SalaryData,10)




# Removing useless columns in the data, and explore the rest
UselessColumns=c()
SalaryData[, UselessColumns]=NULL




###check if all the categorical variables are factor or not

factor_cols=c("workclass","education","marital_status","occupation","race",          
                "sex","native_country","SalaryGT50K")
SalaryData$occupation

for (cat_cols in factor_cols){
  SalaryData[ , cat_cols]=as.factor(SalaryData[ , cat_cols])
}

str(SalaryData)

############################################################

# Step-5
# Whether it is a Regression problem or Classification?
#Classification

# Step-6
# Checking and treating missing values

# Checking missing values
colSums(is.na(SalaryData))

FunctionMode=function(inpData){
  ModeValue=names(table(inpData)[table(inpData)==max(table(inpData))])
  return(ModeValue)
}

FunctionMode(SalaryData$workclass)
FunctionMode(SalaryData$occupation)
FunctionMode(SalaryData$native_country)


SalaryData$workclass[is.na(SalaryData$workclass)] = "Private"
SalaryData$occupation[is.na(SalaryData$occupation)] = "Exec-managerial"
SalaryData$native_country[is.na(SalaryData$native_country)] = "United-States"
# Checking missing values after treatment
colSums(is.na(SalaryData))

# Step-8
# Explore each "Potential" predictor for distribution and Quality
############################################################

# Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("age","hours_per_week")

#Splitting the plot window into many parts
par(mfrow=c(2,1))

# library to generate professional colors
library(RColorBrewer) 

# looping to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(SalaryData[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}
#Salary Data Clubbing values inside the columns based on logic


SalaryData$native_country=as.character(SalaryData$native_country)
south_america = c("Columbia", "Ecuador", "Peru")

north_america =c("Canada", "Cuba", "Dominican-Republic", "El-Salvador", "Guatemala",
                 "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                 "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "Trinadad&Tobago",
                 "United-States")

europe =c("England", "France", "Germany", "Greece",
"Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland",
"Yugoslavia")

asia = c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
         "Philippines", "Taiwan", "Thailand", "Vietnam")

SalaryData$native_country[SalaryData$native_country %in% south_america] = "South-America" 
SalaryData$native_country[SalaryData$native_country %in% north_america] = "North-America"
SalaryData$native_country[SalaryData$native_country %in% europe] = "Europe"
SalaryData$native_country[SalaryData$native_country %in% asia] = "Asia"
table(SalaryData$native_country)


SalaryData$native_country=as.factor(SalaryData$native_country)

str(SalaryData)
############################################################
# Exploring MULTIPLE CATEGORICAL features
ColsForBar=c("workclass","education","marital_status","occupation","race",          
             "sex","native_country","SalaryGT50K")

#Splitting the plot window into many parts
par(mfrow=c(2,4))

# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(SalaryData[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
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

##Age: continuous, Survived: Categorical
par(mfrow=c(2,1))
boxplot(age~SalaryGT50K, data = SalaryData, col=brewer.pal(8,"Paired"))

boxplot(hours_per_week~SalaryGT50K, data = SalaryData, col=brewer.pal(8,"Paired"))

############################################################
############################################################
# Categorical Vs Categorical Visual analysis: Grouped Bar chart

ColsForBar=c("workclass","education","marital_status","occupation","race",          
             "sex","native_country" )
par(mfrow=c(2,4))
for (bar_cols in ColsForBar)
{
  CrossTabResult= table(SalaryData[,c('SalaryGT50K',bar_cols)])
  print(paste("The Chi-squared test for",bar_cols))
  print(chisq.test(CrossTabResult))
  
  barplot(CrossTabResult,beside=T,col=c('Red','Green'),main=paste('Grouped Bar plot between SalaryGT50K and',bar_cols))
  
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

summary(aov(age~SalaryGT50K, data = SalaryData))

summary(aov(hours_per_week~SalaryGT50K, data = SalaryData))


#### Categorical Vs Categorical relationship strength: Chi-Square test
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)


##It takes crosstabulation as the input and gives you the result
options(scipen = 999)
ColsForCross=c("workclass","education","marital_status","occupation","race",          
             "sex","native_country" )

par(mfrow=c(3,6))
for (bar_cols in ColsForCross)
{
  CrossTabResult= table(SalaryData[,c('SalaryGT50K',bar_cols)])
  print(paste("The Chi-squared test for",bar_cols))
  print(chisq.test(CrossTabResult))
  
  
}

############################################################
# Step-10
InputData=SalaryData

# Specifying the Target Variable
TargetVariableName='SalaryGT50K'

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








############################################################



########################################################
# Creating Predictive models on training data to check the accuracy on test data
###### Logistic Regression #######

##we are predicting TV based on all other variables
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


LR_Model1_1=glm(TargetVariable
                ~age+
        I(workclass=="Jobless")+I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
   I(education=="11th")+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
   I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Preschool")+
   I(education=="Pros-school")+
   I(marital_status=="Married")+I(marital_status=="Never-married")+I(marital_status=="Separated")+I(marital_status=="Widowed")+
     I(occupation=="Armed-Forces ")+I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
   I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+
   I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
    I(race=="Asian-Pac-Islander")+I(race=="Other")+I(race=="Black")+I(race=="White")+
     I(sex=="Male")+
     hours_per_week+
     I(native_country=="europe")+I(native_country=="north-America")+I(native_country=="south-America")
   ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_1)


LR_Model1_2=glm(TargetVariable
                ~age+
                  I(workclass=="Jobless")+I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                  I(education=="11th")+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                  I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Preschool")+
                
                  I(marital_status=="Married")+I(marital_status=="Never-married")+I(marital_status=="Separated")+I(marital_status=="Widowed")+
                  I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                  I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+
                  I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
                  I(race=="Asian-Pac-Islander")+I(race=="Other")+I(race=="Black")+I(race=="White")+
                  I(sex=="Male")+
                  hours_per_week+
                  I(native_country=="europe")+I(native_country=="north-America")
                ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_2)

LR_Model1_3=glm(TargetVariable
                ~age+
                  I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                  I(education=="11th")+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                  I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+I(education=="Preschool")+
                  
                  I(marital_status=="Married")+I(marital_status=="Never-married")+I(marital_status=="Separated")+I(marital_status=="Widowed")+
                  I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                  I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+
                  I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
                  I(race=="Asian-Pac-Islander")+I(race=="Other")+I(race=="Black")+I(race=="White")+
                  I(sex=="Male")+
                  hours_per_week+
                  I(native_country=="europe")+I(native_country=="north-America")
                ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_3)

LR_Model1_4=glm(TargetVariable
                ~age+
                  I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                  I(education=="11th")+I(education=="12th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                  I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                  
                  I(marital_status=="Married")+I(marital_status=="Never-married")+I(marital_status=="Separated")+I(marital_status=="Widowed")+
                  I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                  I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+
                  I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
                  I(race=="Asian-Pac-Islander")+I(race=="Other")+I(race=="Black")+I(race=="White")+
                  I(sex=="Male")+
                  hours_per_week+
                  I(native_country=="europe")+I(native_country=="north-America")
                ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_4)


LR_Model1_5=glm(TargetVariable
                ~age+
                  I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                  I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                  I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                  
                  I(marital_status=="Married")+I(marital_status=="Never-married")+I(marital_status=="Separated")+I(marital_status=="Widowed")+
                  I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                  I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+
                  I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
                  I(race=="Asian-Pac-Islander")+I(race=="Other")+I(race=="Black")+I(race=="White")+
                  I(sex=="Male")+
                  hours_per_week+
                  I(native_country=="europe")+I(native_country=="north-America")
                ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_5)

LR_Model1_6=glm(TargetVariable
                ~age+
                  I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                  I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                  I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                  
                  I(marital_status=="Married")+I(marital_status=="Never-married")+I(marital_status=="Separated")+
                  I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                  I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+
                  I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
                  I(race=="Asian-Pac-Islander")+I(race=="Other")+I(race=="Black")+I(race=="White")+
                  I(sex=="Male")+
                  hours_per_week+
                  I(native_country=="europe")+I(native_country=="north-America")
                ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_6)

LR_Model1_7=glm(TargetVariable
                ~age+
                  I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                  I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                  I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                  
                  I(marital_status=="Married")+I(marital_status=="Never-married")+I(marital_status=="Separated")+
                  I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                  I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+
                  I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
                  I(race=="Other")+I(race=="Black")+I(race=="White")+
                  I(sex=="Male")+
                  hours_per_week+
                  I(native_country=="europe")+I(native_country=="north-America")
                ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_7)


LR_Model1_8=glm(TargetVariable
                ~age+
                  I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                  I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                  I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                  
                  I(marital_status=="Married")+I(marital_status=="Never-married")+I(marital_status=="Separated")+
                  I(occupation=="Craft-repair")+I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                  I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+
                  I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
                  I(race=="Other")+I(race=="Black")+I(race=="White")+
                  I(sex=="Male")+
                  hours_per_week+
                  I(native_country=="europe")
                ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_8)

LR_Model1_9=glm(TargetVariable
                ~age+
                  I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                  I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                  I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                  
                  I(marital_status=="Married")+I(marital_status=="Never-married")+I(marital_status=="Separated")+
                  I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                  I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+
                  I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
                  I(race=="Other")+I(race=="Black")+I(race=="White")+
                  I(sex=="Male")+
                  hours_per_week+
                  I(native_country=="europe")
                ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_9)


LR_Model1_10=glm(TargetVariable
                ~age+
                  I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                  I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                  I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                  
                  I(marital_status=="Married")+I(marital_status=="Never-married")+
                  I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                  I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+I(occupation=="Priv-house-serv")+
                  I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
                  I(race=="Other")+I(race=="Black")+I(race=="White")+
                  I(sex=="Male")+
                  hours_per_week+
                  I(native_country=="europe")
                ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_10)


LR_Model1_11=glm(TargetVariable
                 ~age+
                   I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                   I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                   I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                   
                   I(marital_status=="Married")+I(marital_status=="Never-married")+
                   I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                   I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+
                   I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+I(occupation=="Transport-moving")+
                   I(race=="Other")+I(race=="Black")+I(race=="White")+
                   I(sex=="Male")+
                   hours_per_week+
                   I(native_country=="europe")
                 ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_11)


LR_Model1_12=glm(TargetVariable
                 ~age+
                   I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                   I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                   I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                   
                   I(marital_status=="Married")+I(marital_status=="Never-married")+
                   I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                   I(occupation=="Handlers-cleaners")+I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+
                   I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+
                   I(race=="Other")+I(race=="Black")+I(race=="White")+
                   I(sex=="Male")+
                   hours_per_week+
                   I(native_country=="europe")
                 ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_12)


LR_Model1_13=glm(TargetVariable
                 ~age+
                   I(workclass=="Private")+I(workclass=="SelfEmployed")+I(workclass=="state-gov")+
                   I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                   I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                   
                   I(marital_status=="Married")+I(marital_status=="Never-married")+
                   I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                   I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+
                   I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+
                   I(race=="Other")+I(race=="Black")+I(race=="White")+
                   I(sex=="Male")+
                   hours_per_week+
                   I(native_country=="europe")
                 ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_13)


LR_Model1_14=glm(TargetVariable
                 ~age+
                   I(workclass=="Private")+I(workclass=="SelfEmployed")+
                   I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                   I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                   
                   I(marital_status=="Married")+I(marital_status=="Never-married")+
                   I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                   I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+
                   I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+
                   I(race=="Other")+I(race=="Black")+I(race=="White")+
                   I(sex=="Male")+
                   hours_per_week+
                   I(native_country=="europe")
                 ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_14)



LR_Model1_15=glm(TargetVariable
                 ~age+
                   I(workclass=="SelfEmployed")+
                   I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                   I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                   
                   I(marital_status=="Married")+I(marital_status=="Never-married")+
                   I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                   I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+
                   I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+
                   I(race=="Other")+I(race=="Black")+I(race=="White")+
                   I(sex=="Male")+
                   hours_per_week+
                   I(native_country=="europe")
                 ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_15)


LR_Model1_16=glm(TargetVariable
                 ~age+
                   I(workclass=="SelfEmployed")+
                   I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                   I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                   
                   I(marital_status=="Married")+I(marital_status=="Never-married")+
                   I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                   I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+
                   I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+
                   I(race=="Other")+I(race=="White")+
                   I(sex=="Male")+
                   hours_per_week+
                   I(native_country=="europe")
                 ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_16)



LR_Model1_17=glm(TargetVariable
                 ~age+
                   I(workclass=="SelfEmployed")+
                   I(education=="11th")+I(education=="1st-4th")+I(education=="5th-6th")+I(education=="7th-8th")+
                   I(education=="9th")+I(education=="Bachelors")+I(education=="Doctorate")+I(education=="Masters")+
                   
                   I(marital_status=="Married")+I(marital_status=="Never-married")+
                   I(occupation=="Exec-managerial")+I(occupation=="Farming-fishing")+
                   I(occupation=="Machine-op-inspct")+I(occupation=="Other-service")+
                   I(occupation=="Protective-serv")+I(occupation=="Sales")+I(occupation=="Tech-support")+
                   I(race=="Other")+
                   I(sex=="Male")+
                   hours_per_week+
                   I(native_country=="europe")
                 ,data=DataForMLTrain, family='binomial')

summary(LR_Model1_17)


# Checking Accuracy of model on Testing data
PredictionProb=predict(LR_Model1_17,DataForMLTest,type = "response")
PredictionProb

IterationData=data.frame(
  Threshold=numeric(0),
  Accuracy=numeric(0)
)
install.packages('caret', dependencies = TRUE)
library(caret)

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
DataForMLTest$Prediction=ifelse(PredictionProb>0.52, 1, 0)
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
AccuracyResults[['overall']][1]

print(paste('### Overall Accuracy of Ctree Model is: ', round(100 * AccuracyResults[['overall']][1]) , '%'))






