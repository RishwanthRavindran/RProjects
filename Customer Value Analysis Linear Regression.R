# Regression Case Study

# Step-1
# Identify the Problem Statement, What are you trying to solve?
# target variable: Customer Lifetime Value


# Step-2
# Identify the Target variable in the data, What value will be predicted?
##spot the Customer Lifetime Value in data, make sure you have the correct target variable

# Step-3
#Loading the raw Data
CustomerValueAnalysisData=read.csv('D:/Classroom/Stats + R/Final R Project/Material/CustomerValueAnalysisData.csv',na.strings = c(""," ","NA","NULL"))
View(CustomerValueAnalysisData)
##in R, missing values are identified by "na"
##na.strings= to convert the blanks, extra spaces, cells containing the word NA or NULL in 
##string columns to "na"
colSums(is.na(CustomerValueAnalysisData))
#there are no missing values

# Step-4
# Exploring the dataset: data exploratory commands
#gives the descriptive statistics of the data
#what kind of variables are there?
#You spot Customer Lifetime Value , which is the Targer Variable, which you want to fit
str(CustomerValueAnalysisData)
head(CustomerValueAnalysisData)
dim(CustomerValueAnalysisData)
# Removing useless columns in the data, and explore the rest
UselessColumns=c('Customer','Effective.To.Date')
CustomerValueAnalysisData[, UselessColumns]=NULL

###check if all the categorical variables are factor or not
CustomerValueAnalysisData$State=as.factor(CustomerValueAnalysisData$State)

CustomerValueAnalysisData$Response=as.factor(CustomerValueAnalysisData$Response)
CustomerValueAnalysisData$Coverage=as.factor(CustomerValueAnalysisData$Coverage)
CustomerValueAnalysisData$Education=as.factor(CustomerValueAnalysisData$Education)
CustomerValueAnalysisData$EmploymentStatus=as.factor(CustomerValueAnalysisData$EmploymentStatus)
CustomerValueAnalysisData$Gender=as.factor(CustomerValueAnalysisData$Gender)

CustomerValueAnalysisData$Income=as.numeric(CustomerValueAnalysisData$Income)

CustomerValueAnalysisData$Location.Code=as.factor(CustomerValueAnalysisData$Location.Code)
CustomerValueAnalysisData$Marital.Status=as.factor(CustomerValueAnalysisData$Marital.Status)

CustomerValueAnalysisData$Monthly.Premium.Auto=as.numeric(CustomerValueAnalysisData$Monthly.Premium.Auto)
CustomerValueAnalysisData$Months.Since.Last.Claim=as.numeric(CustomerValueAnalysisData$Months.Since.Last.Claim)
CustomerValueAnalysisData$Months.Since.Policy.Inception=as.numeric(CustomerValueAnalysisData$Months.Since.Policy.Inception)

CustomerValueAnalysisData$Type.of.Open.Complaints=as.factor(CustomerValueAnalysisData$Policy.Type)
CustomerValueAnalysisData$Type.of.Policies=as.factor(CustomerValueAnalysisData$Type.of.Policies)
CustomerValueAnalysisData$Policy.Type=as.factor(CustomerValueAnalysisData$Policy.Type)
CustomerValueAnalysisData$Policy=as.factor(CustomerValueAnalysisData$Policy)
CustomerValueAnalysisData$Renew.Offer.Type=as.factor(CustomerValueAnalysisData$Renew.Offer.Type)
CustomerValueAnalysisData$Sales.Channel=as.factor(CustomerValueAnalysisData$Sales.Channel)

CustomerValueAnalysisData$Vehicle.Class=as.factor(CustomerValueAnalysisData$Vehicle.Class)
CustomerValueAnalysisData$Vehicle.Size=as.factor(CustomerValueAnalysisData$Vehicle.Size)


str(CustomerValueAnalysisData)
summary(CustomerValueAnalysisData)


# Step-5
# Whether it is a Regression problem or Classification?
##look at the Target variable always.
#continous-regression



# Step-6
# Checking and treating missing values
##we need to treat missing values before we move ahead
# Checking missing values
#wherever its true- that place is a missing value
View(is.na(CustomerValueAnalysisData))

#colsums: sums up column wise the no. of occurance of "TRUE", that gives total MVs in 
#a column
colSums(is.na(CustomerValueAnalysisData))

#there are no missing values in the data 

tail(CustomerValueAnalysisData$Customer.Lifetime.Value)
summary(CustomerValueAnalysisData$Customer.Lifetime.Value)

#after removing the garbage columns and treating the missing values
#see for which of the columns you have to treat the outliers
boxplot(CustomerValueAnalysisData$Customer.Lifetime.Value, horizontal = T)
options(scipen = 999)
boxplot(CustomerValueAnalysisData$Income, horizontal = T)
boxplot(CustomerValueAnalysisData$Monthly.Premium.Auto, horizontal = T)
boxplot(CustomerValueAnalysisData$Months.Since.Last.Claim , horizontal = T)
boxplot(CustomerValueAnalysisData$Months.Since.Policy.Inception, horizontal = T)

#sorting the Customer.Lifetime.Value column for treating outlier
sort(CustomerValueAnalysisData$Customer.Lifetime.Value,decreasing = FALSE)
#here, there are some very high values in the end which are outliers for the Customer.Lifetime.Value column
##see what is last value in the data, which you won't consider as the outlier
#
quantiles=quantile(CustomerValueAnalysisData$Customer.Lifetime.Value,c( seq(0.75,1.00,0.001)))
                                                                      
quantiles
#check which quantile it approx corresponds to
#and all the values above that will be replaced
quantiles_final=quantile(CustomerValueAnalysisData$Customer.Lifetime.Value,0.985)
quantiles_final
max(CustomerValueAnalysisData$Customer.Lifetime.Value)

CustomerValueAnalysisData$Customer.Lifetime.Value = ifelse(CustomerValueAnalysisData$Customer.Lifetime.Value > quantiles_final , quantiles_final, CustomerValueAnalysisData$Customer.Lifetime.Value)
#check the boxplot again and see whether outliers are removed or not.
boxplot(CustomerValueAnalysisData$Customer.Lifetime.Value, horizontal = T)
max(CustomerValueAnalysisData$Customer.Lifetime.Value)

# Step-7
# Explore each "Potential" predictor for distribution and Quality
#covered earlier-Univariate analysis
##need to understand whether these potential predictors are worthy of
#selection or not- based on their distribution and we'll confirm with the help of different tests

##continuous column- histogram
##categorical column- bar plot


# Exploring a single CONTINUOUS feature
hist(CustomerValueAnalysisData$Income)
#understand the statistical details:
summary(CustomerValueAnalysisData$Income)


# Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("Customer.Lifetime.Value","Income", "Monthly.Premium.Auto", 
              "Months.Since.Last.Claim","Months.Since.Policy.Inception","Total.Claim.Amount")
ColsForHist
#Splitting the plot window into four parts
#2 rows and 2 columns
par(mfrow=c(2,3))

# library to generate professional colors
library(RColorBrewer) 
#?RColorBrewer

##### looping to create the histograms for each column

##hist_cols:an iterator- takes each value in the vector at a time
#ColsForHist: vector of columns which we have identified
#for each of the columns, we are plotting the histogram

#2 agruements: 
#whatever column is in the current iteration
#main(title- string and the variable name which is being plotted)
#col/color- Paired is the color palette name, supports upto 12 professional colors

for (hist_cols in ColsForHist){
  hist(CustomerValueAnalysisData[,c(hist_cols)], main=paste('Histogram of:',hist_cols), 
       col=brewer.pal(8,"Paired"))
}


############################################################
# Exploring a single CATEGORICAL feature
par(mfrow=c(1,1))
table(CustomerValueAnalysisData$State)
barplot(table(CustomerValueAnalysisData$State))

# Exploring MULTIPLE CATEGORICAL features


#Splitting the plot window into many parts
ColsForBar=c("State","Response ","Coverage","Education" ,"EmploymentStatus","Gender","Location.Code",
             "Marital.Status","Type.of.Open.Complaints","Type.of.Policies","Policy.Type",
             "Renew.Offer.Type","Sales.Channel","Vehicle.Class","Vehicle.Size ")
par(mfrow=c(3,6))
# looping to create the Bar-Plots for each column

for (ColumnName in ColsForBar){
  barplot(table(CustomerValueAnalysisData[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Paired"))
}

############################################################

# Step-8
##bivariate analysis

# Visual Relationship between predictors and target variable
##Regression- 2 scenarios
# Continuous Vs Continuous ---- Scatter Plot
# Continuous Vs Categorical --- Box Plot




# Continuous Vs Continuous --- Scatter plot
par(mfrow=c(1,1))
# For multiple columns at once
#no loop required, plot function can take dataframe as an I/P
ContinuousCols=c("Customer.Lifetime.Value","Income", "Monthly.Premium.Auto", 
                 "Months.Since.Last.Claim","Months.Since.Policy.Inception","Total.Claim.Amount")


#take a set of continuous columns, and subset the data and
#pass it to the plot function
####make sure that all the columns are continuous####

plot(CustomerValueAnalysisData[, ContinuousCols], col='blue')

##just to see all relationships at a glance
##want to see the correlation b/w target and predictor variables
##interested here with the 1st row or 1st col



# Continuous Vs Categorical Visual analysis: Boxplot

ColsForBox=c("State","Response","Coverage","Education",
             "EmploymentStatus","Gender","Location.Code","Marital.Status",
             "Type.of.Open.Complaints","Type.of.Policies","Policy.Type","Policy","Renew.Offer.Type",
             "Sales.Channel","Vehicle.Class","Vehicle.Size") 

par(mfrow=c(3,6))
for (Cat_cols in ColsForBox){ 
  boxplot(CustomerValueAnalysisData$Customer.Lifetime.Value~CustomerValueAnalysisData[,c(Cat_cols)], main=paste('BoxPlot of:',Cat_cols), 
          col=brewer.pal(8,"Paired"))
}


############################################
# Step-10
# Strength of Relationship between predictor and target variable
# Continuous Vs Continuous ---- Correlation test
# Continuous Vs Categorical---- ANOVA test


#####how to measure the strength of the relationship?########

# Continuous Vs Continuous : Correlation analysis
#we need to quanitify the strength
##correlation coefficient varies between -1 & 1

# use = "complete.obs" means use only those rows which are complete(No Missing values)

##high negative correlation between them as r is negative and close to -1

#for perfect positive correlation: r=1
#for perfect negative correlation: r=-1
#for zero correlation: r=0

# Correlation coefficient for multiple columns at once
ContinuousCols=c("Customer.Lifetime.Value","Income", "Monthly.Premium.Auto", 
                 "Months.Since.Last.Claim","Months.Since.Policy.Inception","Total.Claim.Amount")



##the following helps us to decide which col to keep and which one to reject
##get pairwise correlation values in a correlation matrix
CorrData=cor(CustomerValueAnalysisData[, ContinuousCols], use = "complete.obs")
CorrData

#-1<r<0
#0<r<1



#### Final columns which has high correlation with the target variable ###
#getting those variable names for which absolute correlation with price is >0.2



#we are interested in the magnitude only - how high the correlation is
#that is not determined by the sign, but by the value
#so, for simplicity, we ignore the negative signs, by taking abs()

abs(CorrData['Customer.Lifetime.Value',])>0.2

names(CorrData['Customer.Lifetime.Value',])
#now, i want those names, for which the condition is true
##command
names(CorrData['Customer.Lifetime.Value',][abs(CorrData['Customer.Lifetime.Value',])>0.2])
##Customer.Lifetime.Value is not a predictor, use the rest of the variables as potential continuous predictors

##### Continuous Vs Categorical correlation strength: ANOVA #####
# Analysis of Variance(ANOVA)
# H0: Variables are NOT correlated
# Small P-Value <5%--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)

ColsForAnova=c("State","Response","Coverage","Education",
               "EmploymentStatus","Gender","Location.Code","Marital.Status",
               "Type.of.Open.Complaints","Type.of.Policies","Policy.Type","Policy","Renew.Offer.Type",
               "Sales.Channel","Vehicle.Class","Vehicle.Size")
for (Anova_cols in ColsForAnova){
  AnovaTest = summary(aov(CustomerValueAnalysisData$Customer.Lifetime.Value~CustomerValueAnalysisData[,c(Anova_cols)]))
  print(Anova_cols)
  print(AnovaTest)
}

#Either by creating loop or by using single column  way we run  ANOVA Test

summary(aov(Customer.Lifetime.Value~State, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Response, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Coverage, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Education, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~EmploymentStatus, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Gender, data =CustomerValueAnalysisData ))
summary(aov(Customer.Lifetime.Value~Location.Code, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Marital.Status, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Type.of.Open.Complaints, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Type.of.Policies, data =CustomerValueAnalysisData ))
summary(aov(Customer.Lifetime.Value~Policy.Type, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Policy, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Renew.Offer.Type, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Sales.Channel, data = CustomerValueAnalysisData))
summary(aov(Customer.Lifetime.Value~Vehicle.Class, data =CustomerValueAnalysisData ))
summary(aov(Customer.Lifetime.Value~Vehicle.Size, data = CustomerValueAnalysisData))

str(CustomerValueAnalysisData)

############################################################

#########################################################################


#########################################################################
#########################################################################
#########################################################################

# Step-11

##try to bring the data in standardised format, to reduce coding effort
# Generating the Data frame for machine learning
InputData=CustomerValueAnalysisData
TargetVariableName='Customer.Lifetime.Value'
TargetVariableName

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis, select the useful variables
BestPredictorName= c("Monthly.Premium.Auto","Total.Claim.Amount","Coverage","Education",
                     "EmploymentStatus","Marital.Status","Type.of.Policies","Renew.Offer.Type",
                     "Vehicle.Class","Vehicle.Size")
                     
BestPredictorName


# Extracting Target and predictor variable columns from whole data to create a 
#generic dataset
TargetVariable =InputData[, c(TargetVariableName)]
TargetVariable
str(TargetVariable)

# Selecting all other columns as Predictors apart from target variable

PredictorVariable =InputData[, BestPredictorName]
PredictorVariable
str(PredictorVariable)

##creating the final data to be used for ML
DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)




#########################################################################

# Step-12

# Sampling | Splitting data into 70% for training 30% for testing

#1:nrow(DataForML): It will give you the row no.s 1 to (total no. of records)
#that is,the total set of rows
##i want 70% of the total no. of rows of the original data for training

set.seed(123)
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )

#so, how many rows are chosen for training?
length(TrainingSampleIndex) 

#Use those row indexes which are selected in TrainingSampleIndex for train set
#get all the rows corresponding to those row indexes which are selected in TrainingSampleIndex
#every time a different set of values are selected- SRSWOR
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTrain

#Use those row indexes which are NOT selected in TrainingSampleIndex for test set
DataForMLTest=DataForML[-TrainingSampleIndex, ]
DataForMLTest
#Rows selected for training will not be selected for testing
dim(DataForMLTrain)
dim(DataForMLTest)

########################################################################
# Creating Predictive models on training data to check the accuracy of each algorithm
###### Linear Regression #######


############# MLR ###############
#predict TV based on all other variables in DataForMLTrain
##either you write the all the variables separated by "+"
Model_Reg=lm(TargetVariable~Monthly.Premium.Auto+Total.Claim.Amount+Coverage+Education+EmploymentStatus+
             Marital.Status+Type.of.Policies+Renew.Offer.Type+
             Vehicle.Class+Vehicle.Size,data=DataForMLTrain)


Model_Reg_1=lm(TargetVariable~Monthly.Premium.Auto+Total.Claim.Amount+Education+EmploymentStatus+
                 Marital.Status+Type.of.Policies+Renew.Offer.Type+
                 Vehicle.Class+Vehicle.Size,data=DataForMLTrain)
summary(Model_Reg_1)

Model_Reg_2=lm(TargetVariable~Monthly.Premium.Auto+Education+EmploymentStatus+
                 Marital.Status+Type.of.Policies+Renew.Offer.Type+
                 Vehicle.Class+Vehicle.Size,data=DataForMLTrain)
summary(Model_Reg_2)

Model_Reg_3=lm(TargetVariable~Monthly.Premium.Auto+EmploymentStatus+
                 Marital.Status+Type.of.Policies+Renew.Offer.Type+
                 Vehicle.Class+Vehicle.Size,data=DataForMLTrain)
summary(Model_Reg_3)

Model_Reg_4=lm(TargetVariable~Monthly.Premium.Auto+EmploymentStatus+
                 Type.of.Policies+Renew.Offer.Type+
                 Vehicle.Class+Vehicle.Size,data=DataForMLTrain)
summary(Model_Reg_4)


Model_Reg_5=lm(TargetVariable~Monthly.Premium.Auto+
                 I(EmploymentStatus == "Employed")+ I(EmploymentStatus == "Medical Leave")+ I(EmploymentStatus == "Retired")+
               I(EmploymentStatus == "Unemployed")+
               Type.of.Policies+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer3")+I(Renew.Offer.Type=="Offer4")+
                 
                 I(Vehicle.Class== "Luxury Car")+I(Vehicle.Class== "Luxury SUV ")+I(Vehicle.Class== "Sports Car")+
                 I(Vehicle.Class== "SUV")+I(Vehicle.Class== "Two-Door Car ")
                ,data=DataForMLTrain)
summary(Model_Reg_5)


Model_Reg_6=lm(TargetVariable~Monthly.Premium.Auto+
                 I(EmploymentStatus == "Employed")+ I(EmploymentStatus == "Medical Leave")+ I(EmploymentStatus == "Retired")+
                 I(EmploymentStatus == "Unemployed")+
                 Type.of.Policies+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer3")+I(Renew.Offer.Type=="Offer4")+
                 
                 I(Vehicle.Class== "Luxury Car")+I(Vehicle.Class== "Sports Car")+
                 I(Vehicle.Class== "SUV")
               ,data=DataForMLTrain)
summary(Model_Reg_6)

Model_Reg_7=lm(TargetVariable~Monthly.Premium.Auto+
                 I(EmploymentStatus == "Employed")+ I(EmploymentStatus == "Retired")+
                 I(EmploymentStatus == "Unemployed")+
                 Type.of.Policies+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer3")+I(Renew.Offer.Type=="Offer4")+
                 
                 I(Vehicle.Class== "Luxury Car")+I(Vehicle.Class== "Sports Car")+
                 I(Vehicle.Class== "SUV")
               ,data=DataForMLTrain)
summary(Model_Reg_7)


Model_Reg_8=lm(TargetVariable~Monthly.Premium.Auto+
                 I(EmploymentStatus == "Employed")+
                 I(EmploymentStatus == "Unemployed")+
                 Type.of.Policies+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer3")+I(Renew.Offer.Type=="Offer4")+
                 
                 I(Vehicle.Class== "Luxury Car")+I(Vehicle.Class== "Sports Car")+
                 I(Vehicle.Class== "SUV")
               ,data=DataForMLTrain)
summary(Model_Reg_8)


Model_Reg_9=lm(TargetVariable~Monthly.Premium.Auto+
                 I(EmploymentStatus == "Employed")+
                 I(EmploymentStatus == "Unemployed")+
                 Type.of.Policies+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer3")+I(Renew.Offer.Type=="Offer4")+
                 
                 I(Vehicle.Class== "Sports Car")+
                 I(Vehicle.Class== "SUV")
               ,data=DataForMLTrain)
summary(Model_Reg_9)

Model_Reg_10=lm(TargetVariable~Monthly.Premium.Auto+
                 I(EmploymentStatus == "Employed")+
                 I(EmploymentStatus == "Unemployed")+
                 Type.of.Policies+
                 I(Renew.Offer.Type=="Offer2")+I(Renew.Offer.Type=="Offer3")+
                 
                 I(Vehicle.Class== "Sports Car")+
                 I(Vehicle.Class== "SUV")
               ,data=DataForMLTrain)
summary(Model_Reg_10)


Model_Reg_11=lm(TargetVariable~Monthly.Premium.Auto+
                  I(EmploymentStatus == "Employed")+
                  I(EmploymentStatus == "Unemployed")+
                  Type.of.Policies+
                 
                  
                  I(Vehicle.Class== "Sports Car")+
                  I(Vehicle.Class== "SUV")
                ,data=DataForMLTrain)
summary(Model_Reg_11)


Model_Reg_12=lm(TargetVariable~Monthly.Premium.Auto+
                  I(EmploymentStatus == "Employed")+
                 
                  Type.of.Policies+
                  
                  
                  I(Vehicle.Class== "Sports Car")+
                  I(Vehicle.Class== "SUV")
                ,data=DataForMLTrain)
summary(Model_Reg_12)
##when you run it, you see a bigger set of coefficients getting printed.
##when you create MLR, you choose multiple columns so you get the coefficients to
#be multiplied by each each column
#Estimate column: the intercept and the coefficient estimates associated to each predictor variable




##these m1, m2,....,c are what the algorithm is searching using the 
#trial and error method. It will again find out the particular set of values
#which is giving me the minimum possible SSE.




# Checking Accuracy of model on Testing data

##Based on the model created using training data, we will now predict
#on the test data and create a new column to store the predictions
#of linear model
head(DataForMLTest)
DataForMLTest$Pred_LM=predict(Model_Reg_12, DataForMLTest)
head(DataForMLTest)

###accuracy###
# Calculating the Absolute Percentage Error for each prediction
DataForMLTest$LM_APE= 100 *(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/DataForMLTest$TargetVariable)
head(DataForMLTest)
##we can see for each prediction what is the error I am commiting
##for final accuracy: we take the mean/median of all the errors and subtract it from 100

MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)
print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))


########################################################################
#############################Decision Tree##############################

##the DT algorithm function:ctree is present in the party library
##ctree is going to see those minute fluctuations in Prices and in each of these columns

##Here DT will select only the columns which helps to bifurcate at each step.

#install.packages("party")
library(party)
startTime=Sys.time()

##You have the Target variable, predicted by all other predictors in DataForMLTrain
Model_CTREE=ctree(TargetVariable ~. , data=DataForMLTrain)

#to see the if-else statements:
##you'll be able to see what kind of rules the DT has created for this data
Model_CTREE

##to visualize this:
plot(Model_CTREE)


###beauty of decision tree is that: even if you pass 20 variables, it will use only
#those variables which are useful in prediction, and it will simply ignore
#other variables.
#It is doing automatic feature selection,
##DT will choose those variables only which are helping to bifurcate


##the algorithm will find out the bifurcating variables on its own

endTime=Sys.time()
endTime-startTime


# Checking Accuracy of model on Testing data
##generating the predictions using decision tree and ariving at the accuracy
DataForMLTest$Pred_CTREE=as.numeric(predict(Model_CTREE, DataForMLTest))
head(DataForMLTest)

DataForMLTest$CTREE_APE= 100 *(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_CTREE)/DataForMLTest$TargetVariable)
head(DataForMLTest)
##we can see for each prediction what is the error I am committing
##for average accuracy: we take the average of all the errors and subtract it from 100

print(paste('### Mean Accuracy of Decision tree Model is: ', 100 - mean(DataForMLTest$CTREE_APE)))
print(paste('### Median Accuracy of Decision tree Model is: ', 100 - median(DataForMLTest$CTREE_APE)))


###we will look at the p-value to conclude the result of the test###
##test for homoskedasticity:
#HO: there exists homoskedasticity : error variances are equal
install.packages("lmtest")
library(lmtest)
bptest(Model_Reg_12)


############test for serial correlation#############
#Autocorrelation occurs when the residuals are not independent from each other
#HO: No autocorrelation
library(lmtest)
options(scipen = 999)
dwtest(Model_Reg_12)
#p-vlaue>0.05 means HO is accepted


#############test for normality################
#HO: errors is normally distributed
install.packages("nortest")
library(nortest)
resid=Model_Reg_12$residuals
options(scipen = 999)
ad.test(resid)


#############test for multicollinearity###################
#you should be able to understand whether we
#can reject any variable based on the test of multicollinearity 

Model_Reg_12=lm(TargetVariable~Monthly.Premium.Auto+
                  I(EmploymentStatus == "Employed")+
                  
                  Type.of.Policies+
                  
                  
                  I(Vehicle.Class== "Sports Car")+
                  I(Vehicle.Class== "SUV")
                ,data=DataForMLTrain)
summary(Model_Reg_12)


library(car)
VIF=vif(Model_Reg_12)
data.frame(VIF)
#VIF values are close to 1, we are good to go with our regression model 
