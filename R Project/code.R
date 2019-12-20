library(tidyverse)
library(stringr)
library(caTools)
library(randomForest)
library(forecast)
library(neuralnet)
library(caret)
library(ggplot2)
library(corrplot)
setwd("C:/Users/Gaurav Korgaonkar/Desktop/Project")

### Data Cleaning
##LOAN STATS
L1<-read.csv("Data/LoanStats_securev1_2016Q1.csv",skip=1)
L2<-read.csv("Data/Q2.csv")
L3<-read.csv("Data/LoanStats_securev1_2016Q3.csv",skip=1)
L4<-read.csv("Data/LoanStats_securev1_2016Q4.csv",skip=1)
LoanStats<-rbind(L1,L2,L3,L4)
LoanStats<-LoanStats[LoanStats$fico_range_low > 660,]
LoanStats$risk_score<- (LoanStats$fico_range_low + LoanStats$fico_range_high) / 2
LoanStats$term<-as.integer(LoanStats$term %>% str_replace(' 36 months','36') %>% str_replace(' 60 months','60'))
LoanStats$int_rate<- as.double(str_replace(LoanStats$int_rate,'%',''))
LoanStats$status<-1
LoanStats$emp_length<-gsub('\\+','',LoanStats$emp_length)   
LoanStats$emp_length<-as.integer( LoanStats$emp_length %>%   str_replace(' years','') %>%  str_replace('< 1 year','0') %>%   str_replace(' year','') )
LoanStats.step1<-LoanStats[,c('loan_amnt', 'purpose', 'risk_score', 'dti', 'addr_state', 'emp_length','status')]
LoanStats.step1<-LoanStats.step1[complete.cases(LoanStats.step1),]
head(LoanStats.step1)

##REJECTED
R1<-read.csv("Data/RejectStats_2016Q1.csv",skip = 1)
R2<-read.csv("Data/RejectStats_2016Q1.csv",skip = 1)
R3<-read.csv("Data/RejectStats_2016Q1.csv",skip = 1)
R4<-read.csv("Data/RejectStats_2016Q1.csv",skip = 1)
RejectStats<-rbind(R1,R2,R3,R4)
colnames(RejectStats)<-c('loan_amnt','date' ,'purpose', 'risk_score', 'dti', 'zip_code', 'addr_state', 'emp_length', 'policy_code')
RejectStats<- RejectStats[,c('loan_amnt', 'purpose', 'risk_score', 'dti', 'addr_state', 'emp_length')]
RejectStats$status<- 0
RejectStats$emp_length<-gsub('\\+','',RejectStats$emp_length)   
RejectStats$emp_length<-as.integer( RejectStats$emp_length %>% str_replace(' years','') %>% str_replace('< 1 year','0') %>%                                   str_replace(' year','') )
RejectStats$dti<- as.double(str_replace(RejectStats$dti,'%',''))
RejectStats<-RejectStats[complete.cases(RejectStats),]
head(RejectStats)

### EDA
eda<-rbind(LoanStats.step1,RejectStats)
hist(eda$loan_amnt,xlab = 'Loan Amount',main = 'Distribution of Loan Amount',col='darkgreen')
hist(eda$emp_length,col='red',xlab='Employment length',main = 'Distribution of Employment length')
hist(LoanStats$int_rate,col='darkgreen',main = 'Distribution of Interest Rate',xlab = 'Interest Rate')
barplot(table(LoanStats$grade),col='red',main = 'Distribution of grade',xlab = 'Grade')

### Machine Learning models
#Step1
## Logistic glm
step1.set<-rbind(LoanStats.step1,RejectStats)
set.seed(101) 
sample = sample.split(step1.set$status, SplitRatio = .75)
step1.train = subset(step1.set, sample == T)
step1.test  = subset(step1.set, sample == F)
step1.model<-glm(status ~.,family=binomial(link='logit'),data=step1.train)
results<-predict(step1.model,newdata = step1.test)
results<-ifelse(results > 0.5,1,0)
gmodels::CrossTable(results,step1.test$status)
Error <- mean(results != step1.test$status)
print(paste('Accuracy',round((1-Error)*100,2),'%'))

## RF

rf.model<-randomForest(status ~ loan_amnt+risk_score+dti+addr_state+emp_length,
             data=step1.train, 
             importance=TRUE, 
             ntree=2)
results<-predict(rf.model,step1.test)
results<-ifelse(results > 0.5,1,0)
gmodels::CrossTable(results,step1.test$status)
Error <- mean(results != step1.test$status)
print(paste('Accuracy',1-Error))

##NN
nn<-neuralnet(status ~ loan_amnt+risk_score+dti+emp_length, data=step1.train, linear.output = F)
results<-predict(nn,step1.test)
results<-ifelse(results > 0.5,1,0)
Error <- mean(results != step1.test$status)
print(paste('Accuracy',1-Error))

#Step 2
#Manual clustering
LoanStats.step2<-LoanStats[,c('risk_score', 'loan_amnt', 'term', 'purpose', 'addr_state', 'debt_settlement_flag', 'dti', 'annual_inc','grade' ,'sub_grade', 'int_rate')]
LoanStats.step2<-LoanStats.step2[complete.cases(LoanStats.step2),]
head(LoanStats.step2)
LoanStats.step2<-LoanStats.step2[LoanStats.step2$int_rate<=25,]
LoanStats.step2<-LoanStats.step2[LoanStats.step2$loan_amnt<=38000,]
boxplot(LoanStats.step2$int_rate)
unique(LoanStats.step2$grade)
LoanStats.step2.A<-LoanStats.step2[LoanStats.step2$grade=='A',]
nrow(LoanStats.step2.A)
LoanStats.step2.B<-LoanStats.step2[LoanStats.step2$grade=='B',]
nrow(LoanStats.step2.B)
LoanStats.step2.C<-LoanStats.step2[LoanStats.step2$grade=='C',]
nrow(LoanStats.step2.C)
LoanStats.step2.D<-LoanStats.step2[LoanStats.step2$grade=='D',]
nrow(LoanStats.step2.D)
LoanStats.step2.E<-LoanStats.step2[LoanStats.step2$grade=='E',]
nrow(LoanStats.step2.E)
LoanStats.step2.F<-LoanStats.step2[LoanStats.step2$grade=='F',]
nrow(LoanStats.step2.F)
LoanStats.step2.G<-LoanStats.step2[LoanStats.step2$grade=='G',]
nrow(LoanStats.step2.G)

