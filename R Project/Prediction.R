# Interest rate prediction
#Cluster A
#Linear
set.seed(101) 
sample = sample.split(LoanStats.step2.A$int_rate, SplitRatio = .75)
step2.train = subset(LoanStats.step2.A, sample == T)
step2.test  = subset(LoanStats.step2.A, sample == F)
step2.model<-lm(int_rate ~risk_score+ loan_amnt + term     +       purpose +addr_state +dti +annual_inc+sub_grade,family = "binomial",data=step2.train)
results.lm<-predict(step2.model,newdata = step2.test)
accuracy(results.lm,step2.test$int_rate)

## RF
rf2.model<-randomForest(int_rate ~risk_score+ loan_amnt + term     +       purpose +addr_state +dti +annual_inc+sub_grade,data=step2.train,
                        importance=TRUE, 
                        ntree=2)
results.rf<-predict(rf2.model,step2.test)
accuracy(results.rf,step2.test$int_rate)
#Cluster B
#Linear
set.seed(101) 
sample = sample.split(LoanStats.step2.B$int_rate, SplitRatio = .75)
step2.train = subset(LoanStats.step2.B, sample == T)
step2.test  = subset(LoanStats.step2.B, sample == F)
step2.model<-lm(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train)
summary(step2.model)
results.lm<-predict(step2.model,newdata = step2.test)
accuracy(results.lm,step2.test$int_rate)
## RF
rf2.model<-randomForest(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train,
                        importance=TRUE, 
                        ntree=2)
results.rf<-predict(rf2.model,step2.test)
accuracy(results.rf,step2.test$int_rate)
#Cluster C
#Linear
set.seed(101) 
sample = sample.split(LoanStats.step2.C$int_rate, SplitRatio = .75)
step2.train = subset(LoanStats.step2.C, sample == T)
step2.test  = subset(LoanStats.step2.C, sample == F)
step2.model<-lm(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train)
summary(step2.model)
results.lm<-predict(step2.model,newdata = step2.test)
accuracy(results.lm,step2.test$int_rate)
## RF
rf2.model<-randomForest(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train,
                        importance=TRUE, 
                        ntree=2)
results.rf<-predict(rf2.model,step2.test)
accuracy(results.rf,step2.test$int_rate)
#Cluster D
#Linear
set.seed(101) 
sample = sample.split(LoanStats.step2.D$int_rate, SplitRatio = .75)
step2.train = subset(LoanStats.step2.D, sample == T)
step2.test  = subset(LoanStats.step2.D, sample == F)
step2.model<-lm(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train)
summary(step2.model)
results.lm<-predict(step2.model,newdata = step2.test)
accuracy(results.lm,step2.test$int_rate)
## RF
rf2.model<-randomForest(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train,
                        importance=TRUE, 
                        ntree=2)
results.rf<-predict(rf2.model,step2.test)
accuracy(results.rf,step2.test$int_rate)
#Cluster E
#Linear
set.seed(101) 
sample = sample.split(LoanStats.step2.E$int_rate, SplitRatio = .75)
step2.train = subset(LoanStats.step2.E, sample == T)
step2.test  = subset(LoanStats.step2.E, sample == F)
step2.model<-lm(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train)
summary(step2.model)
results.lm<-predict(step2.model,newdata = step2.test)
accuracy(results.lm,step2.test$int_rate)
## RF
rf2.model<-randomForest(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train,
                        importance=TRUE, 
                        ntree=2)
results.rf<-predict(rf2.model,step2.test)
accuracy(results.rf,step2.test$int_rate)
#Cluster F
#Linear
set.seed(101) 
sample = sample.split(LoanStats.step2.F$int_rate, SplitRatio = .75)
step2.train = subset(LoanStats.step2.F, sample == T)
step2.test  = subset(LoanStats.step2.F, sample == F)
step2.model<-lm(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train)
summary(step2.model)
results.lm<-predict(step2.model,newdata = step2.test)
accuracy(results.lm,step2.test$int_rate)
## RF
rf2.model<-randomForest(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train,
                        importance=TRUE, 
                        ntree=2)
results.rf<-predict(rf2.model,step2.test)
accuracy(results.rf,step2.test$int_rate)
#Cluster G
#Linear
set.seed(101) 
sample = sample.split(LoanStats.step2.G$int_rate, SplitRatio = .75)
step2.train = subset(LoanStats.step2.G, sample == T)
step2.test  = subset(LoanStats.step2.G, sample == F)
step2.model<-lm(int_rate ~risk_score+ loan_amnt + term+dti +annual_inc+sub_grade,data=step2.train)
summary(step2.model)
results.lm<-predict(step2.model,newdata = step2.test)
accuracy(results.lm,step2.test$int_rate)
library(gains)  
gain <- gains(step2.test$int_rate, results.lm, groups=10)
# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(step2.test$int_rate))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(step2.test$int_rate))~c(0, dim(step2.test)[1]), lty=2)
# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(step2.test$int_rate)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,1.2), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

