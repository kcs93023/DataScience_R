rm(list=ls())
par(mfrow=c(1,2))
#1. LoadData
toyotaData = read.csv('toyota.csv')
#2. Remove UnInformative Data
toyotaData$Model = NULL
toyotaData$Color = NULL
toyotaData$Id = NULL
x = range(1:max(toyotaData$Price))
y = x
#3. Preprocessing
#Extract Unique Data from Column HP
unique_hp = unique(toyotaData$HP)
#Make dummy Variable for 1-of-C Coding
hp_dummy = as.data.frame(matrix(0, nrow(toyotaData), length(unique_hp)-1))
#Do 1-of-C Coding
for( i in 1:(length(unique_hp)-1)){
  tmp = unique_hp[i]
  
  tmp_idx = which(toyotaData$HP == tmp)
  
  hp_dummy[tmp_idx,i] = i
  
  colnames(hp_dummy)[i] = sprintf("hp_%s",tmp)
}
#Column Bind HP Column 1-of-C coded with Original Dataset
prdata = toyotaData[1:length(toyotaData)-1]
prdata = cbind(hp_dummy,prdata)
#Get Training Data(70%) Index from Original Data
trn_ratio = .7
trn_idx = sample(1:nrow(prdata), round(trn_ratio*nrow(prdata)))
tst_idx = setdiff(1:nrow(prdata), trn_idx)
#Get Training Data, Test Data
trn_data = prdata[trn_idx,]
tst_data = prdata[tst_idx,]

#4. Train a linear regression model with the data
#Fit Linear Regression
fit_lr = lm(Price ~., data=trn_data)
#Get Summary LinearRegression
summary(fit_lr)
#Predict from testData
pred_lr = predict(fit_lr, tst_data)
#Plotting Linear Regression with red line(y = x)
plot(tst_data$Price, pred_lr,xlab = 'TestDataSet_Price',ylab='LRPredict_Price',
     abline(a=0,b=1,col="red"))
#Calculate mse 
mse_lr = mean((tst_data$Price-pred_lr)^2 )

#5. Train a stepwise linear regression model with the data
#Fit StepWise Linear Regression
step_lr = step(fit_lr, direction = "both")
#Get Summary StepWise Linear Regression
summary(step_lr)
#Predict from testData
pred_step = predict(step_lr, tst_data)
#Calculate mse
mse_step = mean((tst_data$Price-pred_step)^2)
#Plotting StepWise Linear Regression with blue line(y = x)
plot(tst_data$Price, pred_step,xlab = 'TestDataSet_Price',ylab='StepWiseLRPredict_Price',
     abline(a=0,b=1,col="blue"))


