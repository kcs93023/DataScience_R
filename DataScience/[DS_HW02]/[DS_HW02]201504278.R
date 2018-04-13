rm(list=ls())

toyotaData = read.csv('toyota.csv')
toyotaData$Model = NULL
toyotaData$Color = NULL
toyotaData$Id = NULL
x = range(1:max(toyotaData$Price))
y = x
unique_hp = unique(toyotaData$HP)

print(unique_hp)

hp_dummy = as.data.frame(matrix(0, nrow(toyotaData), length(unique_hp)-1))

for( i in 1:(length(unique_hp)-1)){
  tmp = unique_hp[i]
  
  tmp_idx = which(toyotaData$HP == tmp)
  
  hp_dummy[tmp_idx,i] = i
  
  colnames(hp_dummy)[i] = sprintf("hp_%s",tmp)
}

prdata = toyotaData[1:length(toyotaData)-1]
prdata = cbind(hp_dummy,prdata)

trn_ratio = .7
trn_idx = sample(1:nrow(prdata), round(trn_ratio*nrow(prdata)))
tst_idx = setdiff(1:nrow(prdata), trn_idx)

trn_data = prdata[trn_idx,]
tst_data = prdata[tst_idx,]

fit_lr = lm(Price ~., data=trn_data)

summary(fit_lr)
pred_lr = predict(fit_lr, tst_data)

abline(a=0,b=1,col="red")
plot(tst_data$Price, pred_lr)

mse_lr = mean((tst_data$Price-pred_lr)^2 )
