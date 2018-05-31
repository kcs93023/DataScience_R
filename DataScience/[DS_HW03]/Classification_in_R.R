setwd("/Users/Kyle/Documents/Git/RStudioProjects/DataScience/[DS_HW03]/")
#데이터 읽기
rawdata = read.table("Dataset.data")
#데이터 컬럼 설정
colnames(rawdata) = c('gender', 'length','diameter','height','whole_weight',' shucked_weight','viscera_weight',
                      'shell_weight','ring')
#gender 컬럼에서 유일한 값 추출
unique_genter = unique(rawdata$gender)
#새로운 gender 컬럼 생성(rawdata의 행 수 만큼(컬럼이니까))
new_gender = matrix(0,nrow(rawdata),1)
#각 젠더 F,I,M에 맞게 숫자 값을 할당.
for (i in 1:length(unique_genter)){
  tmp_idx = which(rawdata$gender == unique_genter[i])
  new_gender[tmp_idx] = i
}
#rawdata의 1열을 제외한 나머지
rawdata = rawdata[,-1]
#각 컬럼들의 스케일을 맞춰주기 위해 min-max 정규화
for (i in 1:ncol(rawdata)){
  rawdata[,i] = (rawdata[,i] - min(rawdata[,i])) / (max(rawdata[,i]) - min(rawdata[,i]))
}
#스케일링한 rawdata와 new_gender 컬럼을 컬럼 바인딩으로 preprocessed Data 를 만든다.
prdata = cbind(rawdata, new_gender)
#prdata에서 new_gender를 2,3(I,M)항목에 대해 뽑는다.
tmp_idx = union(which(prdata$new_gender == 2), which(prdata$new_gender == 3))
#gender가 I,M인 데이터들을 추출
prdata = prdata[tmp_idx,]
#편의성을 위해 2번 클래스를 0번 클래스로 3번 클래스를 1번 클래스로 변환
prdata$new_gender[which(prdata$new_gender == 2)] = 0
prdata$new_gender[which(prdata$new_gender == 3)] = 1
#변수의 factor화
prdata$new_gender = as.factor(prdata$new_gender)

#훈련데이터의 비율은 0.7 테스트 데이터는 (1 - 0.7) = 0.3
trn_ratio = 0.7
#비율대로 인덱스 뽑기
trn_idx = sample(1:nrow(prdata), round(trn_ratio*nrow(prdata)))
tst_idx = setdiff(1:nrow(prdata), trn_idx)
#뽑은 인덱스로 데이터 가르기
trn_data = prdata[trn_idx,]
tst_data = prdata[tst_idx,]

library(class)
#knn 돌리기 k = 7
out_knn = knn(trn_data[,1:(ncol(trn_data)-1)], tst_data[,1:(ncol(tst_data)-1)], trn_data[,ncol(trn_data)],k = 7, prob=TRUE)
#Logistic(분류) Regression 돌리기 성별 예측하기
model_lr = glm(new_gender ~., data= trn_data, family='binomial')
out_lr = predict(model_lr, tst_data)

library(rpart)
#DT만들기 동일하게 성별예측하기 minsplit = 20
model_tree = rpart(new_gender ~., data = trn_data, control = rpart.control(minsplit = 20))
out_tree = predict(model_tree, tst_data)
#플라팅
plot(model_tree)
text(model_tree, use.n =TRUE)

#Neural networks
library(nnet)
#뉴럴넷 돌리기 Hidden Layer의사이즈는 15, 훈련 횟수 400번 당연히 성별 예측
model_nn = nnet(new_gender ~., data = trn_data,size = 15, linout =FALSE, maxit = 400)
out_nn = predict(model_nn, tst_data, type='class')


library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_updata.r')
library(reshape2)
plot.nnet(model_nn)

library(e1071)
#svm해보기
model_svm = svm(new_gender ~., data = trn_data, type="C-classification", kernel="radial", gamma=10, cost=50)
out_svm = predict(model_svm, tst_data)
#성능측정
target = tst_data[, ncol(tst_data)]
outs = cbind(target, out_knn)

tmp_idx1 = which(out_lr >= 0)
tmp_idx2 = which(out_lr <0)
out_lr2 = out_lr
out_lr2[tmp_idx1] = 1
out_lr2[tmp_idx2] = 0
outs = cbind(outs, out_lr2)

out_tree2 = out_tree[,2]
tmp_idx1 = which(out_tree2 >= 0.5)
tmp_idx2 = which(out_tree2 < 0.5)
out_tree2[tmp_idx1] = 1
out_tree2[tmp_idx2] = 0
outs = cbind(outs, out_tree2)
#0 : Target, 2 : kNN, 3 : LR, 4 : DT, 5 : NN, 6 : SVM
outs = cbind(outs, out_nn, out_svm)

for (i in 1:ncol(outs)){
  if (length(which(outs[,i] == 2)) == 0){
    next
  }
  
  tmp_idx1 = which(outs[,i] == 2)
  tmp_idx2 = which(outs[,i] == 1)
  
  outs[tmp_idx1, i] = 1
  outs[tmp_idx2, i] = 0
}
#confusionMatrix
library(caret)
#kNN
confusionMatrix(factor(outs[,2]), factor(outs[,1]))
#LR
confusionMatrix(factor(outs[,3]), factor(outs[,1]))
#DT
confusionMatrix(factor(outs[,4]), factor(outs[,1]))
#NN
confusionMatrix(factor(outs[,5]), factor(outs[,1]))
#SVM
confusionMatrix(factor(outs[,6]), factor(outs[,1]))

library(pROC)
plot(roc(tst_data$new_gender, out_lr, direction="<"),
     col='red', lwd=3, main='ROC')
