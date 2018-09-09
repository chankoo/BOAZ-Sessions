results<-read.csv("http://www.openwith.net/wp-content/uploads/2016/08/sms_results.csv")
head(results) #predict_type 이 ham 일때 prob_spam 0에 가까움. 반대의 경우 1에 가깝다.

head(subset(results,actual_type!=predict_type)) #results 에서 예측값과 측정값이 다른 예제만 보여줌

#(1)results로 confusion matrix(혼돈행렬)만들어 (2)모델 평가하기
#(1)혼돈행렬 만드는 방법
#방법1) table()
#방법2) CrossTable()
#방법3) confusionmatrix()
#(2)평가 방법
#방법1)confusion matrix로부터 나온 각종 통계량 ex)정확도,정밀도,재현율 등등
#방법2)ROC 커브
#방법3)카파통계량

#(1)혼돈행렬 만들기 
#table()
table(results$actual_type,results$predict_type,dnn = c("actual","predict"))
#CrossTable()
#install.packages("gmodels")
library(gmodels)
CrossTable(results$actual_type,results$predict_type)
#confusionmatrix()
#install.packages("caret")
library(caret)
confusionMatrix(results$predict_type,results$actual_type,positive = "spam")  #confusionMatrix(예측값,실제값)
#positive: an optional character string for the factor level that corresponds to a "positive" result 

#(2) 모델 평가
#방법1) 각종통계량 계산
#a) 직접계산
accuracy<-(1202+154)/sum(table(results$actual_type,results$predict_type,dnn = c("actual","predict")));accuracy
precision<-154/ (154+9);precision  #스팸으로 예측된 것 중 실제로 스팸인 비율 
recall<-154/(29+154);recall   #실제로 스팸인데 스팸이라고 예측된 비
#b)confusionMatrix() 함수 결과로 판단
confusionMatrix(results$predict_type,results$actual_type,positive = "spam") #sensitivity==recall , ppv==precision

#방법2) ROC curve
#install.packages("ROCR")
library(ROCR)
results_pred<-prediction(predictions=results$prob_spam,labels=results$actual_type) 
perf<-performance(results_pred,measure="tpr",x.measure = "fpr")  #tpr:True Positive Rate=sensitivity , fpr:False Positive Rate=1-specificity
#measure: 평가에 사용할 성능 측정 값 , x.measure: 두번째 성과지표
plot(perf,main="Spam ROC curve")
perf<-performance(results_pred,measure="auc") 
str(perf) # AUC=y.value 0.983  #0.5<auc<1, 1로 갈수록 좋

#방법3) 카파통계량
#a) 직접 계산
pr_a<-accuracy
pr_e<-0.7840
k=(pr_a-pr_e)/(1-pr_e);k   
#b) irr 패키지 / vcd 패키지


#Improving Model Performance -Machine learning with R-
#Tuning models for better performance Using caret for automated parameter tuning

#<들어가기전>
#improving model performance의 방법
#방법1) 각 모델을을 학습시킬 때 최적의 매개변수를 찾는 것
#방법2) 다양한 모델을 모두 이용해 얻은 결과를 조합하여 improving 하는 방법 : 앙상블 --> A,B조에게 넘겨요!


#모델의 매개변수를 임의적으로 선택하는 것은 한계 
#최적의 조합을 찾기 위해 가능한 한 많은 매개변수 값을 찾는 것이 좋다
#모델마다 다양한 파라미터가 존재하기 때문에 모두 측정하기엔 무리..
#Caret 패키지를 이용하여 한번에 최적의 모델을 찾자!


#train()함수에서 핵심 기능을 제공
#이 함수를 사용해 평가와 지표를 선택해 최적의 모델 검색을 자동화

#install.packages("caret")
#install.packages("C50")

library(caret)
library(C50)


##### 1. Predicting default with c5.0
setwd("C:/Users/Chankoo/Desktop/BOAZ/세션자료/180315_clustering_evaluation_parameter tuning")
credit <- read.csv("credit.csv", header = T)
str(credit)
credit$default <- as.factor(credit$default) #숫자형 -> 요인
str(credit)


##1) Creating a simple tuned model(기본값으로 실험하기)
set.seed(300)

#trian() 맛보기
#pls 회귀 예제로 공부해보기
#plosfit<-train(Class~.,data=training,method="pls",preProc=c("center","scale"))   #train의 기본유형


#모델의 목적: 연체 여부 분류
m <- train(default ~ ., data = credit, method = "C5.0") #사용 모델 Decision Tree C 5.0
m
p <- predict(m, newdata=credit)  #예측값 구하기
table(p, credit$default)


#Customizing the tuning process(사용자 지정값으로 실험하기)

#옵션을 사용하여 사용자 주문을 해보자.
#trainControl()함수를 이용해 샘플링방법과 최고모델선정방법을 설정할 수 있다
#method : cv, repeatedcv, boot 등이 존재
#selectionFunction -> Best: metirc이 최고 모델 선정
#                     oneSE: best performance에서 표준편차의 1배수 내에 있는 가장 간단한 모델 선정
#ctrl<-trainControl(method="repeatedcv",repeats=3)  #repeatd k-fold crossvalidation을 3번 반복하겠다
#plofit<-train(Class~.,data=training,method="pls",trControl=ctrl,metric="ROC",preProc=c("center","scale"))
#method : 사용되는 resampling의 유형. default="bootstrap"
#metric : 최상의 결과를 가지는 조절모수 선택.  option:accuracy,Kappa,RMSE,Rsquared 
#tuneGrid : 조절모수의 후보 값을 변경하는 옵션 

ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")   
#ctrl 객체 생성. k-fold crossvalidation method를 10번 반복하고  oneSE selectionFunction을 사용

grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")
grid #1*1*8 총 8행의 데이터프레임 생성

set.seed(300)
m <- train(default ~., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m
m$bestTune

p <- predict(m, credit)
table(p, credit$default)

##### 2. iris with Knn

data(iris)
TrainData <- iris[,1:4]
Species <- iris[,5]

knnFit1 <- train(TrainData, Species,
                 method = "knn",
                 trControl = trainControl(method="cv"))

knnFit1
pred1<-predict(knnFit1,newdata=iris)
table(pred1,Species)


knnFit2 <- train(TrainData, Species,
                 method = "knn",
                 trControl = trainControl(method = "boot"))

knnFit2
pred2<-predict(knnFit2,iris)
table(pred2,Species)


