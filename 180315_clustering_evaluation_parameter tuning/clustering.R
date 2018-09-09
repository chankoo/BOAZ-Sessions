##knn
rm(list = ls())
data <- iris[, c("Sepal.Length", "Sepal.Width", "Species")]

# 데이터 나누기 (train set, validation set, test set)

# 재현성을 위한 seed 설정 
set.seed(123) 

# idx 설정 
idx <- sample(x = c("train", "valid", "test"), 
              size = nrow(data), 
              replace = TRUE, 
              prob = c(3, 1, 1)) 

# idx에 따라 데이터 나누기 
train <- data[idx == "train", ] 
valid <- data[idx == "valid", ] 
test <- data[idx == "test", ]

# 데이터 분포 확인하기
# 색상 투명도 설정을 위한 패키지 설치 및 라이브러리 불러오기 
#install.packages("scales") 
library(scales)

# train 산점도 그리기 
plot(formula = Sepal.Length ~ Sepal.Width, 
     data = train, 
     col = alpha(c("purple", "blue", "green"), 0.7)[train$Species],
     main = "train - Classification Species") 

# valid 표시 
points(formula = Sepal.Length ~ Sepal.Width, 
       data = valid, 
       pch = 17, 
       cex = 1.2, 
       col = "red") 

# test 표시 
points(formula = Sepal.Length ~ Sepal.Width, 
       data = test, 
       pch = 15, 
       cex = 1.2, 
       col = "orange") 

# 범례 그리기 
legend("topright", 
       c(levels(data$Species), "valid", "test"), 
       pch = c(1, 1, 1, 17, 15), 
       col = c(alpha(c("purple", "blue", "green"), 0.7), "red", "orange"), 
       cex = 0.9)

# x와 y로 나누기

train_x <- train[, -3] 
valid_x <- valid[, -3] 
test_x <- test[, -3] 

train_y <- train[, 3] 
valid_y <- valid[, 3] 
test_y <- test[, 3]

# 5.  knn 적용하기
#knn(train,test,k=num,cl(=traing set의 클래스 벡터))
#install.packages("class") 
library(class) 

# k = 1 일 때 
set.seed(1234) 
knn_1 <- knn(train = train_x, 
             test = valid_x, 
             cl = train_y, 
             k = 1) 

# train 산점도 그리기 
plot(formula = Sepal.Length ~ Sepal.Width, 
     data = train, 
     col = alpha(c("purple", "blue", "green"), 0.7)[train$Species], 
     main = "KNN (k = 1)") 


# knn valid 결과 표시하기 
points(formula = Sepal.Length ~ Sepal.Width, 
       data = valid, 
       pch = 17, 
       cex = 1.2, 
       col = alpha(c("purple", "blue", "green"), 0.7)[knn_1]) 

# 범례 그리기 
legend("topright", 
       c(paste("train", levels(train$Species)), 
         paste("valid", levels(valid$Species))), 
       pch = c(rep(1, 3), rep(17, 3)), 
       col = c(rep(alpha(c("purple", "blue", "green"), 0.7), 2)), 
       cex = 0.9) 

# 산점도를 보면, setosa는 잘 분류되는 반면 versicolor와 virginica는 분류 오류가 있음을 알 수 있다.

# 분류 정확도 계산하기 
accuracy_1 <- sum(knn_1 == valid_y) / length(valid_y) ; accuracy_1

# k = 21 일 때 
set.seed(1234) 

knn_21 <- knn(train = train_x, 
              test = valid_x, 
              cl = train_y, k = 21) 

plot(formula = Sepal.Length ~ Sepal.Width, 
     data = train, 
     col = alpha(c("purple", "blue", "green"), 0.7)[train$Species],
     main = "KNN (k = 21)") 


# knn valid 결과 표시하기 
points(formula = Sepal.Length ~ Sepal.Width, 
       data = valid, 
       pch = 17, 
       cex = 1.2, 
       col = alpha(c("purple", "blue", "green"), 0.7)[knn_21]) 


# 범례 그리기 
legend("topright", 
       c(paste("train", levels(train$Species)), 
         paste("valid", levels(valid$Species))), 
       pch = c(rep(1, 3), rep(17, 3)), 
       col = c(rep(alpha(c("purple", "blue", "green"), 0.7), 2)), cex = 0.9) 


# 분류 정확도 계산하기 
accuracy_21 <- sum(knn_21 == valid_y) / length(valid_y) ; accuracy_21

#-------------------------------------------------------------------------------------------------
## Kmeans 클러스터링
#iris
rm(list=ls())
head(iris)
str(iris) #iris 자료구조 확인
data<-iris[,-5]          #5열은 연속형 변수가 아니기 때문에 빼줌
k_data1<-kmeans(data,3)  #kmeans를 이용해 꽃을 setosa,versicolor,virginica 3가지 군집으로 나눠보자
      
k_data1
names(k_data1)
k_data1$cluster # 각 데이터가 어떤 군집으로 분류되었는지 확인
k_data1$centers # 각 군집의 중심점
k_data1$withinss # 군집 내 거리제곱합(값이 작을수록 좋다) / 반대로 군집 간 거리제곱합은 클수록 좋다.
sum(k_data1$withinss)
table(k_data1$cluster, iris$Species) #함수 table을 통해 잘 분류되었는지 확인

# 잘못군집화 하는 경우를 대비해서 시행횟수를 늘리는 방법 : nstart 
# 결과를 반복했을 때 결과값은 같지만, 분류된 군집명은 랜덤으로 지정된다.
k_data2<-kmeans(data,3,nstart = 25)  #총 25번을 시행하고 그중 최적의 결과를 제시해줌
table(k_data2$cluster,iris$Species)
#accuracy
(50+48+36)/sum(table(k_data2$cluster,iris$Species))

## kmeans 에 표준화 적용시키기
# 군집 분석은 관측치 간의 거리를 이용하기 때문에 변수의 단위가 결과에 큰 영향을 미친다.  
# 그래서 변수를 표준화 하는 작업이 필요!
# (iris 데이터는 단위가 같기 때문에 사실은 표준화를 안해줘도 될거같다)
data_s<-scale(data);data_s #iris를 데이터를 표준화 시킨다
k_data_s<-kmeans(data_s,3,iter.max=25)  #여기서는 iter.max라는 옵션으로 반복횟수를 주었음
table(k_data_s$cluster,iris$Species)
#accuracy
(50+39+36)/sum(table(k_data_s$cluster,iris$Species))

#군집확인
library(ggplot2)
data$cluster<-as.factor(k_data_s$cluster)
qplot(Petal.Width,Petal.Length,colour=cluster,data=data)

##최적의 k값 결정
# K-means 군집분석에서는 군집의 중심 갯수를 지정하는 것이 중요하다. 
# 몇개의 군집 중심이 적당한지 결정하는 방법에는 여러가지가 존재한다. 
# wssplot 함수를 이용하는 방법, NbClust 패키지를 사용하는 방법등이 있다.


#방법1) NbClust
#install.packages("NbClust")
library(NbClust)
set.seed(1234)
nc<-NbClust(data_s,min.nc = 2,max.nc=10,method = "kmeans")  #min.nc&max.nc : 최소,최대 군집수를 설정  
barplot(table(nc$Best.n[1,]),xlab = "# of Clusters",ylab="# of Criteria",main="# of Clusters Chosen")

#방법2) wssplot
wssplot<-function(data,nc=10,seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=i)$withinss)}
  plot(1:nc,wss,type='b',xlab="# of Clusters",ylab="Within groups sum of squares")
}

wssplot(data)


##################################################################################################

## Hclust(계층적 군집화)
# 참고 : hclust의 계층적 클러스터링의 결과를 kmeans의 초기 군집으로 이용해
# 결과값의 안정성을 올리기도 한다.



measure <-structure(list(V1 = 1:20, V2 = c(34L, 37L, 38L, 36L, 38L, 43L, 40L, 38L, 40L, 41L, 36L, 
                                           36L, 34L, 33L, 36L, 37L, 34L, 36L, 38L, 35L), 
                                          V3 = c(30L, 32L, 30L, 33L, 29L, 32L, 33L, 30L, 30L, 32L, 24L, 25L, 24L, 22L, 26L, 26L, 25L, 26L, 28L, 23L), 
                                          V4 = c(32L, 37L, 36L, 39L, 33L, 38L, 42L, 40L, 37L, 39L, 35L, 37L, 37L, 34L, 38L, 37L, 38L, 37L, 40L, 35L)), 
                                          .Names = c("V1", "V2", "V3", "V4"), class = "data.frame", row.names = c(NA, -20L))
head(measure)
measure <- measure[,-1]
names(measure)<-c("chest","waist","hips")
measure$gender<-gl(2,10)   #gl() : factor level 생성해주는 함수
levels(measure$gender)<-c("male","female")
head(measure)

#거리 행렬 생성
#dist함수 <-행간 거리를 계산해주는 함수
dm<-dist(measure[,-4]);dm
round(dm,2)

??hclust
#덴드로그램 그려보기
h1<-hclust(dm,method="single")  #최단연결법
plot(h1)

h2<-hclust(dm,method="complete") #최장연결법
plot(h2)

h3<-hclust(dm,method="average") #평균연결법
plot(h3)

par(mfrow=c(1,3))
plot(h1);plot(h2);plot(h3)

#위 자료 해석
#자료가 남자 10명, 여자 10명으로 구성되어있기 때문에 2개의 군집으로 나눠질거라 예상할 수 있다
#덴드로그램 높이의 변화를 보고 2개의 군집으로 나눌 수 있는 기준(cut point) 를 결정
#덴드로그램 확인 결과 최장연결법과 평균연결법은 구분이 명확함
#최단연결법은 비교적 구분을 잘 하지 못함 : 연쇄현상이 발생



