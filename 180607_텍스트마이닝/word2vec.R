#======================================
# word2vec로 연관 키워드 추출하기
#======================================
rm(list=ls())
# 
# install.packages("devtools") #Rtools는 Windows에 깔때 별도로 깔아야 한다.
# install.packages("wordVectors")
# install.packages("tsne") #다차원 척도법(MDS) MAP을 그리기 위해 필요함 

library(devtools)   
install_github("bmschmidt/wordVectors") 
library(wordVectors)
library(tsne)

#맥에서 한글처리
par(family = "AppleGothic")
#readr 패키지 설치
install.packages("readr")
library(readr)
#형태소 분석기 실행하기
system("tctStart")
#분석 결과 가져오기
parsedData =read.csv("kei_content_data.csv")
View(parsedData)

#word2vec Train용 TXT파일 만들기
write.table(parsedData$parsedContent,file = "kei_w2.txt", row.names = FALSE, col.names = FALSE)

#모델 Training
model = train_word2vec("kei_w2v.txt", output_file = "kei_w2v_2.bin", 
                       threads=3, vectors=100, window=12) 
#threads:코어갯수 높을수록 속도 빨라짐
#vectors:벡터갯수 높을수록 결과를 자세히 볼 수 있음(데이터가 10만개 넘으면 100~500개 정도로 설정해줌)
#word2vec 모델링은 학습데이터의 양이 많을수록 추론의 정확도가 좋아짐

#word2vector 확인하기
read.vectors("kei_w2v_2.bin")



#연관 키워드 추출하기
nearest_to(model,model[["환경"]], 10) #'환경'와 가까운 단어 10개 출력(거리측정법: 코사인거리)


#2가지 이상 키워드에 대한 연관 키워드 추출하기
some = nearest_to(model,model[[c("기후","정책")]], 20)
some

#단어간 연산하기
subVec = model[rownames(model)=="환경",] - model[rownames(model) == "제도",] + model[rownames(model) == "기후",] 
subNear = nearest_to(model, subVec, 20)
subNear
#단어 관계 시각화
plot(filter_to_rownames(model,names(some)))
plot(model)

#Euclidean Distance
dist(model[(row.names(model)=="환경" | row.names(model)=="개선"),])

#Cosine 유사도
cosineSimilarity(model[["기후","환경"]], model[["제도"]])
cosineDist(model[["기후","환경"]], model[["제"]])
