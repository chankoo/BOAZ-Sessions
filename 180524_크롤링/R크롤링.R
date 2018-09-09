#크롬을 사용하는 이유 can search with url
#크롬와 Explorer의 차이

library(rvest)
library(stringr)



# 아파트 매물 크롤링

url <- "http://land.naver.com/article/articleList.nhn?rletTypeCd=B01&tradeTypeCd=B1&rletNo=112008&cortarNo=1165010700&hscpTypeCd=B01%3AB02%3AB03&mapX=&mapY=&mapLevel=&page=1&articlePage=&ptpNo=&rltrId=&mnex=&bildNo=&articleOrderCode=&cpId=&period=&prodTab=&atclNo=&atclRletTypeCd=&location=2172&bbs_tp_cd=&sort=&siteOrderCode=&schlCd=&tradYy=&exclsSpc=&splySpcR=&cmplYn=#_content_list_target"
htm <- read_html(url)

node <- html_nodes(htm, ".txt")
txt1 <- html_text(node)

node <- html_nodes(htm, ".align_r strong")
txt2 <- html_text(node)

node <- html_nodes(htm, ".title_agent")
txt3 <- html_text(node)
txt3 <- gsub("\n|\t", "", txt3)

ds <- cbind(txt1, txt2, txt3)
colnames(ds) <- c("장점", "가격", "부동산")
ds

node <- html_nodes(htm, ".name .inner")
txt4 <- html_text(node)
txt4 <- gsub("\n|\t", "", txt4)








#page라는 정보 알아내기
"http://land.naver.com/article/articleList.nhn?rletTypeCd=B01&tradeTypeCd=B1&rletNo=112008&cortarNo=1165010700&hscpTypeCd=B01%3AB02%3AB03&mapX=&mapY=&mapLevel=&page=2&articlePage=&ptpNo=&rltrId=&mnex=&bildNo=&articleOrderCode=&cpId=&period=&prodTab=&atclNo=&atclRletTypeCd=&location=2173&bbs_tp_cd=&sort=&siteOrderCode=&schlCd=&tradYy=&exclsSpc=&splySpcR=&cmplYn=#_content_list_target"


url1 <- "http://land.naver.com/article/articleList.nhn?rletTypeCd=B01&tradeTypeCd=B1&rletNo=112008&cortarNo=1165010700&hscpTypeCd=B01%3AB02%3AB03&mapX=&mapY=&mapLevel=&page="
url2 <- "&articlePage=&ptpNo=&rltrId=&mnex=&bildNo=&articleOrderCode=&cpId=&period=&prodTab=&atclNo=&atclRletTypeCd=&location=2173&bbs_tp_cd=&sort=&siteOrderCode=&schlCd=&tradYy=&exclsSpc=&splySpcR=&cmplYn=#_content_list_target"

total_ds <- data.frame()
for (i in 1:10){
  url <- paste0(url1, i, url2)
  print(url)
  htm <- read_html(url)
  
  node <- html_nodes(htm, ".txt")
  txt1 <- html_text(node)
  
  node <- html_nodes(htm, ".align_r strong")
  txt2 <- html_text(node)
  
  node <- html_nodes(htm, ".title_agent")
  txt3 <- html_text(node)
  txt3 <- gsub("\n|\t", "", txt3)
  
  ds <- cbind(txt1, txt2, txt3)
  colnames(ds) <- c("장점", "가격", "부동산")
  ds
  
  total_ds <- rbind(total_ds, ds)
}


#오른쪽 맨위 찍어서 보여드리기
total_ds





#사이트 주소 따내는 방법
url <- "https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=1&ie=utf8&query=영화+베를린"
htm <- read_html(url)
node <- html_node(htm, ".sh_movie_link")
html_attr(node, 'href')






# HTTP Trace Chrome 활용
url <- "http://www.kobis.or.kr/kobis/business/mast/thea/findTheaterCodeList.do?pageIndex=9"
htm <- read_html(url)
node <- html_nodes(htm, "td:nth-child(3)")
txt <- html_text(node)
txt <- txt[-1]
code_list <- append(code_list, txt)
print(i)

