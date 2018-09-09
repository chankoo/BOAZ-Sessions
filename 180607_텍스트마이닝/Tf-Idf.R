
############################
#####TF-IDF####################
##############################

# install.packages("dplyr")
# install.packages("janeaustenr")
# install.packages("tidytext")

library(dplyr)
library(janeaustenr)
library(tidytext)

#######################################
##unnest_tokens()에 대한 간단한 설명~~##
#########################################
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

text_df <- data_frame(line = 1:4, text = text)

text_df

text_df %>%
  unnest_tokens(word, text)   #ouput column : word // input column : text  #단어를 쪼개줌


#Jane Austen 의 소설에 어떤 단어들이 많이 쓰였는지 확인해보자
book_words <- austen_books() %>%
  
  unnest_tokens(word, text) %>%  #text들을 단어 1개씩 tokenize를 한 후 word 라는 output column에 저장
  
  count(book, word, sort = TRUE) %>%  #count(dataframe,vars)
  
  ungroup()

book_words


total_words <- book_words %>% 
  
  group_by(book) %>% 
  
  summarize(total = sum(n)) # book 별로 n 값을 합치고, 그 값을 total 에 저장


book_words <- left_join(book_words, total_words)

book_words

# n : the number of times that word is used in the book 
# total : total words in the book. 
# TF : n/total

# TF 시각화 

library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +     #x축 : TF , y축 : count 
  windows() + geom_histogram(show.legend = FALSE) + #TF 값이 큰 단어들(=자주 출현하는 단어들) 은 몇개 없고
  xlim(NA, 0.0009) +                                #TF 값이 작은 단어들(=자주 출현하지 않는 단어들) 은 많다
  facet_wrap(~book, ncol = 2, scales = "free_y")    #또한 책별로 그래프의 모양이 비슷하다!


###############################################
#bind_tf_idf 함수를 통해 TF-IDF 더 쉽게 구현###
###############################################



book_words <- book_words %>%   #위에서 구구절절히 썼던 것을 bind_tf_idf를 통해 한번에 구현할 수 있다.
  
  bind_tf_idf(word, book, n)   #bind_df_idf( terms/tokens, document, counts)
#total 값을 계산해주므로 tf, idf, tf-idf 값 까지 출력된다
#TF값이 높을수록 많이 나오는 것 
#DF값이 높을수록 자주나오고 안중요한 것
#IDF값이 높을수록 자주 안나오고 중요한것

book_words


book_words %>%
  
  select(-total) %>%
  
  arrange(desc(tf_idf))  #tf-idf 를 기준으로 내림차순 정렬


#tf-idf 시각화

library(ggplot2)

plot_austen <- book_words %>%
  
  arrange(desc(tf_idf)) %>%
  
  mutate(word = factor(word, levels = rev(unique(word))))


plot_austen %>% 
  
  group_by(book) %>% #책별로
  
  top_n(15) %>% #tf-idf 값이 높은 단어들을 상위 15개 까지 뽑음
  
  ungroup %>%
  
  ggplot(aes(word, tf_idf, fill = book)) +
  
  geom_col(show.legend = FALSE) +
  
  labs(x = NULL, y = "tf-idf") +
  windows() +
  
  facet_wrap(~book, ncol = 2, scales = "free") +
  
  coord_flip()
