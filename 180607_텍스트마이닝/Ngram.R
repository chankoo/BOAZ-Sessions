############################
##########n-gram############
##############################
rm(list=ls())

# install.packages("dplyr")
# install.packages("tidytext")
# install.packages("janeaustenr")

library(dplyr)
library(tidytext)
library(janeaustenr)

# tf-idf 에서도 unnest_tokens()를 사용했었음
# tf-idf 에서는 unnest_tokens()를 통해 text를 단어나 문장 단위로 tokenize하여 출현 빈도수를 파악하는 것이였다면
# n-gram은 단어간의 시퀀스를 고려
# unnest_tokens()에 token="ngrams"와 단어를 몇 개씩 쪼갤것인가를 정하는 n 옵션을 추가하면 된다.
# n=1 -> unigram , n=2 -> bigram , n=3 -> trigram 

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams


##counting and filtering n-grams

austen_bigrams %>%
  count(bigram, sort = TRUE)  #bigram 내림차순 정렬

library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_separated

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%  #separate 한 bigram 들의 불용어 제거
  filter(!word2 %in% stop_words$word)

bigrams_filtered

# new bigram counts:

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)        #불용어 제거한 bigram들 count!

bigram_counts

bigrams_united <- bigrams_filtered %>%  
  unite(bigram, word1, word2, sep = " ")  #나누었던 bigram들을 다시 합침

bigrams_united


austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%         # 단어를 세개씩 쪼개 trigram을 만든다
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%    # 위와 같은 과정으로, 단어를 나눈후 불용어 제거하고 count!
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


##Analyzing bigrams
##bigram을 이용하여 분석해보자

#street 이 가장 많이 포함된 bigram 을 찾고싶을 때
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)  #word2에 "street"이 포함된 bigram을 count


#tf-idf 가 높은 bigram을 찾고싶을 때
bigram_tf_idf <- bigrams_united %>%  #tf-idf 가 높은 bigram을 내림차순 정렬
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf


##Using bigrams to provide context in sentiment analysis
##감성분석 시 주의사항 : 문장안에서 부정적 의미로 쓰인 단어가, 개별 단어로서는 긍정적 의미를 가질 때
##ex) I am not happy and I do not like it  => 문장은 부정적이지만 단어로 count를 할때에 happy와 like는 긍정적 단어로 count된다

#얼마나 자주 단어들이 not과 같은 부정어 뒤에 오는지 파악

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")  #AFINN : lexicon which gives a numeric sentiment score for each word

AFINN

not_words <- bigrams_separated %>%                #sentiment 와 연관된 단어들 중 not 뒤에 가장 많이 나타난 단어를 정렬      
  filter(word1 == "not") %>%                       
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words


#어떠한 단어들이 얼마만큼 "잘못된 해석을 하게끔 기여하였는가" 를 알아보자 
#x축 : n(출현빈도수)*score,  y축 : not 뒤에 온 단어들
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  windows() +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

#결과를 확인하면
#not like와 not help가 잘못해석하게 만드는 가장 큰 요인들 : 문장이 실제 의미보다 더 긍정적인 것처럼 보이게 함
#not afriad와 not fail은 실제 의미보다 더 부정적이게 해석하게끔 만드는것을 확인 할 수 있다.


negation_words <- c("not", "no", "never", "without")   #not 이외의 다른 부정어도 분석해보자.

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()


# 시각화는 각자 해보세요~!
# ##Visualizing a network of bigrams with ggraph
# 
library(igraph)

# original counts
bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)