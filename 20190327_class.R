install.packages("tidytext")
library(tidytext) 
library(stringr) 
library(tidyverse)

folder = "aclImdb/" #aclImdb is a folder in your working directory
fnames = list.files(folder, recursive = T) #get all filenames under aclImdb
#we are only interested in the txt files under the neg/pos folder
fnames=fnames[str_detect(fnames, "/(neg|pos)/.+txt")] 
fnames[1:50]
fnames[1]
length(fnames)

install.packages("doParallel")
library(foreach) 
library(doParallel)
n_core = parallel::detectCores() 
registerDoParallel(n_core) #initiate a parallel cluster
# read files into R in parallel
txts = foreach(i = 1:length(fnames), .combine = c, .packages = "tidyverse") %dopar% 
  { read_file(str_c(folder, fnames[i]))
  }
head(txts)
df <- data_frame(fname = fnames, text = txts)
df <- df %>% separate(fname,
                     into=c("type", "polarity", "review_id", "rating", "ext"), 
                     sep="/|_|\\.")

df = df %>%
  mutate(doc_id = str_c(type, polarity, review_id, sep = "_")) %>% 
  mutate(text = str_replace_all(text, "(<br />)+", " "),
        review_id = as.numeric(review_id),
        rating = as.numeric(rating)) %>% 
  select(type, polarity, review_id, doc_id, text)


head(df)

tokens <- df %>%
  unnest_tokens(output = word, input = text) 
tokens %>%
  count(word, sort = TRUE)

temp <- tokens %>%
  group_by(polarity, doc_id) %>%
  summarise(n_tokens = n())

temp2 <- temp %>%
  group_by(polarity) %>%
  summarise(mean_n_tokens = mean(n_tokens))

head(tokens)

get_stopwords()

cleaned_tokens <- tokens %>% anti_join(get_stopwords())

nums <- cleaned_tokens %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
nums

cleaned_tokens <- cleaned_tokens %>% anti_join(nums, by = "word")

length(unique(cleaned_tokens$word))

cleaned_tokens %>%
  count(word, sort = T) %>%
  rename(word_freq = n) %>%
  ggplot(aes(x=word_freq)) +
  geom_histogram(aes(y=..count..), color="black", fill="blue", alpha=0.3) + 
  scale_x_continuous(breaks=c(0:5,10,100,500,10e2,10e3), 
                     trans="log1p", expand=c(0,0)) + 
  scale_y_continuous(breaks=c(0,100,1000,5e3,10e3,5e4,10e4,4e4), expand=c(0,0)) + theme_bw()

rare <- cleaned_tokens %>%
  count(word) %>%
  filter(n<10) %>% #remove words appearing in less than 10 reviews 
  select(word) %>% unique()
rare

cleaned_tokens <- cleaned_tokens %>% anti_join(rare, by = "word")
length(unique(cleaned_tokens$word))

install.packages("wordcloud")
library(wordcloud)
# define a nice color palette
pal <- brewer.pal(8,"Dark2")
# plot the 100 most common words
cleaned_tokens %>%
  count(word) %>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))

get_sentiments("nrc")

sent_reviews = cleaned_tokens %>% 
  left_join(get_sentiments("nrc")) %>% 
  rename(nrc = sentiment) %>% 
  left_join(get_sentiments("bing")) %>% 
  rename(bing = sentiment) %>% 
  left_join(get_sentiments("afinn")) %>% 
  rename(afinn = score)

sent_reviews

bing_word_counts <- sent_reviews %>% 
  filter(!is.na(bing)) %>% 
  count(word, bing, sort = TRUE)
bing_word_counts

bing_word_counts %>%
  filter(n > 10000) %>%
  mutate(n = ifelse(bing == "negative", -n, n)) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = bing)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

review_sentences <- df %>%
  unnest_tokens(output = sentence, input = text, token = "sentences")
review_sentences

install.packages("sentimentr")
library(sentimentr)
sentiment('I am not very happy. He is very happy')

df_annotated <- read_csv("df_annotated.csv")

neg_annotated <-df_annotated %>%
  filter(doc_id == "test_neg_0") %>%

stopImplicitCluster() #remember to stop the cluster if you don't need it anymore