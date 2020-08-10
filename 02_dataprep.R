
#####
# DATA PREP
#####

# packages
packages <- c("tidyverse", "magrittr", "rtweet", "tidytext", "widyr",
              "quanteda", "SnowballC")
lapply(packages, library, character.only = TRUE)

# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/zonstiges/spielRei/coRona"
} #else if (Sys.info()["nodename"]=="..."){
#root.dir <- "C:/..."
#}

setwd(root.dir)

# load data
load("tweets.RData")

# tidy up text
corona_tweets_de_tidy <- corona_tweets_de %>%
  unnest_tokens(word, text)

corona_tweets_en_tidy <- corona_tweets_en %>%
  unnest_tokens(word, text)


# remove stop words
stopWords_de <- read.table("stopwords-de.txt", encoding = "UTF-8", colClasses=c('character'))
stopwords_de <- data.frame(word = stopwords("german")) %>%
  bind_rows(data.frame(word = c("https", "t.co", " ", "\n", "virus", stopWords_de$V1)))
remove(stopWords_de)

corona_tweets_de_tidy_reduc <- corona_tweets_de_tidy %>% anti_join(stopwords_de)

corona_tweets_en_tidy_reduc <- corona_tweets_en_tidy %>%
  filter(!word %in% c(stopwords(), "https", "t.co", " ", "\n", "virus"))

corona_tweets_de_tidy_reduc$word <- gsub("corona", NA, corona_tweets_de_tidy_reduc$word)
corona_tweets_de_tidy_reduc$word <- gsub("covid*", NA, corona_tweets_de_tidy_reduc$word)

corona_tweets_en_tidy_reduc$word <- gsub("corona", NA, corona_tweets_en_tidy_reduc$word)
corona_tweets_en_tidy_reduc$word <- gsub("covid*", NA, corona_tweets_en_tidy_reduc$word)


# remove numbers
corona_tweets_de_tidy_reduc <- corona_tweets_de_tidy_reduc[-grep("\\b\\d+\\b", corona_tweets_de_tidy_reduc$word),]

corona_tweets_en_tidy_reduc <- corona_tweets_en_tidy_reduc[-grep("\\b\\d+\\b", corona_tweets_en_tidy_reduc$word),]

# lemmatize (not working)
# corona_tweets_de_tidy_reduc$wordlem <- make_lemma_dictionary(corona_tweets_de_tidy_reduc$word,
  #                                                           engine = 'hunspell',
   #                                                          lang = "de") # German
#corona_tweets_en_tidy_reduc$wordlem <- make_lemma_dictionary(corona_tweets_en_tidy_reduc$word) # English

# stemming
corona_tweets_de_tidy_reduc$wordstem <- wordStem(corona_tweets_de_tidy_reduc$word, language = "de")

corona_tweets_en_tidy_reduc$wordstem <- wordStem(corona_tweets_en_tidy_reduc$word, language = "en")


# listwise deletion
DE_tweets_prep <- corona_tweets_de_tidy_reduc[!is.na(corona_tweets_de_tidy_reduc$wordstem),]

EN_tweets_prep <- corona_tweets_en_tidy_reduc[!is.na(corona_tweets_en_tidy_reduc$wordstem),]


remove(corona_tweets_de_tidy_reduc, corona_tweets_en_tidy_reduc, corona_tweets_de_tidy, corona_tweets_en_tidy)


# save
save.image("tweets.RData")

