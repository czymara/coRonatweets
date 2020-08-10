
#####
# N-GRAMS
#####


# packages
packages <- c("tidyverse", "magrittr", "rtweet", "tidytext", "widyr", "quanteda", "influential", "ggraph", "cowplot", "SnowballC")
lapply(packages, library, character.only = TRUE)

# dir
if (Sys.info()["nodename"]=="DBSFCL2"){
  root.dir <- "C:/Users/czymara.local/PowerFolders/lehrstuhlstuff/Corona Survey/"
} #else if (Sys.info()["nodename"]=="..."){
#root.dir <- "C:/..."
#}

setwd(root.dir)

# load data
load("coRona2/in/data.RData")

### create tidy text
data_priv <- data[ which(data$OF01_01 != ""), ]

answers_priv <- tibble(id = data_priv$CASE,
                       private = data_priv$lialone,
                       text = as.character(data_priv$OF01_01))

privat_lonely_tidy <- answers_priv %>% unnest_tokens(word, text)

# stop words
stopWords_de <- read.table("coRona2/in/stopwords-de.txt", encoding = "UTF-8", colClasses=c('character'))

privat_lonely_tidy_reduc <- privat_lonely_tidy %>% filter(!word %in% c(stopWords_de$V1))

# remove numbers
privat_lonely_tidy_reduc <- privat_lonely_tidy_reduc[-grep("\\b\\d+\\b", privat_lonely_tidy_reduc$word),]

# stemming
privat_lonely_tidy_reduc$wordstem <- wordStem(privat_lonely_tidy_reduc$word, language = "de")


# listwise deletion
privat_lonely_tidy <- privat_lonely_tidy_reduc[!is.na(privat_lonely_tidy_reduc$wordstem),]



#### n-grams

# use not single words but bi-grams
private_bigrams <- answers_priv %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# clean
# erst zerschneiden...
bigrams_separated <- private_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
# stopwords
private_bigrams_reduc <- bigrams_separated %>%
  filter(!word1 %in% stopWords_de$V1) %>%
  filter(!word2 %in% stopWords_de$V1)
# remove numbers
private_bigrams_reduc <- private_bigrams_reduc[-grep("\\b\\d+\\b", private_bigrams_reduc$word1),]
private_bigrams_reduc <- private_bigrams_reduc[-grep("\\b\\d+\\b", private_bigrams_reduc$word2),]
# stemming
private_bigrams_reduc$word1stem <- wordStem(private_bigrams_reduc$word1, language = "de")
private_bigrams_reduc$word2stem <- wordStem(private_bigrams_reduc$word2, language = "de")
# listwise deletion
privat_lonely_tidy <- private_bigrams_reduc[!is.na(private_bigrams_reduc$word1stem),]
privat_lonely_tidy <- private_bigrams_reduc[!is.na(private_bigrams_reduc$word2stem),]
# ... dann wieder zusammenfuehren
private_bigrams_reduc <- privat_lonely_tidy %>%
  unite(bigram, word1, word2, sep = " ")


# which words occur most often in pairs
bigram_counts <- private_bigrams_reduc %>%
  count(word1stem, word2stem, sort = T)

bigram_counts


private_bigrams_reduc %>%
  filter(word1stem == "sozial") %>%
  count(word2stem, word2stem, sort = TRUE)


# verneinungen
private_bigrams_reduc %>%
  filter(word1stem == "nicht") %>%
  count(word2stem, word2stem, sort = TRUE) # interestingly hardly any


