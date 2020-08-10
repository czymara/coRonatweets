
#####
# CO-OCCURANCE ANALYSIS
#####


# packages
packages <- c("tidyverse", "magrittr", "rtweet", "tidytext", "widyr", "quanteda", "influential", "ggraph", "cowplot")
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

# count words co-occuring within users
word_pairs_de <- DE_tweets_prep %>%
  pairwise_count(wordstem, user_id, sort = TRUE)

word_pairs_de

word_pairs_en <- EN_tweets_prep %>%
  pairwise_count(wordstem, user_id, sort = TRUE)

word_pairs_en



# which words occurs most often with china?
word_pairs_de %>%
  filter(str_detect(item1, "china"))

word_pairs_en %>%
  filter(str_detect(item1, "china"))

## phi coefficient: how much more likely is that either both word X and Y appear, or neither do, than that one appears without the other
# equivalent to Pearson correlation when applied to binary data
word_cors_de <- DE_tweets_prep %>%
  group_by(wordstem) %>%
  filter(n() >= 20) %>% # filter relatively common words
  pairwise_cor(wordstem, user_id, sort = TRUE)

word_cors_de

word_cors_en <- EN_tweets_prep %>%
  group_by(wordstem) %>%
  filter(n() >= 20) %>% # filter relatively common words
  pairwise_cor(wordstem, user_id, sort = TRUE)

word_cors_en


# which words correlates most often with antisemitisch
word_cors_de %>%
  filter(str_detect(item1, "china"))

word_cors_en %>%
  filter(str_detect(item1, "china"))



# plot as network
set.seed(1337)
network_de <- word_cors_de %>%
  filter(correlation > .7) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
# + labs(title = "German tweets")

set.seed(1337)
network_en <- word_cors_en %>%
  filter(correlation > .7) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
# +  labs(title = "English tweets")

corrgraphscomb <- plot_grid(network_de, network_en,
               labels=c("German tweets", "English tweets")
               )

title <- ggdraw() + draw_label("Word correlations > 0.7 on COVID-19 tweets", fontface='bold') # make title

plot_grid(title, corrgraphscomb, ncol=1, rel_heights=c(0.1, 1)) # add title

dev.copy(png,"corrs.png")
dev.off()


