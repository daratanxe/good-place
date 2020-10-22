
library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(topicmodels)
library(kableExtra)
library(reshape2)
library(wordcloud)

  # load data into tibbles ----

scripts <- tibble()
for (i in seq_len(52)) {
  scripts <-
    bind_rows(scripts,
              tibble(i, read_csv(str_c("chapter", as.character(i), ".csv"),
                                 col_types = "cc")))
}
scripts <- scripts %>%
  rename(chapter = i, character = Character, line = Line)

characters <- read_csv("characters.csv", col_types = "cccc")

  # data wrangling and preparation ----

    # separate 'character' in scripts for two-character observations ----

scripts <- scripts %>%
  mutate(character =
           str_replace(str_replace(character, "\\n", ""), ", ", ",")) %>%
  separate(character, into = c("character1", "character2"), sep = ",") %>%
  pivot_longer(c(character1, character2),
               names_to = "role", values_to = "character") %>%
  filter(!is.na(character)) %>%
  select(chapter, character, line)

    # update stop_words with character and colloquial words ----

custom_stop_words <- stop_words %>%
  bind_rows(tibble(word = (characters %>%
                             unnest_tokens(word, stop_word) %>%
                             filter(!is.na(word)) %>%
                             unique())$word,
                   lexicon = "character")) %>%
  bind_rows(tibble(word =
                     c("ah", "aw", "barg", "blah", "braap", "dude", "eh", "em",
                       "ew", "ga", "gonna", "gotta", "guys", "ha", "hey",
                       "hmm", "huh", "kay", "kinda", "mm", "mmm", "ooh", "ow",
                       "ugh", "uh", "um", "wanna", "whoa", "wow", "y'all",
                       "ya", "yadda", "yeah", "yep", "yo", "yup"),
                   lexicon = "colloquial"))

  # sentiment analysis ----

sentiment_tokens <- scripts %>%
  unnest_tokens(word, line) %>%
  anti_join(custom_stop_words, by = "word") %>%
  inner_join(characters, by = "character") %>%
  filter(affiliation == "Soul Squad")

    # analysis with afinn: soul squad as a group ----

sentiment_afinn <- sentiment_tokens %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(chapter) %>%
  summarize(sentiment = sum(value)) %>%
  left_join(sentiment_tokens %>% count(chapter), by = "chapter") %>%
  mutate(weighted_sentiment = sentiment / n)

sentiment_afinn %>%
  ggplot() + geom_col(aes(chapter, weighted_sentiment,
                          fill = sentiment > 0), show.legend = FALSE) +
  scale_fill_manual(values = c("firebrick", "forestgreen")) +
  labs(x = "Chapter", y = "Weighted Sentiment",
       title = "Soul Squad's Sentiments by Chapter") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_vline(xintercept = c(13.5, 26.5, 39.5), linetype = "dashed")

    # analysis with nrc: individual soul squad members ----

sentiment_nrc <- sentiment_tokens %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment != "positive", sentiment != "negative") %>%
  left_join(tibble(sentiment = c("anger", "anticipation", "disgust", "fear",
                                 "joy", "sadness", "surprise", "trust"),
                   sentiment_factor =
                     factor(c("Anger", "Anticipation", "Disgust", "Fear",
                              "Joy", "Sadness", "Surprise", "Trust"),
                            levels =
                              c("Anger", "Anticipation", "Disgust", "Joy",
                                "Fear", "Surprise", "Sadness", "Trust"))),
            by = "sentiment") %>%
  count(group, chapter, sentiment_factor) %>%
  group_by(chapter) %>%
  mutate(weighted_n = n / (sentiment_tokens %>% count(chapter))$n[chapter])

sentiment_nrc %>%
  filter(group == "GROUP") %>%
  ggplot() + geom_col(aes(chapter, weighted_n, fill = sentiment_factor),
                      show.legend = FALSE) +
  scale_fill_manual(values = c("palevioletred", "darkseagreen", "firebrick",
                               "seagreen", "violetred", "mediumseagreen",
                               "darkred", "darkgreen")) +
  labs(x = "Chapter", y = "Proportion of Tokenized Words",
       title = "GROUP's Sentiments by Chapter") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  facet_wrap(~ sentiment_factor, ncol = 2) +
  geom_vline(xintercept = c(13.5, 26.5, 39.5), linetype = "dashed")

  # frequency analysis ----

    # differentiating similar character types: versions of janet ----

frequency_unigrams %>%
  filter(group %in% c("Bad Janet", "Janet", "Neutral Janet")) %>%
  left_join(tibble(group = c("Bad Janet", "Janet", "Neutral Janet"),
                   type = factor(c("Bad", "Good", "Neutral"),
                                 levels = c("Bad", "Neutral", "Good"))),
            by = "group") %>%
  count(type, word) %>%
  bind_tf_idf(word, type, n) %>%
  group_by(type) %>%
  arrange(desc(tf_idf)) %>%
  filter(row_number() <= 10) %>%
  ggplot() + geom_col(aes(fct_reorder(word, tf_idf), tf_idf, fill = type)) +
  scale_fill_discrete(type = c("darkgray", "tan", "thistle"), name = "Type") + 
  labs(x = "Unigram", y = "TF-IDF Value", title = "Unigrams by Janet Type") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  coord_flip() +
  facet_wrap(~ type, ncol = 3, scales = "free")

    # identifying characters with unique phrases ----

frequency_bigrams <- scripts %>%
  inner_join(characters, by = "character") %>%
  filter(affiliation == "Soul Squad") %>%
  unnest_tokens(phrase, line, token = "ngrams", n = 2) %>%
  separate(phrase, c("word1", "word2"), sep = " ") %>%
  filter(!(word1 %in% custom_stop_words$word),
         !(word2 %in% custom_stop_words$word)) %>%
  count(group, word1, word2) %>%
  unite(phrase, word1, word2, sep = " ") %>%
  bind_tf_idf(phrase, group, n)

frequency_bigrams %>%
  arrange(desc(tf_idf)) %>%
  filter(row_number() <= 10) %>%
  ggplot() + geom_col(aes(fct_reorder(phrase, tf_idf), tf_idf, fill = group)) +
  scale_fill_discrete(type = c("palevioletred", "cornflowerblue",
                               "mediumaquamarine"),
                      name = "Character") +
  labs(x = "Bigram", y = "TF-IDF Value",
       title = "Soul Squad Bigrams by Character") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  coord_flip()

  # pairwise analysis ----

pairwise_characters <- characters %>%
  inner_join(scripts, by = "character") %>%
  count(group, sort = TRUE) %>%
  filter(row_number() <= 15) %>%
  left_join(characters, by = "group")

pairwise_graph <- scripts %>%
  mutate(scene = row_number() %/% 40) %>%
  inner_join(pairwise_characters, by = "character") %>%
  pairwise_cor(group, scene, sort = TRUE) %>%
  filter(row_number() %% 2 == 1) %>%
  filter(correlation > 0.05) %>%
  graph_from_data_frame()

set.seed(1)
pairwise_graph %>%
  ggraph(layout = "gem") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_label(aes(label = name), size = 3.5,
                  color = c(rep("darkgreen", 2), rep("darkblue", 2),
                            "darkmagenta", "firebrick", rep("darkblue", 2),
                            "darkgreen", rep("firebrick", 2), "darkmagenta",
                            rep("darkblue", 2), "firebrick")) +
  labs(title = "Character Pairwise Correlation") +
  theme(panel.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold", hjust = 0.5))

  # topic modeling ----

topic_dtm <- scripts %>%
  inner_join(characters, by = "character") %>%
  filter(!is.na(affiliation)) %>%
  mutate(affiliation =
           ifelse(affiliation == "Soul Squad", group, affiliation)) %>%
  unnest_tokens(word, line) %>%
  anti_join(custom_stop_words, by = "word") %>%
  group_by(word) %>%
  mutate(total_n = n()) %>%
  filter(total_n < 150) %>%
  ungroup() %>%
  count(affiliation, word, name = "affiliation_n") %>%
  cast_dtm(affiliation, word, affiliation_n)

topic_model <- topic_dtm %>%
  LDA(k = 10, control = list(seed = 3))

    # beta: most common words by topic ----

topic_beta <- topic_model %>%
  tidy(matrix = "beta") %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

topic_beta %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot() + geom_col(aes(term, beta,
                          fill = factor(topic)), show.legend = FALSE) +
  scale_fill_manual(values =
                      c("palevioletred", "sandybrown", "darkseagreen",
                        "cornflowerblue", "mediumpurple", "firebrick",
                        "peru", "seagreen", "royalblue", "darkorchid")) +
  labs(x = "Beta Value", y = "Word", title = "Most Common Words by Topic") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  coord_flip() +
  facet_wrap(~ topic, nrow = 2, scales = "free") +
  scale_x_reordered()

    # gamma: topic by character or affiliation ----

topic_gamma <- topic_model %>%
  tidy(matrix = "gamma")

topic_gamma %>%
  filter(gamma > 0.01) %>%
  kable(col.names = c("Document", "Topic", "Gamma")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE,
                font_size = 13) %>%
  row_spec(c(2, 3), background = "cornsilk") %>%
  row_spec(c(4, 9), background = "papayawhip")
  
    # visualizing topics of interest ----

set.seed(1)
augment(topic_model, data = topic_dtm) %>%
  filter(.topic == 3 | .topic == 8) %>%
  group_by(.topic) %>%
  arrange(desc(count)) %>%
  filter(row_number() <= 50) %>%
  acast(term ~ .topic, value.var = "count", fill = 0) %>%
  comparison.cloud(scale = c(2.5, 0.5),
                   colors = c("darkseagreen", "darkgreen"),
                   title.size = 2, title.bg.colors = "white")

set.seed(1)
augment(topic_model, data = topic_dtm) %>%
  filter(.topic == 2) %>%
  group_by(document) %>%
  arrange(desc(count)) %>%
  filter(row_number() <= 50) %>%
  acast(term ~ document, value.var = "count", fill = 0) %>%
  comparison.cloud(scale = c(3, 0.5), colors = c("darkgreen", "darkmagenta"),
                   title.size = 2, title.bg.colors = "white")
