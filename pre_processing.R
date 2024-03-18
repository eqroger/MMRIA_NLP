# Install & Load Necessary Packages ------------------------------------------------------------------------

library(pacman)
pacman::p_load("tm", "stringr", "SnowballC", "lsa", "dplyr", "wordcloud", "ngram", 
               "tidytext", "ggeasy", "ggplot2", "MASS", "topicmodels")

# Read in Data ---------------------------------------------------------------------------------------------
setwd("/Users/erinrogers/Library/CloudStorage/OneDrive-EmoryUniversity/Desktop/Research_Projects/TADA/")
df <- read.csv("./TADA_Dataset_Oct2023.csv")

# Pre-Process Relevant Text Columns -----------------------------------------------------------------------
recs <- df$crcfw_c_recom #subset relevant text fields
corpus <- VCorpus(VectorSource(recs)) #transform to relevant data structure
corpus <- tm_map(corpus, content_transformer(tolower)) #lowercase
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
corpus <- tm_map(corpus, removeWords, stopwords("english")) #remove English language stopwords
length(corpus)

# Generate N-Grams ------------------------------------------------------------
NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 1:3), paste, collapse = "_"), use.names = FALSE)
}
n_gram_corpus <- tm_map(corpus, content_transformer(NLP_tokenizer))

length(n_gram_corpus)
n_gram_corpus[[2]]$content

#Analyzing N-Grams -----------------------------------------------------------------------------------------
corpus_df <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)

corpus_ngrams <- corpus_df %>% 
  unnest_tokens(ngrams, text, token="ngrams", n=3)

corpus_ngrams <- corpus_ngrams %>% 
  count(ngrams) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop)) %>% 
  mutate(id = row_number()) %>%
  filter(ngrams != "NA")

top_ten <- corpus_ngrams %>% 
  filter(id %in% c(1:10))

b <- ggplot(top_ten, aes(x=reorder(ngrams, -n), y = n)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_x_discrete(name = 'Trigrams') + 
  scale_y_continuous(name = "Count of Trigrams") +
  ggtitle("Top 10 Trigrams Identified in Recommendations") +
  ggeasy::easy_center_title()

b

# Word Clouds-----------------------------------------------------------------------------------------
doc_term_matrix <- DocumentTermMatrix(corpus)
wc1 <- wordcloud(corpus, min.freq = 30, max.words = 100, random.order=FALSE, colors="black", vfont=c("sans serif", "plain"))

# N-Grams sans 'Substance' 'use' 'disorder'
sud <- data.frame(word = c("substance", "use", "disorder")) #smack
in_lexicon <- df %>%
  tidytext::unnest_tokens(output = 'words', input = crcfw_c_recom) %>%
  dplyr::mutate(
    in_lexicon = !(words %in% sud$word)
  ) 

# EVALUATING BY CONTRIBUTING FACTOR LEVEL

#
df <- df %>% distinct(X_id, crcfw_c_recom, .keep_all = TRUE)

df$crcfw_c_recom <- gsub("substance use", "", df$crcfw_c_recom, ignore.case = TRUE)

df_pat_fam <- df %>% 
  filter(Contributing.Factor.Level == 'Patient/Family') %>% 
  select(crcfw_c_recom) %>% 
  filter(crcfw_c_recom != '')

recs <- df_pat_fam$crcfw_c_recom

corpus <- VCorpus(VectorSource(recs)) #transform to relevant data structure
corpus <- tm_map(corpus, content_transformer(tolower)) #lowercase
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
corpus <- tm_map(corpus, removeWords, stopwords("english")) #remove English language stopwords
length(corpus)

#Analyzing N-Grams -----------------------------------------------------------------------------------------
corpus_df <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)

corpus_ngrams <- corpus_df %>% 
  unnest_tokens(ngrams, text, token="ngrams", n=3)

corpus_ngrams <- corpus_ngrams %>% 
  count(ngrams) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop)) %>% 
  mutate(id = row_number()) %>%
  filter(ngrams != "NA")

top_ten <- corpus_ngrams %>% 
  filter(id %in% c(1:10))

b <- ggplot(top_ten, aes(x=reorder(ngrams, -n), y = n)) + 
  geom_bar(stat="identity") + 
  theme_minimal() +  # Use a minimal theme as a starting point
  theme(
    panel.background = element_rect(fill = "white"),  # Set background color to white
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  ) +
  scale_x_discrete(name = 'Trigrams') + 
  scale_y_continuous(name = "Count of Trigrams") +
  ggtitle("Top 10 Trigrams Identified in Recommendations") +
  labs(subtitle = "Contributing Factor Level: Patient/Family") +
  ggeasy::easy_center_title()

b + theme(plot.subtitle = element_text(hjust = 0.5))


# Word Clouds-----------------------------------------------------------------------------------------
doc_term_matrix <- DocumentTermMatrix(corpus)
wc1 <- wordcloud(corpus, min.freq = 30, max.words = 100, random.order=FALSE, colors="black", vfont=c("sans serif", "plain"))



