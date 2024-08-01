
# Install & Load Necessary Packages ------------------------------------------------------------------------

install.packages("pacman")

library(pacman)
pacman::p_load(ggplot2, 
               ggeasy, 
               tm,
               dplyr, 
               tidyr, 
               topicmodels, 
               wordcloud, 
               stringr, 
               SnowballC, 
               lsa, 
               ngram,
               tidytext,
               quanteda)

# Read in Data ---------------------------------------------------------------------------------------------
setwd("/Users/carolinebarry/Documents/Emory/Emory PhD/TADA/CDC Rotation/")
df <- read.csv("./TADA_Dataset_Oct2023.csv")

# Pre-Process Relevant Text Column(s) -----------------------------------------------------------------------
df <- df %>% distinct(X_id, crcfw_c_recom,
                      .keep_all=TRUE) # Keep all columns while retaining distinct rows based on X_id and rec

df$Underlying.Cause.of.Death.Category..Reported <- gsub("Hemorrhage (Excludes Aneurysms or CVA)", 
                                                        "Hemorrhage", 
                                                        df$Underlying.Cause.of.Death.Category..Reported)
df$Underlying.Cause.of.Death.Category..Reported <- gsub("HemorrhageVA)", 
                                                        "Hemorrhage", 
                                                        df$Underlying.Cause.of.Death.Category..Reported)


dim(df)

recs <- df$crcfw_c_recom #subset relevant text fields
corpus <- VCorpus(VectorSource(recs)) #transform to relevant data structure
corpus <- tm_map(corpus, content_transformer(tolower)) #lowercase
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
custom_stopwords <- c("care", "disorder", "disorders", "ensure", "health", "include", 
                      "including", "medical", "need", "patient", "patients", "people", 
                      "postpartum", "pregnant", "pregnancy", "provide", "provider", 
                      "providers", "recommend", "service", "services", "substance", 
                      "substance use disorder", "sud", "treatment", "use", "woman", "women")      # Define custom stopwords if needed
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), custom_stopwords)) #remove stopwords
corpus <- tm_map(corpus, stemDocument) #stem words

#Analyzing N-Grams -----------------------------------------------------------------------------------------
corpus_df <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)

# n = # of n-grams (for trigrams n = 3)
corpus_trigrams <- corpus_df %>% 
  unnest_tokens(ngrams, text, token="ngrams", n=3) 

# generates a data frame with ngrams by frequency
corpus_trigrams <- corpus_trigrams %>% 
  count(ngrams) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop)) %>% 
  mutate(id = row_number()) %>%
  filter(ngrams != "NA")

#subsetting to the top ten n-grams
top_ten_trigrams <- corpus_trigrams %>% 
  filter(id %in% c(1:10))

#bar plot depicting the top-ten n-grams
b3 <- ggplot(top_ten_trigrams, aes(x=reorder(ngrams, -n), y = n)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_x_discrete(name = 'Trigrams') + 
  scale_y_continuous(name = "Count of Trigrams") +
  ggtitle("Top 10 Trigrams Identified in Recommendations") +
  ggeasy::easy_center_title()

# repeat above for pentagrams
corpus_pentagrams <- corpus_df %>% 
  unnest_tokens(ngrams, text, token="ngrams", n=5)

# generates a data frame with pentagrams by frequency
corpus_pentagrams <- corpus_pentagrams %>% 
  count(ngrams) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop)) %>% 
  mutate(id = row_number()) %>%
  filter(ngrams != "NA")

#subsetting to the top ten n-grams
top_ten_pentagrams <- corpus_pentagrams %>% 
  filter(id %in% c(1:10))

#bar plot depicting the top-ten n-grams
b5 <- ggplot(top_ten_pentagrams, aes(x=reorder(ngrams, -n), y = n)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_x_discrete(name = 'Pentagrams') + 
  scale_y_continuous(name = "Count of Pentagrams") +
  ggtitle("Top 10 Pentagrams Identified in Recommendations") +
  ggeasy::easy_center_title()

# Word Clouds-----------------------------------------------------------------------------------------
doc_term_matrix <- DocumentTermMatrix(corpus)

# Define a custom color palette
custom_colors <- brewer.pal(10, "Set1")

# Generate the word cloud with customizations
wc1 <- wordcloud(corpus, min.freq = 30, max.words = 100, random.order = FALSE, 
                 colors = custom_colors, vfont = c("sans serif", "bold"))

wc1 <- wordcloud(corpus, min.freq = 30, max.words = 100, random.order=FALSE, colors="black", vfont=c("sans serif", "plain"))

# Term Frequency-Inverse Document Frequency (tf-idf)
# tf-idf reflects the importance of a term (word) in a document relative to a collection of documents (corpus). 
# tf-idf is commonly used in to identify the significance of words.

# Set recs as the relevant text field

recs <- df$crcfw_c_recom #subset relevant text fields
corpus <- VCorpus(VectorSource(recs)) #transform to relevant data structure
corpus <- tm_map(corpus, content_transformer(tolower)) #lowercase
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
corpus <- tm_map(corpus, removeWords, stopwords("english")) #remove stopwords
corpus <- tm_map(corpus, stemDocument) #stem words

# Create a document-term matrix
recs_dtm <- DocumentTermMatrix(corpus)

# Calculate TF-IDF
recs_tfidf <- weightTfIdf(recs_dtm)

# Convert TF-IDF matrix to a data frame
recs_tfidf_df <- as.data.frame(as.matrix(recs_tfidf))

# View the TF-IDF data frame
print(recs_tfidf_df)

# Calculate the sum of TF-IDF scores for each document
doc_tfidf_sum <- rowSums(as.matrix(recs_tfidf))

# Calculate the average TF-IDF score for each document
doc_tfidf_avg <- rowMeans(as.matrix(recs_tfidf))

# Identify top terms with a threshold (e.g., sum of TF-IDF > 1)
top_terms_sum <- colnames(as.matrix(recs_tfidf))[apply(as.matrix(recs_tfidf), 2, function(x) sum(x) > 1)]

# Get the sum of TF-IDF scores for each term
term_sum <- apply(as.matrix(recs_tfidf), 2, sum)

# Create a data frame with terms and their sums
term_sum_df <- data.frame(Term = colnames(as.matrix(recs_tfidf)), Sum_TFIDF = term_sum)

# Order the data frame by sum of TF-IDF in descending order
term_sum_df <- term_sum_df[order(-term_sum_df$Sum_TFIDF), ]

# Select the top 10 terms
top_terms_sum <- head(term_sum_df$Term, 10)

# Create a bar plot for the top 10 terms based on sum of TF-IDF
barplot(term_sum_df$Sum_TFIDF[1:10], names.arg = term_sum_df$Term[1:10], col = rainbow(10), main = "Top 10 Terms (Sum of TF-IDF)", las = 2)

# Identify top terms with a threshold (e.g., maximum TF-IDF > 0.2)
top_terms_max <- colnames(as.matrix(recs_tfidf))[apply(as.matrix(recs_tfidf), 2, function(x) max(x) > 0.2)]

# Get the maximum TF-IDF score for each term
term_max <- apply(as.matrix(recs_tfidf), 2, max)

# Create a data frame with terms and their maximum TF-IDF scores
term_max_df <- data.frame(Term = colnames(as.matrix(recs_tfidf)), Max_TFIDF = term_max)

# Order the data frame by maximum TF-IDF in descending order
term_max_df <- term_max_df[order(-term_max_df$Max_TFIDF), ]

# Select the top 10 terms
top_terms_max <- head(term_max_df$Term, 20)

# Create a bar plot for the top 10 terms based on maximum TF-IDF
barplot(term_max_df$Max_TFIDF[1:20], names.arg = term_max_df$Term[1:20], col = rainbow(10), main = "Top 20 Terms (Maximum TF-IDF)", las = 2)


#####
#####

## check freqeuncies of underlying cause of death and contributing factor class
frequency_table_cod <- table(df$Underlying.Cause.of.Death.Category..Reported)
frequency_table_contrfactor <- table(df$Contributing.Factor.Class)
frequency_table_contrfactorlev <- table(df$Contributing.Factor.Level)

# Sort the tables in descending order
sorted_frequency_table_cod <- sort(frequency_table_cod, decreasing = TRUE)
sorted_frequency_table_contrfactor <- sort(frequency_table_contrfactor, decreasing = TRUE)
sorted_frequency_table_contrfactorlev <- sort(frequency_table_contrfactorlev, decreasing = TRUE)

# Print the sorted tables
print(sorted_frequency_table_cod)
print(sorted_frequency_table_contrfactor)
print(sorted_frequency_table_contrfactorlev)

#####
#####

# Create a subset based on the specified condition

# options for subsetting

## replace what's in quotes with the underlying cause of death or contributing factor you're interested in
## the top causes of death were: Mental Health Conditions, Cardiomyopathy, Cardiovascular Conditions,
## Hemorrhage (Excludes Aneurysms or C, Hemorrhage (Excludes Aneurysms or CVA), Infection

# subset by COD = mental health conditions
subset_data <- df[df$Underlying.Cause.of.Death.Category..Reported. %in% c("Mental Health Conditions"), ]

# subset by COD = cardiomyopathy
## subset_data <- df[df$Underlying.Cause.of.Death.Category..Reported. %in% c("Cardiomyopathy"), ]

# subset by COD = cardiovascular conditions
## subset_data <- df[df$Underlying.Cause.of.Death.Category..Reported. %in% c("Cardiovascular Conditions"), ]

# subset by COD = hemorrhage
## subset_data <- df[df$Underlying.Cause.of.Death.Category..Reported. %in% c("Hemorrhage (Excludes Aneurysms or C", "Hemorrhage (Excludes Aneurysms or CVA)"), ]

# subset by COD = infection
## subset_data <- df[df$Underlying.Cause.of.Death.Category..Reported. %in% c("Infection"), ]

## the top contributing factors were: Substance Use Disorder- Alcohol, Illicit/Prescription Drugs,
## Continuity of Care/Care Coordination, Clinical Skill/Quality of Care, Mental Health Conditions, 
## Access/Financial

# subset by contributing factor = Substance Use Disorder- Alcohol, Illicit/Prescription Drugs
## subset_data <- df[df$Contributing.Factor.Class %in% c("Substance Use Disorder- Alcohol, Illicit/Prescription Drugs"), ]

# subset by contributing factor = Continuity of Care/Care Coordination
## subset_data <- df[df$Contributing.Factor.Class %in% c("Continuity of Care/Care Coordination"), ]

# subset by contributing factor = Clinical Skill/Quality of Care
## subset_data <- df[df$Contributing.Factor.Class %in% c("Clinical Skill/Quality of Care"), ]

# subset by contributing factor = Mental Health Conditions
## subset_data <- df[df$Contributing.Factor.Class %in% c("Mental Health Conditions"), ]

# subset by contributing factor = Access/Financial
## subset_data <- df[df$Contributing.Factor.Class %in% c("Access/Financial"), ]

# subset by contributing factor = Discrimination
## subset_data <- df[df$Contributing.Factor.Class %in% c("Discrimination"), ]

# subset by contributing factor = Social Support/Isolation
## subset_data <- df[df$Contributing.Factor.Class %in% c("Social Support/Isolation"), ]

# subset by contributing factor level = Patient/Family
## subset_data <-df[df$Contributing.Factor.Level %in% c("Patient/Family"), ]

# subset by contributing factor level = Provider
## subset_data <-df[df$Contributing.Factor.Level %in% c("Provider"), ]

# subset by contributing factor level = Facility
## subset_data <-df[df$Contributing.Factor.Level %in% c("Facility"), ]

# subset by contributing factor level = System
## subset_data <-df[df$Contributing.Factor.Level %in% c("System"), ]

# subset by contributing factor level = Community
## subset_data <-df[df$Contributing.Factor.Level %in% c("Community"), ]

# check dimensions--these should match the frequency counts from above
dim(subset_data)

# Preprocess the text column in the subset
recs_subset <- subset_data$crcfw_c_recom
corpus_subset <- VCorpus(VectorSource(recs_subset))
corpus_subset <- tm_map(corpus_subset, content_transformer(tolower))
corpus_subset <- tm_map(corpus_subset, removePunctuation)
custom_stopwords <- c("care", "disorder", "disorders", "ensure", "health", "include", 
                      "including", "medical", "need", "patient", "patients", "people", 
                      "postpartum", "pregnant", "pregnancy", "provide", "provider", 
                      "providers", "recommend", "service", "services", "substance", 
                      "substance use", "sud", "treatment", "use", "woman", "women")     
                      # Define custom stopwords if needed
corpus_subset <- tm_map(corpus, removeWords, c(stopwords("english"), custom_stopwords)) #remove stopwords
corpus_subset <- tm_map(corpus_subset, stemDocument)

# Analyzing Trigrams for the subset
corpus_subset_df <- data.frame(text = sapply(corpus_subset, as.character), stringsAsFactors = FALSE)
corpus_trigrams_subset <- corpus_subset_df %>% 
  unnest_tokens(ngrams, text, token = "ngrams", n = 3) 

# Generates a data frame with trigrams by frequency for the subset
corpus_trigrams_subset <- corpus_trigrams_subset %>% 
  count(ngrams) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop)) %>% 
  mutate(id = row_number()) %>%
  filter(ngrams != "NA")

# Subsetting to the top ten trigrams for the subset
top_ten_trigrams_subset <- corpus_trigrams_subset %>% 
  filter(id %in% c(1:11))

# Bar plot depicting the top-ten trigrams for the subset
b3_subset <- ggplot(top_ten_trigrams_subset, aes(x = reorder(ngrams, -n), y = n)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(name = 'Trigrams') + 
  scale_y_continuous(name = "Count of Trigrams") +
  ggtitle("Top 10 Trigrams Identified in Recommendations (Contributing Factor Level: Facility)") +
  ggeasy::easy_center_title()

# Repeat above for pentagrams for the subset
corpus_pentagrams_subset <- corpus_subset_df %>% 
  unnest_tokens(ngrams, text, token = "ngrams", n = 5)

# Generates a data frame with pentagrams by frequency for the subset
corpus_pentagrams_subset <- corpus_pentagrams_subset %>% 
  count(ngrams) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop)) %>% 
  mutate(id = row_number()) %>%
  filter(ngrams != "NA")

# Subsetting to the top ten pentagrams for the subset
top_ten_pentagrams_subset <- corpus_pentagrams_subset %>% 
  filter(id %in% c(1:11))

# Bar plot depicting the top-ten pentagrams for the subset
b5_subset <- ggplot(top_ten_pentagrams_subset, aes(x = reorder(ngrams, -n), y = n)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(name = 'pentagrams') + 
  scale_y_continuous(name = "Count of pentagrams") +
  ggtitle("Top 10 pentagrams Identified in Recommendations (Subset: Mental Health Conditions)") +
  ggeasy::easy_center_title()

# Word Clouds for the subset
doc_term_matrix_subset <- DocumentTermMatrix(corpus_subset)
wc1_subset <- wordcloud(corpus_subset, min.freq = 30, max.words = 100, random.order = FALSE, colors = "black", vfont = c("sans serif", "plain"))



### TF-IDF ###

# Assuming subset_data is your subset
recs <- df$crcfw_c_recom

# Set recs as the relevant text field
recs_corpus <- Corpus(VectorSource(recs))

# Preprocess the corpus
recs_corpus <- tm_map(recs_corpus, content_transformer(tolower))
recs_corpus <- tm_map(recs_corpus, removePunctuation)
custom_stopwords <- c("care", "disorder", "disorders", "ensure", "health", "include", 
                      "including", "medical", "need", "patient", "patients", "people", 
                      "postpartum", "pregnant", "pregnancy", "provide", "provider", 
                      "providers", "recommend", "service", "services", "substance", 
                      "substance use disorder", "sud", "treatment", "use", "woman", "women")      # Define custom stopwords if needed
recs_corpus <- tm_map(recs_corpus, removeWords, c(stopwords("english"), custom_stopwords)) #remove stopwords
recs_corpus <- tm_map(recs_corpus, stemDocument)

# Create a document-term matrix
recs_dtm <- DocumentTermMatrix(recs_corpus)

# Calculate TF-IDF
recs_tfidf <- weightTfIdf(recs_dtm)

# Convert TF-IDF matrix to a data frame
recs_tfidf_df <- as.data.frame(as.matrix(recs_tfidf))

# View the TF-IDF data frame
print(recs_tfidf_df)

# Calculate the sum of TF-IDF scores for each document
doc_tfidf_sum <- rowSums(as.matrix(recs_tfidf))

# Calculate the average TF-IDF score for each document
doc_tfidf_avg <- rowMeans(as.matrix(recs_tfidf))

# Identify top terms with a threshold (e.g., sum of TF-IDF > 1)
top_terms_sum <- colnames(as.matrix(recs_tfidf))[apply(as.matrix(recs_tfidf), 2, function(x) sum(x) > 1)]

# Get the sum of TF-IDF scores for each term
term_sum <- apply(as.matrix(recs_tfidf), 2, sum)

# Create a data frame with terms and their sums
term_sum_df <- data.frame(Term = colnames(as.matrix(recs_tfidf)), Sum_TFIDF = term_sum)

# Order the data frame by sum of TF-IDF in descending order
term_sum_df <- term_sum_df[order(-term_sum_df$Sum_TFIDF), ]

# Select the top 20 terms
top_terms_sum <- head(term_sum_df$Term, 20)

# Create a bar plot for the top 20 terms based on sum of TF-IDF
barplot(term_sum_df$Sum_TFIDF[1:20], names.arg = term_sum_df$Term[1:20], col = rainbow(20), main = "Top 10 Terms (Sum of TF-IDF)", las = 2)

# Identify top terms with a threshold (e.g., maximum TF-IDF > 0.2)
top_terms_max <- colnames(as.matrix(recs_tfidf))[apply(as.matrix(recs_tfidf), 2, function(x) max(x) > 0.2)]

# Get the maximum TF-IDF score for each term
term_max <- apply(as.matrix(recs_tfidf), 2, max)

# Create a data frame with terms and their maximum TF-IDF scores
term_max_df <- data.frame(Term = colnames(as.matrix(recs_tfidf)), Max_TFIDF = term_max)

# Order the data frame by maximum TF-IDF in descending order
term_max_df <- term_max_df[order(-term_max_df$Max_TFIDF), ]

# Select the top 20 terms
top_terms_max <- head(term_max_df$Term, 20)

# Create a bar plot for the top 20 terms based on maximum TF-IDF
barplot(term_max_df$Max_TFIDF[1:20], names.arg = term_max_df$Term[1:20], col = rainbow(20), main = "Top 20 Terms (Maximum TF-IDF)", las = 2)



## TF-IDF by subset: cause of death or contributing factor class

# options for subsetting

## replace what's in quotes with the underlying cause of death or contributing factor you're interested in
## the top causes of death were: Mental Health Conditions, Cardiomyopathy, Cardiovascular Conditions,
## Hemorrhage (Excludes Aneurysms or C, Hemorrhage (Excludes Aneurysms or CVA), Infection

# subset by COD = mental health conditions
subset_data <- df[df$Underlying.Cause.of.Death.Category..Reported. %in% c("Mental Health Conditions"), ]

# subset by COD = cardiomyopathy
## subset_data <- df[df$Underlying.Cause.of.Death.Category..Reported. %in% c("Cardiomyopathy"), ]

# subset by COD = cardiovascular conditions
## subset_data <- df[df$Underlying.Cause.of.Death.Category..Reported. %in% c("Cardiovascular Conditions"), ]

# subset by COD = hemorrhage
## subset_data <- df[df$Underlying.Cause.of.Death.Category..Reported. %in% c("Hemorrhage (Excludes Aneurysms or C", "Hemorrhage (Excludes Aneurysms or CVA)"), ]

# subset by COD = infection
## subset_data <- df[df$Underlying.Cause.of.Death.Category..Reported. %in% c("Infection"), ]

## the top contributing factors were: Substance Use Disorder- Alcohol, Illicit/Prescription Drugs,
## Continuity of Care/Care Coordination, Clinical Skill/Quality of Care, Mental Health Conditions, 
## Access/Financial

# subset by contributing factor = Substance Use Disorder- Alcohol, Illicit/Prescription Drugs
## subset_data <- df[df$Contributing.Factor.Class %in% c("Substance Use Disorder- Alcohol, Illicit/Prescription Drugs"), ]

# subset by contributing factor = Continuity of Care/Care Coordination
## subset_data <- df[df$Contributing.Factor.Class %in% c("Continuity of Care/Care Coordination"), ]

# subset by contributing factor = Clinical Skill/Quality of Care
## subset_data <- df[df$Contributing.Factor.Class %in% c("Clinical Skill/Quality of Care"), ]

# subset by contributing factor = Mental Health Conditions
## subset_data <- df[df$Contributing.Factor.Class %in% c("Mental Health Conditions"), ]

# subset by contributing factor = Access/Financial
## subset_data <- df[df$Contributing.Factor.Class %in% c("Access/Financial"), ]

# subset by contributing factor = Discrimination
## subset_data <- df[df$Contributing.Factor.Class %in% c("Discrimination"), ]

# subset by contributing factor = Social Support/Isolation
## subset_data <- df[df$Contributing.Factor.Class %in% c("Social Support/Isolation"), ]

# check dimensions--these should match the frequency counts from above
dim(subset_data)




### TOPIC MODELING

# Pre-Process Relevant Text Column(s) -----------------------------------------------------------------------

recs <- df$crcfw_c_recom #subset relevant text fields
corpus <- VCorpus(VectorSource(recs)) #transform to relevant data structure
corpus <- tm_map(corpus, content_transformer(tolower)) #lowercase
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
custom_stopwords <- c("care", "disorder", "disorders", "ensure", "health", "include", 
                      "including", "medical", "need", "patient", "patients", "people", 
                      "postpartum", "pregnant", "pregnancy", "provide", "provider", 
                      "providers", "recommend", "service", "services", "substance", 
                      "sud", "treatment", "use", "woman", "women")      # Define custom stopwords if needed
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), custom_stopwords)) #remove stopwords
corpus <- tm_map(corpus, stemDocument) #stem words

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus, control = list(minWordLength = 3, minDocFreq = 2))

# Identify rows with all zero entries
all_zero_rows <- which(rowSums(as.matrix(dtm)) == 0)

# Print the indices of problematic rows
print(all_zero_rows)

# Filter out documents with all-zero rows
non_zero_rows <- which(rowSums(as.matrix(dtm)) != 0)
dtm_filtered <- dtm[non_zero_rows, ]

# Check the dimensions of the filtered DTM
dim(dtm_filtered)

# Fit the LDA model
lda_model <- LDA(dtm_filtered, k = 7)

# Print the model summary or inspect the topics
lda_model

# Print the top terms for each topic
terms(lda_model, 20) # Adjust 'k' to the desired number of terms per topic

# Extract the topics for each document
topics <- as.data.frame(topics(lda_model))

# Print the topics assigned to each document
print(topics)



### LDA topic models on bigrams

# Custom bigram tokenizer function
bigram_tokenizer <- function(x) {
  ngrams <- function(words, n) {
    c(
      lapply(seq_along(words), function(i) {
        phrase <- words[i:min(i + n - 1, length(words))]
        if (length(unique(phrase)) == length(phrase)) {
          paste(phrase, collapse = " ")
        } else {
          NULL
        }
      }),
      sapply(seq_along(words), function(i) {
        phrase <- rev(words[max(i - n + 1, 1):i])
        if (length(unique(phrase)) == length(phrase)) {
          paste(phrase, collapse = " ")
        } else {
          NULL
        }
      })
    )
  }
  unlist(ngrams(words(x), 2))  # Adjust to 2 for bigrams
}

# Transform corpus with custom tokenization
corpus <- VCorpus(VectorSource(recs))
corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, removePunctuation) 
custom_stopwords <- c("care", "disorder", "disorders", "ensure", "health", "include", 
                      "including", "medical", "need", "patient", "patients", "people", 
                      "postpartum", "pregnant", "pregnancy", "provide", "provider", 
                      "providers", "recommend", "service", "services", "substance", 
                      "sud", "treatment", "use", "woman", "women")
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), custom_stopwords)) 
corpus <- tm_map(corpus, stemDocument) 
corpus <- tm_map(corpus, content_transformer(bigram_tokenizer))  # Apply custom bigram tokenizer

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus, control = list(tokenize = bigram_tokenizer, minWordLength = 3, minDocFreq = 2))

# Identify rows with all zero entries
all_zero_rows <- which(rowSums(as.matrix(dtm)) == 0)
print(all_zero_rows)  # Print the indices of problematic rows

# Filter out documents with all-zero rows
non_zero_rows <- which(rowSums(as.matrix(dtm)) != 0)
dtm_filtered <- dtm[non_zero_rows, ]

# Fit the LDA model
lda_model <- LDA(dtm_filtered, k = 7)

# Print the model summary or inspect the topics
print(lda_model)

# Print the top terms for each topic
terms(lda_model, 20) # Adjust 'k' to the desired number of terms per topic




###


### LDA topic models on trigrams

# Custom trigram tokenizer function
trigram_tokenizer <- function(x) {
  ngrams <- function(words, n) {
    c(
      lapply(seq_along(words), function(i) {
        phrase <- words[i:min(i + n - 1, length(words))]
        if (length(unique(phrase)) == length(phrase)) {
          paste(phrase, collapse = " ")
        } else {
          NULL
        }
      }),
      sapply(seq_along(words), function(i) {
        phrase <- rev(words[max(i - n + 1, 1):i])
        if (length(unique(phrase)) == length(phrase)) {
          paste(phrase, collapse = " ")
        } else {
          NULL
        }
      })
    )
  }
  unlist(ngrams(words(x), 3))
}

# Transform corpus with custom tokenization
corpus <- VCorpus(VectorSource(recs))
corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, removePunctuation) 
custom_stopwords <- c("care", "disorder", "disorders", "ensure", "health", "include", 
                      "including", "medical", "need", "patient", "patients", "people", 
                      "postpartum", "pregnant", "pregnancy", "provide", "provider", 
                      "providers", "recommend", "service", "services", "substance", 
                      "sud", "treatment", "use", "woman", "women")
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), custom_stopwords)) 
corpus <- tm_map(corpus, stemDocument) 
corpus <- tm_map(corpus, content_transformer(trigram_tokenizer))  # Apply custom trigram tokenizer

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus, control = list(tokenize = trigram_tokenizer, minWordLength = 3, minDocFreq = 2))

# Identify rows with all zero entries
all_zero_rows <- which(rowSums(as.matrix(dtm)) == 0)
print(all_zero_rows)  # Print the indices of problematic rows

# Filter out documents with all-zero rows
non_zero_rows <- which(rowSums(as.matrix(dtm)) != 0)
dtm_filtered <- dtm[non_zero_rows, ]

# Fit the LDA model
lda_model <- LDA(dtm_filtered, k = 3)

# Print the model summary or inspect the topics
print(lda_model)

# Print the top terms for each topic
terms(lda_model, 20) # Adjust 'k' to the desired number of terms per topic



### LDA topic models on subsets

# Custom trigram tokenizer function
trigram_tokenizer <- function(x) {
  ngrams <- function(words, n) {
    c(
      lapply(seq_along(words), function(i) {
        phrase <- words[i:min(i + n - 1, length(words))]
        if (length(unique(phrase)) == length(phrase)) {
          paste(phrase, collapse = " ")
        } else {
          NULL
        }
      }),
      sapply(seq_along(words), function(i) {
        phrase <- rev(words[max(i - n + 1, 1):i])
        if (length(unique(phrase)) == length(phrase)) {
          paste(phrase, collapse = " ")
        } else {
          NULL
        }
      })
    )
  }
  unlist(ngrams(words(x), 3))
}


subset_df <- df[df$Underlying.Cause.of.Death.Category..Reported. == "Mental Health Conditions", ]

subset_recs <- subset_df$crcfw_c_recom
subset_corpus <- VCorpus(VectorSource(subset_recs))
subset_corpus <- tm_map(subset_corpus, content_transformer(tolower))
subset_corpus <- tm_map(subset_corpus, removePunctuation)
subset_corpus <- tm_map(subset_corpus, removeWords, c(stopwords("english"), custom_stopwords))
subset_corpus <- tm_map(subset_corpus, stemDocument)
subset_corpus <- tm_map(subset_corpus, content_transformer(trigram_tokenizer))  # Apply custom trigram tokenizer

# Create a document-term matrix
dtm <- DocumentTermMatrix(subset_corpus, control = list(tokenize = trigram_tokenizer, minWordLength = 3, minDocFreq = 2))

# Identify rows with all zero entries
all_zero_rows <- which(rowSums(as.matrix(dtm)) == 0)
print(all_zero_rows)  # Print the indices of problematic rows

# Filter out documents with all-zero rows
non_zero_rows <- which(rowSums(as.matrix(dtm)) != 0)
dtm_filtered <- dtm[non_zero_rows, ]

# Fit the LDA model
lda_model <- LDA(dtm_filtered, k = 5) # Adjust 'k' to the desired number of terms per topic

# Print the model summary or inspect the topics
print(lda_model)

# Print the top terms for each topic
terms(lda_model, 20) 






########


## contributing factor levels: Patient/Family, Facility, Provider, System, Community

## TF-IDF ------------------------------------------------------------------------

subset_df <- df[df$Contributing.Factor.Level == "Community", ]

subset_recs <- subset_df$crcfw_c_recom
subset_corpus <- VCorpus(VectorSource(subset_recs))
subset_corpus <- tm_map(subset_corpus, content_transformer(tolower))
subset_corpus <- tm_map(subset_corpus, removePunctuation)
subset_corpus <- tm_map(subset_corpus, removeWords, c(stopwords("english"), custom_stopwords))
subset_corpus <- tm_map(subset_corpus, stemDocument)

subset_corpus_df <- data.frame(text = sapply(subset_corpus, as.character), stringsAsFactors = FALSE)

# Calculate term frequency (tf) among this subset
subset_trigrams <- subset_corpus_df %>%
  unnest_tokens(ngrams, text, token = "ngrams", n = 3) %>%
  count(ngrams, sort = TRUE)

# Filter the data for everything except designated subset
non_subset_df <- df[!df$Contributing.Factor.Level == "Community", ]

non_subset_recs <- df[df$Contributing.Factor.Level != "Community", ]$crcfw_c_recom
non_subset_corpus <- VCorpus(VectorSource(non_subset_recs))
non_subset_corpus <- tm_map(non_subset_corpus, content_transformer(tolower))
non_subset_corpus <- tm_map(non_subset_corpus, removePunctuation)
non_subset_corpus <- tm_map(non_subset_corpus, removeWords, c(stopwords("english"), custom_stopwords))
non_subset_corpus <- tm_map(non_subset_corpus, stemDocument)

non_subset_corpus_df <- data.frame(text = sapply(non_subset_corpus, as.character), stringsAsFactors = FALSE)
  
# Calculate document frequency (df) for each term
df_terms <- non_subset_corpus_df %>%
  unnest_tokens(ngrams, text, token = "ngrams", n = 3) %>%
  count(ngrams, sort=TRUE)

# Merge tf and idf data frames to calculate tfidf
merged_data <- merge(subset_trigrams, df_terms, by = "ngrams", all.x = TRUE)

# Rename columns to 'tf' and 'idf'
colnames(merged_data)[colnames(merged_data) == "n.x"] <- "tf"
colnames(merged_data)[colnames(merged_data) == "n.y"] <- "df"

# Replace NA values with 0 (to handle terms that are not in the subset but are in the idf calculation)
merged_data[is.na(merged_data)] <- 0

# Add 1 to every value in the "df" column so that idf (1/df) is a real value
merged_data <- merged_data %>%
  mutate(df = df + 1)

# Calculate idf for each term
merged_data <- merged_data %>%
  mutate(idf = log(nrow(df_terms) / df))

# Calculate tfidf by multiplying tf and idf
merged_data$tfidf <- merged_data$tf * merged_data$idf

# Sort the data frame by tfidf in descending order
sorted_merged_data <- merged_data %>%
  arrange(desc(tfidf))

# Remove the row with "0" in the ngrams column
sorted_merged_data <- sorted_merged_data[sorted_merged_data$ngrams != "0", ]

# Select the top 20 trigrams
top_trigrams <- head(sorted_merged_data, 20)

# Create a ggplot object
ggplot(top_trigrams, aes(x = reorder(ngrams, desc(tfidf)), y = tfidf, fill = ngrams)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 TF-IDF Trigrams (Community)", x = "Trigrams", y = "TF-IDF") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust x-axis text angle
        axis.text = element_text(size = 8),  # Adjust axis text size
        axis.title = element_text(size = 10),  # Adjust axis title size
        plot.title = element_text(size = 12)) +  # Adjust plot title size
  guides(fill = FALSE)  # Remove legend

