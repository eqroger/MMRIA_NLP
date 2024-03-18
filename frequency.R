#Analyzing N-Grams -----------------------------------------------------------------------------------------
corpus_df <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)

# n = # of n-grams (for trigrams n = 3)
corpus_ngrams <- corpus_df %>% 
  unnest_tokens(ngrams, text, token="ngrams", n=3) 

# generates a data frame with ngrams by frequency
corpus_ngrams <- corpus_ngrams %>% 
  count(ngrams) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop)) %>% 
  mutate(id = row_number()) %>%
  filter(ngrams != "NA")

#subsetting to the top ten n-grams
top_ten <- corpus_ngrams %>% 
  filter(id %in% c(1:10))

#bar plot depicting the top-ten n-grams
b <- ggplot(top_ten, aes(x=reorder(ngrams, -n), y = n)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_x_discrete(name = 'Trigrams') + 
  scale_y_continuous(name = "Count of Trigrams") +
  ggtitle("Top 10 Trigrams Identified in Recommendations") +
  ggeasy::easy_center_title()

# Word Clouds-----------------------------------------------------------------------------------------
doc_term_matrix <- DocumentTermMatrix(corpus)
wc1 <- wordcloud(corpus, min.freq = 30, max.words = 100, random.order=FALSE, colors="black", vfont=c("sans serif", "plain"))


# Frequencies by subsets—-----------------------------------------------------------------------

# Explore frequencies by subsets
# Subset by underlying cause of death; contributing factor class

## check frequencies of underlying cause of death and contributing factor class
frequency_table_cod <- table(df$Underlying.Cause.of.Death.Category..Reported)
frequency_table_contrfactor <- table(df$Contributing.Factor.Class)

# Sort the tables in descending order
sorted_frequency_table_cod <- sort(frequency_table_cod, decreasing = TRUE)
sorted_frequency_table_contrfactor <- sort(frequency_table_contrfactor, decreasing = TRUE)

# Print the sorted tables
print(sorted_frequency_table_cod)
print(sorted_frequency_table_contrfactor)

# Create a subset based on the specified condition

## replace what's in quotes with the underlying cause of death or contributing factor you're interested in (commented out options)

## the top underlying causes of death were: Mental Health Conditions, Cardiomyopathy, Cardiovascular Conditions, Hemorrhage (Excludes Aneurysms or C, Infection, Hemorrhage (Excludes Aneurysms or CVA)

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

## the top contributing factors were: Substance Use Disorder- Alcohol, Illicit/Prescription Drugs, Continuity of Care/Care Coordination, Clinical Skill/Quality of Care, Mental Health Conditions, Access/Financial
## other common factors: Assessment, Knowledge, Discrimination, Social Support/Isolation, Referral, Trauma

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

# check dimensions--these should match the frequency counts from above
dim(subset_data)

# Preprocess the text column in the subset
recs_subset <- subset_data$crcfw_c_recom
corpus_subset <- VCorpus(VectorSource(recs_subset))
corpus_subset <- tm_map(corpus_subset, content_transformer(tolower))
corpus_subset <- tm_map(corpus_subset, removePunctuation)
corpus_subset <- tm_map(corpus_subset, removeWords, stopwords("english"))
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
  filter(id %in% c(1:10))

# Bar plot depicting the top-ten trigrams for the subset
b3_subset <- ggplot(top_ten_trigrams_subset, aes(x = reorder(ngrams, -n), y = n)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(name = 'Trigrams') + 
  scale_y_continuous(name = "Count of Trigrams") +
  ggtitle("Top 10 Trigrams Identified in Recommendations (Subset: Mental Health Conditions)") +
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
  filter(id %in% c(1:10))

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

# TF-IDF—--------------------------------------------------------------------------------------

# Term Frequency-Inverse Document Frequency (tf-idf)

# Set recs as the relevant text field
recs_corpus <- Corpus(VectorSource(recs))

# Preprocess the corpus
recs_corpus <- tm_map(recs_corpus, content_transformer(tolower))
recs_corpus <- tm_map(recs_corpus, removePunctuation)
recs_corpus <- tm_map(recs_corpus, removeWords, stopwords("english"))
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
top_terms_max <- head(term_max_df$Term, 10)

# Create a bar plot for the top 10 terms based on maximum TF-IDF
barplot(term_max_df$Max_TFIDF[1:10], names.arg = term_max_df$Term[1:10], col = rainbow(10), main = "Top 10 Terms (Maximum TF-IDF)", las = 2)

### TF-IDF ###

# Assuming subset_data is your subset (specified previously, using )
recs_subset <- subset_data$crcfw_c_recom

# Set recs_subset as the relevant text field
recs_corpus_subset <- Corpus(VectorSource(recs_subset))

# Preprocess the corpus
recs_corpus_subset <- tm_map(recs_corpus_subset, content_transformer(tolower))
recs_corpus_subset <- tm_map(recs_corpus_subset, removePunctuation)
recs_corpus_subset <- tm_map(recs_corpus_subset, removeWords, stopwords("english"))
recs_corpus_subset <- tm_map(recs_corpus_subset, stemDocument)

# Create a document-term matrix
recs_dtm_subset <- DocumentTermMatrix(recs_corpus_subset)

# Calculate TF-IDF
recs_tfidf_subset <- weightTfIdf(recs_dtm_subset)

# Convert TF-IDF matrix to a data frame
recs_tfidf_df_subset <- as.data.frame(as.matrix(recs_tfidf_subset))

# View the TF-IDF data frame
print(recs_tfidf_df_subset)

# Calculate the sum of TF-IDF scores for each document
doc_tfidf_sum_subset <- rowSums(as.matrix(recs_tfidf_subset))

# Calculate the average TF-IDF score for each document
doc_tfidf_avg_subset <- rowMeans(as.matrix(recs_tfidf_subset))

# Identify top terms with a threshold (e.g., sum of TF-IDF > 1)
top_terms_sum_subset <- colnames(as.matrix(recs_tfidf_subset))[apply(as.matrix(recs_tfidf_subset), 2, function(x) sum(x) > 1)]

# Get the sum of TF-IDF scores for each term
term_sum_subset <- apply(as.matrix(recs_tfidf_subset), 2, sum)

# Create a data frame with terms and their sums
term_sum_df_subset <- data.frame(Term = colnames(as.matrix(recs_tfidf_subset)), Sum_TFIDF = term_sum_subset)

# Order the data frame by sum of TF-IDF in descending order
term_sum_df_subset <- term_sum_df_subset[order(-term_sum_df_subset$Sum_TFIDF), ]

# Select the top 10 terms
top_terms_sum_subset <- head(term_sum_df_subset$Term, 10)

# Create a bar plot for the top 10 terms based on sum of TF-IDF
barplot(term_sum_df_subset$Sum_TFIDF[1:10], names.arg = term_sum_df_subset$Term[1:10], col = rainbow(10), main = "Top 10 Terms (Sum of TF-IDF)", las = 2)

# Identify top terms with a threshold (e.g., maximum TF-IDF > 0.2)
top_terms_max_subset <- colnames(as.matrix(recs_tfidf_subset))[apply(as.matrix(recs_tfidf_subset), 2, function(x) max(x) > 0.2)]

# Get the maximum TF-IDF score for each term
term_max_subset <- apply(as.matrix(recs_tfidf_subset), 2, max)

# Create a data frame with terms and their maximum TF-IDF scores
term_max_df_subset <- data.frame(Term = colnames(as.matrix(recs_tfidf_subset)), Max_TFIDF = term_max_subset)

# Order the data frame by maximum TF-IDF in descending order
term_max_df_subset <- term_max_df_subset[order(-term_max_df_subset$Max_TFIDF), ]

# Select the top 10 terms
top_terms_max_subset <- head(term_max_df_subset$Term, 10)

# Create a bar plot for the top 10 terms based on maximum TF-IDF
barplot(term_max_df_subset$Max_TFIDF[1:10], names.arg = term_max_df_subset$Term[1:10], col = rainbow(10), main = "Top 10 Terms (Maximum TF-IDF)", las = 2)

## Repeat the above TF-IDF on subsets of your choice, e.g., underlying cause of death or contributing factor class (subset the same way we did for n-grams by subset previously)

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

# Assuming the relevant text field is Underlying.Cause.of.Death
recs_subset <- subset_data$crcfw_c_recom

# Set recs_subset as the relevant text field
recs_corpus_subset <- Corpus(VectorSource(recs_subset))

# Preprocess the corpus
recs_corpus_subset <- tm_map(recs_corpus_subset, content_transformer(tolower))
recs_corpus_subset <- tm_map(recs_corpus_subset, removePunctuation)
recs_corpus_subset <- tm_map(recs_corpus_subset, removeWords, stopwords("english"))
recs_corpus_subset <- tm_map(recs_corpus_subset, stemDocument)

# Create a document-term matrix
recs_dtm_subset <- DocumentTermMatrix(recs_corpus_subset)

# Calculate TF-IDF
recs_tfidf_subset <- weightTfIdf(recs_dtm_subset)

# Convert TF-IDF matrix to a data frame
recs_tfidf_df_subset <- as.data.frame(as.matrix(recs_tfidf_subset))

# View the TF-IDF data frame
print(recs_tfidf_df_subset)

# Calculate the sum of TF-IDF scores for each document
doc_tfidf_sum_subset <- rowSums(as.matrix(recs_tfidf_subset))

# Calculate the average TF-IDF score for each document
doc_tfidf_avg_subset <- rowMeans(as.matrix(recs_tfidf_subset))

# Identify top terms with a threshold (e.g., sum of TF-IDF > 1)
top_terms_sum_subset <- colnames(as.matrix(recs_tfidf_subset))[apply(as.matrix(recs_tfidf_subset), 2, function(x) sum(x) > 1)]

# Get the sum of TF-IDF scores for each term
term_sum_subset <- apply(as.matrix(recs_tfidf_subset), 2, sum)

# Create a data frame with terms and their sums
term_sum_df_subset <- data.frame(Term = colnames(as.matrix(recs_tfidf_subset)), Sum_TFIDF = term_sum_subset)

# Order the data frame by sum of TF-IDF in descending order
term_sum_df_subset <- term_sum_df_subset[order(-term_sum_df_subset$Sum_TFIDF), ]

# Select the top 10 terms
top_terms_sum_subset <- head(term_sum_df_subset$Term, 10)

# Create a bar plot for the top 10 terms based on sum of TF-IDF
barplot(term_sum_df_subset$Sum_TFIDF[1:10], names.arg = term_sum_df_subset$Term[1:10], col = rainbow(10), main = "Top 10 Terms (Sum of TF-IDF)", las = 2)

# Identify top terms with a threshold (e.g., maximum TF-IDF > 0.2)
top_terms_max_subset <- colnames(as.matrix(recs_tfidf_subset))[apply(as.matrix(recs_tfidf_subset), 2, function(x) max(x) > 0.2)]

# Get the maximum TF-IDF score for each term
term_max_subset <- apply(as.matrix(recs_tfidf_subset), 2, max)

# Create a data frame with terms and their maximum TF-IDF scores
term_max_df_subset <- data.frame(Term = colnames(as.matrix(recs_tfidf_subset)), Max_TFIDF = term_max_subset)

# Order the data frame by maximum TF-IDF in descending order
term_max_df_subset <- term_max_df_subset[order(-term_max_df_subset$Max_TFIDF), ]

# Select the top 10 terms
top_terms_max_subset <- head(term_max_df_subset$Term, 10)

# Create a bar plot for the top 10 terms based on maximum TF-IDF
barplot(term_max_df_subset$Max_TFIDF[1:10], names.arg = term_max_df_subset$Term[1:10], col = rainbow(10), main = "Top 10 Terms (Maximum TF-IDF)", las = 2)

## TOPIC MODELING

# Pre-Process Relevant Text Column(s) -----------------------------------------------------------------------
df <- df %>% distinct(X_id, 
                      Underlying.Cause.of.Death.Category..Reported., 
                      Contributing.Factor.Class, 
                      crcfw_c_recom)
dim(df)

recs <- df$crcfw_c_recom #subset relevant text fields
corpus <- VCorpus(VectorSource(recs)) #transform to relevant data structure
corpus <- tm_map(corpus, content_transformer(tolower)) #lowercase
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
custom_stopwords <- c("care", "disorder", "disorders", "ensure", "health", "include", 
                      "including", "medical", "need", "patient", "patients", "people", 
                      "postpartum", "pregnant", "pregnancy", "provide", "provider", 
                      "providers", "recommend", "service", "services", "substance", 
                      "sud", "treatment", "use", "woman", "women")  # Define custom stopwords corpus <- 
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
lda_model <- LDA(dtm_filtered, k = 5)

# Print the model summary or inspect the topics
lda_model

# Print the top terms for each topic
terms(lda_model, 5) # Adjust '5' to the desired number of terms per topic

# Extract the topics for each document
topics <- as.data.frame(topics(lda_model))

# Print the topics assigned to each document
print(topics)

