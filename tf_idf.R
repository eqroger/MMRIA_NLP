library(pacman)
pacman::p_load("tm", "stringr", "SnowballC", "lsa", "dplyr", "wordcloud", "ngram", 
               "tidytext", "ggeasy", "ggplot2", "MASS", "topicmodels", "janeaustenr")

# Read in Data ---------------------------------------------------------------------------------------------
setwd("/Users/erinrogers/Library/CloudStorage/OneDrive-EmoryUniversity/Desktop/Research_Projects/TADA/")
df <- read.csv("./TADA_Dataset_Oct2023.csv")

df <- df %>% distinct(X_id, crcfw_c_recom,
                      .keep_all=TRUE)

## TF-IDF ------------------------------------------------------------------------

calculate_tfidf <- function(df, var_name, num_grams, contributing_factor_column) {
  custom_stopwords <- c("care", "disorder", "disorders", "ensure", "health", "include", 
                        "including", "medical", "need", "patient", "patients", "people", 
                        "postpartum", "pregnant", "pregnancy", "provide", "provider", 
                        "providers", "recommend", "service", "services", "substance", 
                        "sud", "treatment", "use", "woman", "women") 
  # Subset data for the specific contributing factor
  subset_df <- df[df[[var_name]] == contributing_factor_column, ]
  
  subset_recs <- subset_df$crcfw_c_recom
  subset_corpus <- VCorpus(VectorSource(subset_recs))
  subset_corpus <- tm_map(subset_corpus, content_transformer(tolower))
  subset_corpus <- tm_map(subset_corpus, removePunctuation)
  subset_corpus <- tm_map(subset_corpus, removeWords, c(stopwords("english"), custom_stopwords))
  subset_corpus <- tm_map(subset_corpus, stemDocument)
  
  subset_corpus_df <- data.frame(text = sapply(subset_corpus, as.character), stringsAsFactors = FALSE)
  
  # Calculate term frequency (tf) among this subset
  subset_trigrams <- subset_corpus_df %>%
    unnest_tokens(ngrams, text, token = "ngrams", n = num_grams) %>%
    count(ngrams, sort = TRUE)
  
  # Filter the data for everything except the specific contributing factor
  non_subset_df <- df[!df[[var_name]] == contributing_factor_column, ]
  
  non_subset_recs <- non_subset_df$crcfw_c_recom
  non_subset_corpus <- VCorpus(VectorSource(non_subset_recs))
  non_subset_corpus <- tm_map(non_subset_corpus, content_transformer(tolower))
  non_subset_corpus <- tm_map(non_subset_corpus, removePunctuation)
  non_subset_corpus <- tm_map(non_subset_corpus, removeWords, c(stopwords("english"), custom_stopwords))
  non_subset_corpus <- tm_map(non_subset_corpus, stemDocument)
  
  non_subset_corpus_df <- data.frame(text = sapply(non_subset_corpus, as.character), stringsAsFactors = FALSE)
  
  # Calculate document frequency (df) for each term
  df_terms <- non_subset_corpus_df %>%
    unnest_tokens(ngrams, text, token = "ngrams", n = num_grams) %>%
    count(ngrams, sort=TRUE)
  
  # Merge tf and idf data frames to calculate tfidf
  merged_data <- merge(subset_trigrams, df_terms, by = "ngrams", all.x = TRUE)
  
  # Rename columns to 'tf' and 'idf'
  colnames(merged_data)[colnames(merged_data) == "n.x"] <- "tf"
  colnames(merged_data)[colnames(merged_data) == "n.y"] <- "df"
  
  # Replace NA values with 0 (to handle terms that are not in the subset but are in the idf calculation)
  merged_data[is.na(merged_data)] <- 0
  
  # Add 1 to every value in the "n" column
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
  
  return(sorted_merged_data)
}

mh1 <- calculate_tfidf(df, var_name = "Underlying.Cause.of.Death.Category..Reported.", 1, "Mental Health Conditions")
mh2 <- calculate_tfidf(df, var_name = "Underlying.Cause.of.Death.Category..Reported.", 2, "Mental Health Conditions")
mh3 <- calculate_tfidf(df, var_name = "Underlying.Cause.of.Death.Category..Reported.", 3, "Mental Health Conditions")

cardio1 <- calculate_tfidf(df, var_name = "Underlying.Cause.of.Death.Category..Reported.", 1, "Cardiovascular Conditions")
cardio2 <- calculate_tfidf(df, var_name = "Underlying.Cause.of.Death.Category..Reported.", 2, "Cardiovascular Conditions")
cardio3 <- calculate_tfidf(df, var_name = "Underlying.Cause.of.Death.Category..Reported.", 3, "Cardiovascular Conditions")

# Recommendation Level
facility1 <- calculate_tfidf(df, var_name = "Recommendation.Level", 1, "Facility")
facility2 <- calculate_tfidf(df, var_name = "Recommendation.Level", 2, "Facility")
facility3 <- calculate_tfidf(df, var_name = "Recommendation.Level", 3, "Facility")

patfam1 <- calculate_tfidf(df, var_name = "Recommendation.Level", 1, "Patient/Family")
patfam2 <- calculate_tfidf(df, var_name = "Recommendation.Level", 2, "Patient/Family")
patfam3 <- calculate_tfidf(df, var_name = "Recommendation.Level", 3, "Patient/Family")

community1 <- calculate_tfidf(df, var_name = "Recommendation.Level", 1, "Community")
community2 <- calculate_tfidf(df, var_name = "Recommendation.Level", 2, "Community")
community3 <- calculate_tfidf(df, var_name = "Recommendation.Level", 3, "Community")

system1 <- calculate_tfidf(df, var_name = "Recommendation.Level", 1, "System")
system2 <- calculate_tfidf(df, var_name = "Recommendation.Level", 2, "System")
system3 <- calculate_tfidf(df, var_name = "Recommendation.Level", 3, "System")

provider1 <- calculate_tfidf(df, var_name = "Recommendation.Level", 1, "Provider")
provider2 <- calculate_tfidf(df, var_name = "Recommendation.Level", 2, "Provider")
provider3 <- calculate_tfidf(df, var_name = "Recommendation.Level", 3, "Provider")
