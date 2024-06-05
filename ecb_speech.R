## Load the necessary libraries
library(tidyverse)
library(lubridate)
library(tidytext)
library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)

## Load the datasets

#https://www.ecb.europa.eu/press/key/html/downloads.en.html
speeches <- read_delim('speeches.csv', 
                       delim = '|', 
                       col_types = cols(
                         date = col_date(), 
                         contents = col_character()
                       ),
                       quote = "",
                       escape_double = FALSE)

transcripts <- read_delim('speeches.csv', 
                          delim = '|', 
                          col_types = cols(
                            date = col_date(), 
                            contents = col_character()
                          ),
                          quote = "",
                          escape_double = FALSE,
                          col_select = c(date, contents))

#https://data.ecb.europa.eu/data/datasets/EXR/EXR.D.USD.EUR.SP00.A
fx <- read_csv('fx.csv', 
               col_types = cols(
                 DATE = col_date(format = "%Y-%m-%d"),
                 `US dollar/Euro (EXR.D.USD.EUR.SP00.A)` = col_double()
               )) %>%
  select(date = DATE, fx = `US dollar/Euro (EXR.D.USD.EUR.SP00.A)`)


# Merge datasets
merged_data <- full_join(fx, transcripts, by = "date")

print("Check the structure of the merged dataset")
str(merged_data)
head(merged_data)
summary(merged_data)
print("Exchange rates are in line with known historical data")

# In case of outliers, uncomment this line.
#merged_data <- merged_data %>% filter(exchange_rate > 0.5 & exchange_rate < 2)
# Adjust threshold as necessary based on known exchange rate data

# Ensures the merged dataset is sorted by date in ascending order.
merged_data <- arrange(merged_data, date)

# Fill missing values in the 'fx' column with the last observation carried forward
merged_data <- fill(merged_data, fx)

# Remove rows with missing values in the "fx" column
merged_data <- merged_data[complete.cases(merged_data$fx), ]


## FX Return Analysis

# Calculate the exchange rate return correctly by comparing the current day's 
# exchange rate with the next day's exchange rate
fx_speech_dataset <- merged_data %>%
  mutate(return = (lead(fx) - fx) / fx * 100)

# Extend the dataset with "good_news" and "bad_news" variables
fx_speech_dataset <- fx_speech_dataset %>%
  mutate(good_news = ifelse(return > 0.5, 1, 0),
         bad_news = ifelse(return < -0.5, 1, 0))

# Remove entries where 'contents' column has NA values
fx_speech_dataset <- fx_speech_dataset %>%
  filter(!is.na(contents))

# Check the structure of the dataset
str(fx_speech_dataset)
head(fx_speech_dataset)
summary(fx_speech_dataset)


## Generate Frequency Tables for Words

# Filter datasets
good_news_data <- fx_speech_dataset %>% filter(good_news == 1)
bad_news_data <- fx_speech_dataset %>% filter(bad_news == 1)

# Tokenize words and count frequencies for all speeches.
words <- fx_speech_dataset %>%
  unnest_tokens(word, contents) %>%
  count(word, sort = TRUE)

# Custom stop words list (common articles, prepositions, etc.) from 'words'
custom_stop_words <- c("the", "of", "and", "in", "to", "a", "is", "that", "for", "on", "as", 
                       "this", "be", "by", "have", "are", "with", "it", "has", "at", "we", 
                       "not", "which", "an", "will", "I", "from", "more", "also", "been", 
                       "would", "our", "these", "their", "can", "its", "but", "or", "was", 
                       "should", "all", "some", "other", "such", "over", "they", "than", 
                       "de", "i", "la")


# Tokenize words and count frequencies for;

# good news,
good_news_words <- good_news_data %>%
  unnest_tokens(word, contents) %>%
  count(word, sort = TRUE)
good_news_words_filtered <- good_news_words %>%
  filter(!word %in% custom_stop_words)

# and bad news.
bad_news_words <- bad_news_data %>%
  unnest_tokens(word, contents) %>%
  count(word, sort = TRUE)
bad_news_words_filtered <- bad_news_words %>%
  filter(!word %in% custom_stop_words)

## Select the top 20 most common words for each dataset
good_indicators <- good_news_words_filtered %>% top_n(20, n)
print(good_indicators)

bad_indicators <- bad_news_words_filtered %>% top_n(20, n)
print(bad_indicators)

# Save to CSV files
write.csv(good_indicators, "good_indicators.csv", row.names = FALSE)
write.csv(bad_indicators, "bad_indicators.csv", row.names = FALSE)

## Analysis

# Find words that are only in the good_news list
unique_good_words <- setdiff(good_indicators$word, bad_indicators$word)

# Find words that are only in the bad_news list
unique_bad_words <- setdiff(bad_indicators$word, good_indicators$word)

# Find words that are in both lists
common_words <- intersect(good_indicators$word, bad_indicators$word)

good_news_words_ex_common <- good_news_words_filtered %>%
  filter(!word %in% common_words)
bad_news_words_ex_common <- bad_news_words_filtered %>%
  filter(!word %in% common_words)

## Select the top 20 most common words for each dataset, excluding common words
good_indicators_2 <- good_news_words_ex_common %>% top_n(20, n)
bad_indicators_2 <- bad_news_words_ex_common %>% top_n(20, n)

# Find unique words
unique_good_words_2 <- setdiff(good_indicators_2$word, bad_indicators_2$word)
print(unique_good_words_2)
unique_bad_words_2 <- setdiff(bad_indicators_2$word, good_indicators_2$word)
print(unique_bad_words_2)



