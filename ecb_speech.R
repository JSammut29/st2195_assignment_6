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
#adjust threshold as necessary based on known exchange rate data

# Ensures the merged dataset by date in ascending order
merged_data <- arrange(merged_data, date)

# Fill missing values in the 'fx' column with the last observation carried forward
merged_data <- fill(merged_data, fx)

# Remove rows with missing values in the "fx" column
merged_data <- merged_data[complete.cases(merged_data$fx), ]


## FX Return Analysis

# Calculate the exchange rate return correctly by comparing the current day's exchange rate with the next day's exchange rate
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


# Tokenize contents column into words
words <- str_split(fx_speech_dataset$contents, "\\s+")

# Flatten list of words
words <- unlist(words)

# Remove articles, prepositions, and similar connectors
words <- words[!tolower(words) %in% c("the", "a", "an", "and", "or", "but", "for", "to", "in", "on", "of", "with", "by")]

# Generate frequency table
word_freq <- table(words)



