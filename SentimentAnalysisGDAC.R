# Load the libraries
library(tidytext)
library(dplyr)
library(tidyr)
library(syuzhet)
library(SnowballC)
library(magrittr)
library(ggplot2)
library(scales)

# Load the data
df <- read.csv("dfcleaned.csv")

#Top 12 Brands
df <- df[df$brand %in% c("Limit Break","Paramount Plus: Stallone faces off","MARVEL- Guardians Galaxy","Warner Bros. Trailer: The Flash","Disney 100 Special Look","DraftKings: Kevin Hart-Free bet","Disney Trailer: Indiana Jones and the Dial of Destiny","Turbo Tax live2023","Tubi","Pop Corners: Breaking Bad reunion","Amazon Studios Trailer: AIR: Courting A Legend","Transformers","Meta"), ]

#Remove all columns except for Brand and Tweet
df <- df %>%
  select(brand, tweet)

# Preprocess data
sentiment_data <- df %>%
  select(brand, tweet) %>%
  unnest_tokens(word, tweet) %>%
  anti_join(stop_words) %>%
  filter(!grepl("^http", word)) %>%
  mutate(word = wordStem(word))

# Perform sentiment analysis
sentiment_scores <- get_sentiment(sentiment_data$word, method = "bing")
sentiment_data <- cbind(sentiment_data, sentiment_scores)

# Analyze the sentiment scores
sentiment_summary <- sentiment_data %>%
  group_by(brand) %>%
  summarize(sentiment = mean(sentiment_scores))

# Plot the sentiment scores for each brand
  sentiment_data %>%
  group_by(brand) %>%
  summarize(sentiment = mean(sentiment_scores)) %>%
  ggplot(aes(x = brand, y = sentiment, fill = brand)) +
  geom_bar(stat = "identity", alpha = 1) +
  labs(title = "Sentiment Analysis",
        x = "Brand",
        y = "Sentiment Score",
        fill = "Brand") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white"),
    panel.grid.major.y = element_line(color = color_rgba("white", 0.2)),
    panel.grid.minor = element_blank()
   ) +
  ylim(-.1, .1)
  
  
  
  
  



