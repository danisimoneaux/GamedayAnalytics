require(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(treemap)
library(scales)
library(ggpubr)

#wordcloud
df <- read_csv("dfcleaned.csv")
df1 <- read_csv("top50_celeb.csv")


#Create word cloud in tableau - Data Cleaning Proccess
skew_words <- df[(df$keywords %in% c("auditions", "bait","betty","bigpapi","blizzard","bonus","bts","cheri","comeback","dontsneeze","dreamcometrue","emotional","gambling","gotme","gazette","howyoudoin","howyoudoing","indie","indy","jesse","lucas","realorfake","passion","stare","bonus","young","thankyou","vibes","tax","space","friendship","100","lvii","experts","lgbtq","award","website","tasty","chewy")), ]

df_counts <- skew_words %>%
  group_by(brand, keywords) %>%
  summarize(keyword_counts = n()) %>%
  ungroup()

# print result
print(df_counts[, c("brand", "keywords", "keyword_counts")])

write.csv(df_counts,"skew_keyword_count.csv", row.names = FALSE)

#Other Visuals:

#Plot line chart - Engagement ROI by Brand

#Define color palette
brand_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                           "#D55E00", "#CC79A7", "#A020F0", "#999999", "#32CD32")

ggplot(df1, aes(x = brand, y = engagement_ROI_3, group = 1, color = brand)) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "Engagement ROI by Brand", 
       x = "Brand", y = "Engagement ROI 3", color = "Brand") +
  scale_color_manual(values = brand_colors) +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white"),
  )

#Heatmap of Final score by Brand and Percent difference

ggplot(df1, aes(x = pd, y = brand, fill = final_score)) +
  geom_tile() +
  labs(title = "Heatmap of Final Score by Brand and % Difference",
       x = "Percent Differnce from First", y = "Brand", fill = "Final Score") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")


#Barplot - Engagement Score by Brand and Celeb Status

z <- .13

ggplot(df1, aes(x = brand, y = final_score, fill = celeb)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "#FF69B4", "FALSE" = "#CBC3E3"), 
                    name = "Celeb", 
                    labels = c("Yes", "No")) +
  labs(title = "Engagement Score by Brand and Celeb Status", 
       x = "Brand", y = "Final Score") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = 'white'),
    axis.text.y = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(color = "white"),
    legend.position = "right",
    legend.background = element_rect(fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    panel.grid.major.y = element_line(color = "white")
  ) +
  scale_y_continuous(labels = scales::comma) +
  annotate(geom = "rect", xmin = 6, xmax = 7, ymin = 30, ymax = 35, 
           fill = 'white', color = "white", size = 1) +
  annotate(geom = "text", x = 8, y = 100, 
           label = paste("R^2 =", z),
           color = "white", fontface = "bold", size = 6, hjust = 0.5, vjust = -5)

#Didn't use but keep for future reference

#Tree Map
df1 %>% 
  group_by(brand, celeb) %>%
  summarise(count = n()) %>%
  treemap(index = c("brand", "celeb"), vSize = "count", palette = "Set3", 
          title = "Tree Map of Brand and Celebrity Data")

#Violin Plot - Distribution of Engagement Scores by Celebrity Involvement
ggplot(df1, aes(x = celeb, y = final_score, fill = celeb)) +
  geom_violin() + 
  labs(title = "Distribution of Engagement Scores by Celebrity Involvement", 
       x = "Celebrity", y = "Engagement Score", fill = "Celebrity")

#Dot chart

df1 %>% 
  group_by(brand, celeb) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = brand, color = celeb)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("grey70", "red")) +
  labs(x = "Count", y = "Brand", color = "Celebrity") +
  ggtitle("Dot Plot of Brand and Celebrity Data")
