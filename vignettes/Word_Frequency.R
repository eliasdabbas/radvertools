## ------------------------------------------------------------------------
df <- data.frame(text = c("first word", "second word", "third word"), metric = 1)
df

## ------------------------------------------------------------------------
library(tidyr)
df <- df %>% separate(col = text, into = as.character(1:2), sep = " ", remove = F)
df


## ------------------------------------------------------------------------
df <- df %>% gather(order, word, -c(text, metric))
df

## ----message=FALSE-------------------------------------------------------
library(dplyr)
df <- df %>% 
  group_by(word) %>% 
  summarise(metric = sum(metric, na.rm = T)) %>% 
  arrange(desc(metric))

df

## ------------------------------------------------------------------------
df2 <- data.frame(
  text = c("book name", "book title", "great title", "great name"), 
  metric = c(100, 120, 300, 150))
df2

## ------------------------------------------------------------------------
df2 <- df2 %>% separate(col = text, into = as.character(1:2), sep = " ", remove = F) %>% 
  gather(order, word, -c(text, metric)) %>% 
  group_by(word) %>% 
  summarise(metric = sum(metric, na.rm = T)) %>% 
  arrange(desc(metric))
df2

## ----message=FALSE, warning=FALSE----------------------------------------

movies <- data.frame(
  text = c("Star Wars The Force Awakens", "Avatar", "Titanic", "Jurassic World", "Marvels The Avengers", "The Dark Knight", "Star Wars Episode I - The Phantom Menace", "Star Wars", "Avengers Age of Ultron", "The Dark Knight Rises"),
  metric = c(935254389, 760507625, 658672302, 652270625, 623357910, 534858444, 474544677, 460998007, 459005868, 448139099)
)
movies
library(advertools)
word_frequency(movies)



