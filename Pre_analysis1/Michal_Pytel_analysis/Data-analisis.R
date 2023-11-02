library("dplyr")
# install.packages("ggridges")
library("ggridges")
# install.packages('reticulate')
library("reticulate")
library(ggplot2)
library(stringr)
library(tidyr)
setwd("C:\\Users\\admin\\Documents\\TWD_projekt")

source_python("read_pickle.py")
pickle_data <- read_pickle_file("Data\\ingr_map.pkl")
Interactions_Raw <- read.csv('Data\\Raw_interactions.csv')
Recipes_Raw <- read.csv("Data\\RAW_recipes.csv")
Recipes_PP <- read.csv("Data\\PP_recipes.csv")
Users_PP <- read.csv("Data\\PP_users.csv")
glimpse(Recipes_Raw)





recipes <- Recipes_Raw %>%
  mutate(
    nutrition = gsub("\\[|\\]", "", nutrition),  # Remove brackets
    nutrition = gsub(" ", "", nutrition),  # Remove spaces
    nutrients = strsplit(nutrition, ","),  # Split into individual nutrient values
    calories = as.numeric(sapply(nutrients, function(x) as.numeric(x[1]))),
    total_fat = as.numeric(sapply(nutrients, function(x) as.numeric(x[2]))),
    sugar = as.numeric(sapply(nutrients, function(x) as.numeric(x[3]))),
    sodium = as.numeric(sapply(nutrients, function(x) as.numeric(x[4]))),
    protein = as.numeric(sapply(nutrients, function(x) as.numeric(x[5]))),
    saturated_fat = as.numeric(sapply(nutrients, function(x) as.numeric(x[6])))
  )




analiza1 <- Recipes_Raw %>% 
  mutate(
    tags = gsub("\\[|\\]", "", tags)
    ) %>% 
  separate_longer_delim(tags, delim = ", ") %>%
  group_by(tags) %>% 
  summarize(n = n()) %>% 
  arrange(-n) %>% 
  head(20)

ggplot(analiza1, aes(x = tags, y = n))+
  geom_bar(stat = "identity", fill = "blue")
recipes %>% 
  filter(calories < 1500) %>% 
  ggplot(aes(x = calories, y = protein)) +
  geom_point()

recipes %>% 
  filter(calories < 1500) %>%
  filter(minutes < 120) %>% 
  ggplot(aes(x = minutes, y = protein)) +
  geom_point()

recipes %>% 
  filter(calories < 2000) %>%
  filter(minutes < 120) %>% 
  ggplot(aes(x = minutes)) +
  geom_density()

recipes %>% 
  filter(calories < 2000) %>%
  filter(minutes < 120) %>% 
  ggplot(aes(x = calories)) +
  geom_density()


recipes_seperated_tags <- recipes %>% 
  mutate(
    tags = gsub("\\[|\\]", "", tags)
  ) %>% 
  separate_longer_delim(tags, delim = ", ")

recipes_time <- recipes_seperated_tags %>% 
  filter(tags %in% c("'15-minutes-or-less'", "'30-minutes-or-less'",
                     "'60-minutes-or-less'","'4-hours-or-less'"))
recipes_time %>% 
  filter(calories < 1500) %>% 
  ggplot(aes(x = calories, y = tags))+
  geom_density_ridges()+
  labs(title = "Distribution of recepies with given calories",
       x = "Calories",
       y = "Time Tag") +
  theme_minimal()

recipes_time %>% 
  filter(calories < 1500) %>% 
  ggplot(aes(x = calories, y = tags))+
  geom_boxplot()+
  labs(title = "Distribution of recepies with given calories",
       x = "Calories",
       y = "Time Tag") +
  theme_minimal()

recipes_time %>% 
  filter(protein < 150) %>% 
  ggplot(aes(x = protein, y = tags))+
  geom_boxplot()+
  labs(title = "Distribution of recepies with given protein",
       x = "Protein",
       y = "Time Tag") +
  theme_minimal()

recipes_time %>% 
  filter(protein < 150) %>% 
  ggplot(aes(x = protein, y = tags))+
  geom_density_ridges2()+
  labs(title = "Distribution of recepies with given protein",
       x = "Protein",
       y = "Time Tag") +
  theme_minimal()
