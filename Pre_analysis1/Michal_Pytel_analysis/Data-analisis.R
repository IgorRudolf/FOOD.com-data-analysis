library("dplyr")
# install.packages("ggridges")
library("ggridges")
# install.packages('reticulate')
library("reticulate")
library(ggplot2)
library(stringr)
library(tidyr)
library(ggstatsplot)
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
    saturated_fat = as.numeric(sapply(nutrients, function(x) as.numeric(x[6]))),
    carbohydrates = as.numeric(sapply(nutrients, function(x) as.numeric(x[7])))
  )




analiza1 <- Recipes_Raw %>% 
  mutate(
    tags = gsub("\\[|\\]", "", tags)
    ) %>% 
  separate_longer_delim(tags, delim = ", ") %>%
  group_by(tags) %>% 
  summarize(n = n()) %>% 
  arrange(-n) %>% 
  head(100)

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

recipes_kuchnia <- recipes_seperated_tags %>% 
  filter(tags %in% c("'mexican'", "'italian'",
                     "'greek'","'canadian'", "'indian'", "'french'"))

recipes_type <- recipes_seperated_tags %>% 
  filter(tags %in% c("'french'", "'italian'"))

library(fmsb)

data_radar <- recipes_type %>% 
  group_by(tags) %>% 
  mutate(calories = calories/20) %>% 
  summarise(mean_calorie = mean(calories), mean_protein = mean(protein), mean_sodium = mean(sodium), mean_total_fat = mean(total_fat),
            mean_saturated_fat = mean(saturated_fat), mean_sugar = mean(sugar), mean_carbohydrates = mean(carbohydrates)) %>% 
  select(tags, mean_calorie,mean_protein,mean_sodium,mean_total_fat,mean_saturated_fat,mean_sugar,mean_carbohydrates)
data_radar <- as.data.frame(data_radar)
rownames(data_radar) <- c("meat", "vegan")
data_radar <- rbind(rep(60,7) , rep(0,7) , data_radar)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

rect(-1, -1, 1, 1, col = "#fffad4", border = "#fffad4", density = -1, lty = 0)
radarchart(data_radar, axistype=1 , 
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
           #custom the grid
           cglcol="#4c410c", cglty=1, axislabcol="#4c410c", caxislabels=seq(0,60,15), cglwd=0.8,
           #custom labels
           vlcex=0.8)
legend(x=0.7, y=1, legend = rownames(data_radar[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

library(RColorBrewer)
library(showtext)
font_add(family="Caveat Brush", regular="C:\\users\\admin\\appdata\\local\\microsoft\\windows\\fonts\\caveatbrush-regular.ttf")
showtext_auto() 
recipes_kuchnia%>% 
  filter(minutes < 120) %>% 
  ggplot(aes(x = minutes, y = tags, fill = tags))+
  geom_density_ridges(alpha = .7)+
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none", text = element_text(family="Caveat Brush", color = "#4c410c", size=20))+
  labs(title = "Density of time to make a recipe by cuisine",
       x = "Minutes",
       y = "Cuisine") +
  
  theme(plot.background = element_rect(fill = "#fffad4"))+
  theme(
    panel.background = element_rect(fill = "#f0e6b4",
                                    colour = "#f0e6b4",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#fffad4"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "#fffad4"))+
  xlim(0, 120)

recipes_type %>% 
  filter(calories < 2000) %>% 
  ggplot(aes(x = calories, group = tags, fill = tags))+
  geom_density(adjust=1.5, alpha = .4)

recipes_type %>% 
  filter(protein < 100) %>% 
  ggplot(aes(x = protein, group = tags, fill = tags))+
  geom_density(adjust=1.5, alpha = .4)

recipes_type %>% 
  filter(sugar < 100, sugar > 0) %>% 
  ggplot(aes(x = sugar, group = tags, fill = tags))+
  geom_density(adjust=1.5, alpha = .4)

recipes_type %>% 
  filter(total_fat < 100) %>% 
  ggplot(aes(x = total_fat, group = tags, fill = tags))+
  geom_density(adjust=1.5, alpha = .4)

library(tidyverse)

# Filter and reshape the data
filtered_data <- recipes_type %>%
  filter(calories < 2000 | protein < 100 | sugar < 100 | total_fat < 100) %>%
  pivot_longer(
    cols = c(calories, protein, sugar, total_fat),
    names_to = "category"
  )

# Create the density plot with facet_wrap
filtered_data %>%
  ggplot(aes(x = value, group = tags, fill = tags)) +
  geom_density_ridges(adjust = 1.5, alpha = 0.4) +
  facet_wrap(~category, scales = "free_x") +
  labs(
    title = "Distribution of recipes with given nutrients",
    x = "Value",
    y = "Density"
  )

filtered_data <- recipes_type %>%
  filter(calories < 2000 | protein < 100 | sugar < 100 | total_fat < 100) %>%
  pivot_longer(
    cols = c(calories, protein, sugar, total_fat),
    names_to = "category"
  )

filtered_data %>%
  ggplot(aes(x = value, y = tags, fill = tags)) +
  geom_density_ridges(aes(y = tags), alpha = 0.4) +
  facet_wrap(~category, scales = "free_x") +
  labs(
    title = "Distribution of recipes with given nutrients",
    x = "Value",
    y = "Tags"
  )


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

filtered_data <- recipes_type %>%
  filter(calories < 2000 & protein < 100 & sugar < 100 & total_fat < 100) %>%
  pivot_longer(
    cols = c(calories, protein, sugar, total_fat),
    names_to = "category"
  )

# Create the density plot with facets
filtered_data %>%
  ggplot(aes(x = value, group = tags, fill = tags)) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  facet_wrap(~category, scales = "free_x") +
  labs(
    title = "Distribution of nutrients with given tags",
    x = "Value",
    y = "Density"
  )
  

data <- data.frame(
  Category = c("A", "B", "C", "D", "E"),
  Value = c(5, 4, 3, 2, 1)
)

spider_plot <- ggplot(data, aes(x = Category, y = Value)) +
  geom_polygon(fill = "lightblue", color = "blue", size = 1) +
  geom_path(color = "blue", size = 1) +  # Add lines connecting the points
  theme_minimal() +
  coord_polar(start = pi/8) +  # Adjust the starting angle as needed
  labs(title = "Spider Chart", x = NULL, y = NULL)

print(spider_plot)
