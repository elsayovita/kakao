#Import Library and dataset
library(tidyverse)
library(readxl)
library(car)
library(RVAideMemoire)
setwd("~/Documents/Datasets/kakao")
data = read_excel("kakao_data.xlsx")

#Transform "genre" column to factor
data$genre = as.factor(data$genre)

#Compute the numerical summaries for each factor
tapply(data$likes, data$genre, summary)
tapply(data$likes, data$genre, sd)

#Create a box plot to assess visually
ggplot(data, aes(x = genre, y = likes)) +
  geom_boxplot() +
  theme_bw() + #bg color
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Genre",
       y = "Likes",
       title = "Number of Likes by Genre")

#Levene Test of variance homogeneity
leveneTest(data$likes ~ data$genre)
#Non homogenous variance

#Shapiro Wilk test of normalty
byf.shapiro(data$likes ~ data$genre)
#No normal distribution

#Anova assumption does not hold - to use non-parametric test

#Kruskal-Wallis Test
kruskal.test(data$likes ~ data$genre)
#As p-value <0.05, there is at least a group with significant difference 

#Pairwise Wilcoxon Test
pairwise.wilcox.test(data$likes, data$genre,
                     p.adjust.method = "BH")