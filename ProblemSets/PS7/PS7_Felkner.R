# install the necessary packages
install.packages("mice")
install.packages("modelsummary")
no

# load the necessary packages
library(tidyverse)
library(modelsummary)
library(mice)
library(caret)
# set working directory
setwd("~/Desktop/DScourseS24/ProblemSets/PS7")

# load the file as a dataframe 
df <- read.csv("wages.csv")

# drop missing hgc observations
df <- df %>% 
  filter(!is.na(hgc))

# drop missing tenure observations
df <- df %>% 
  filter(!is.na(tenure))

# summary table
summary_table <- datasummary_skim(data = df, output = 'latex')
print(summary_table)

# Imputation methods
# complete cases model
df_complete_cases <- df %>% 
  filter(!is.na(logwage)) 

lm_complete_cases <- lm(logwage ~ hgc + college + tenure + age + married,
                        data = df_complete_cases)

# mean imputation model
df_mean_imp <- df %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

lm_mean_imp <- lm(logwage ~ hgc + college + tenure + age + married,
                  data = df_mean_imp)

# impute predicted values from complete cases model
predicted_values <- predict(lm_complete_cases, newdata = df)

df_complete_imp <- df %>%
  mutate(logwage = ifelse(is.na(logwage), predicted_values, logwage))

lm_complete_imp <- lm(logwage ~ hgc + college + tenure + age + married,
                  data = df_complete_imp)

# multiple imputation model using mice package
df_mice <- mice(df, m = 5, printFlag = FALSE)

lm_mice <- with(df_mice, lm(logwage ~ hgc + college + tenure + age + married))

# Modelsummary table with all 4 regression models
modelsummary(list("Complete Cases" = lm_complete_cases, 
                  "Mean Imp" = lm_mean_imp, 
                  "Complete Imp" = lm_complete_imp, 
                  "Mice" = lm_mice), stars = TRUE, output = 'latex')
