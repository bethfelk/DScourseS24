# I will first import my datasets from baseball reference
# I will be using the HOF data from 2015-2024
# After tuning my algorithm I will also apply it to the 2025 likely candidate
# data for fun, but I won't know how accurate my predictions are unti next year
# load the necessary packages

library(rvest)
library(tidyverse)

url_2015 <- "https://www.baseball-reference.com/awards/hof_2015.shtml"

webpage_2015 <- read_html(url_2015)

data_2015 <- webpage_2015 %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
data_2015 <- data_2015[[1]]

# Assign the first row as column names
colnames(data_2015) <- data_2015[1, ]

# And then remove the first row since it is just the names
data_2015 <- data_2015[-1, ]

# and repeat this process for the subsequent years

# 2016
url_2016 <- "https://www.baseball-reference.com/awards/hof_2016.shtml"

webpage_2016 <- read_html(url_2016)

data_2016 <- webpage_2016 %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
data_2016 <- data_2016[[1]]

# Assign the first row as column names
colnames(data_2016) <- data_2016[1, ]

# And then remove the first row since it is just the names
data_2016 <- data_2016[-1, ]

# 2017
url_2017 <- "https://www.baseball-reference.com/awards/hof_2017.shtml"

webpage_2017 <- read_html(url_2017)

data_2017 <- webpage_2017 %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
data_2017 <- data_2017[[1]]

# Assign the first row as column names
colnames(data_2017) <- data_2017[1, ]

# And then remove the first row since it is just the names
data_2017 <- data_2017[-1, ]

# 2018
url_2018 <- "https://www.baseball-reference.com/awards/hof_2018.shtml"

webpage_2018 <- read_html(url_2018)

data_2018 <- webpage_2018 %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
data_2018 <- data_2018[[1]]

# Assign the first row as column names
colnames(data_2018) <- data_2018[1, ]

# And then remove the first row since it is just the names
data_2018 <- data_2018[-1, ]

# 2019
url_2019 <- "https://www.baseball-reference.com/awards/hof_2019.shtml"

webpage_2019 <- read_html(url_2019)

data_2019 <- webpage_2019 %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
data_2019 <- data_2019[[1]]

# Assign the first row as column names
colnames(data_2019) <- data_2019[1, ]

# And then remove the first row since it is just the names
data_2019 <- data_2019[-1, ]

# 2020
url_2020 <- "https://www.baseball-reference.com/awards/hof_2020.shtml"

webpage_2020 <- read_html(url_2020)

data_2020 <- webpage_2020 %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
data_2020 <- data_2020[[1]]

# Assign the first row as column names
colnames(data_2020) <- data_2020[1, ]

# And then remove the first row since it is just the names
data_2020 <- data_2020[-1, ]

# 2021
url_2021 <- "https://www.baseball-reference.com/awards/hof_2021.shtml"

webpage_2021 <- read_html(url_2021)

data_2021 <- webpage_2021 %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
data_2021 <- data_2021[[1]]

# Assign the first row as column names
colnames(data_2021) <- data_2021[1, ]

# And then remove the first row since it is just the names
data_2021 <- data_2021[-1, ]

# 2022
url_2022 <- "https://www.baseball-reference.com/awards/hof_2022.shtml"

webpage_2022 <- read_html(url_2022)

data_2022 <- webpage_2022 %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
data_2022 <- data_2022[[1]]

# Assign the first row as column names
colnames(data_2022) <- data_2022[1, ]

# And then remove the first row since it is just the names
data_2022 <- data_2022[-1, ]

# 2023
url_2023 <- "https://www.baseball-reference.com/awards/hof_2023.shtml"

webpage_2023 <- read_html(url_2023)

data_2023 <- webpage_2023 %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
data_2023 <- data_2023[[1]]

# Assign the first row as column names
colnames(data_2023) <- data_2023[1, ]

# And then remove the first row since it is just the names
data_2023 <- data_2023[-1, ]

# 2024
url_2024 <- "https://www.baseball-reference.com/awards/hof_2024.shtml"

webpage_2024 <- read_html(url_2024)

data_2024 <- webpage_2024 %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
data_2024 <- data_2024[[1]]

# Assign the first row as column names
colnames(data_2024) <- data_2024[1, ]

# And then remove the first row since it is just the names
data_2024 <- data_2024[-1, ]

# Now I need to manually add a Hall of Fame dummy variable because on
# baseball reference it's indicated by being highlighted which doesn't
# translate through the web scraping
data_2015$HoF <- ifelse(row.names(data_2015) %in% c(1:4), "Yes", "No")
data_2016$HoF <- ifelse(row.names(data_2016) %in% c(1:2), "Yes", "No")
data_2017$HoF <- ifelse(row.names(data_2017) %in% c(1:3), "Yes", "No")
data_2018$HoF <- ifelse(row.names(data_2018) %in% c(1:4), "Yes", "No")
data_2019$HoF <- ifelse(row.names(data_2019) %in% c(1:4), "Yes", "No")
data_2020$HoF <- ifelse(row.names(data_2020) %in% c(1:2), "Yes", "No")
data_2021$HoF <- "No"
data_2022$HoF <- ifelse(row.names(data_2022) %in% c(1:1), "Yes", "No")
data_2023$HoF <- ifelse(row.names(data_2023) %in% c(1:1), "Yes", "No")
data_2024$HoF <- ifelse(row.names(data_2024) %in% c(1:3), "Yes", "No")


# now I will vertically bind each yearly dataset into one combined dataset
combined_data <- rbind(data_2015, data_2016, data_2017, data_2018, data_2019,
                       data_2020, data_2021, data_2022, data_2023, data_2024)

# now I need to rename some variables that currently have the same name for
# both hitter and pitcher home runs
names(combined_data)[names(combined_data) == "G"] <- c("Batter_G", "Pitcher_G")
names(combined_data)[names(combined_data) == "H"] <- c("Batter_H", "Pitcher_H")
names(combined_data)[names(combined_data) == "HR"] <- c("Batter_HR", "Pitcher_HR")
names(combined_data)[names(combined_data) == "BB"] <- c("Batter_BB", "Pitcher_BB")


# then I will remove the "X-" prefix and "HOF" suffix from player names
# X- means they are leaving the ballot that year
# and HOF means they made the Hall in a subsequent year
# but I need to remove them so that I can remove duplicate names
combined_data$Name <- gsub("HOF$", "", combined_data$Name)
combined_data$Name <- gsub("^X-", "", combined_data$Name)
 
# now I will remove the duplicates of the players who appear more than once
# note that my code keeps each recurring player only in their final year of
# appearance, which would either be the year they were inducted into the HoF
# or the year they ran out of eligibility (after 10 years)
combined_data <- combined_data %>%
  group_by(Name) %>%
  slice(n())

# Now I need to convert all my statistics variable from character to numeric
combined_data <- combined_data %>%
  mutate_at(vars(6:38), as.numeric)

# and %vote which is current expressed a percentage
names(combined_data)[names(combined_data) == "%vote"] <- "vote_percentage"
combined_data$vote_percentage <- as.numeric(gsub("%", "", combined_data$vote_percentage)) / 100

# Then I will exclude pitchers by excluding all observations with a nonzero
# number of wins (note that position players occasionally pitch in blowout
# games but they would not get a win for this)
combined_data <- combined_data %>%
  filter(is.na(W) | W == 0)

# Then I am going to remove all the pitching statistics from my dataset
# since I am only focused on hitting here
combined_data <- combined_data %>%
  select(-c(26:38))

# Finally I need to convert HoF to a factor variable since it's what I am 
# predicting in my classification model
combined_data$HoF <- factor(combined_data$HoF, levels = c("No", "Yes"))

# going to rename my hitters data to "hitters
hitters <- combined_data

# Ok now I have my completed dataset so time to run some trees

library(tidymodels)
library(rpart)

# First we need to set up the split
hitters_split <- initial_split(hitters, prop = 0.8)
hitters_train <- training(hitters_split)
hitters_test  <- testing(hitters_split)

# set up the task and the engine
tune_tree_spec <- decision_tree(
  min_n = tune(), # tuning parameter
  tree_depth = tune(), # tuning parameter
  cost_complexity = tune(), # tuning parameter
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (complexity, depth, etc.)
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.01))
tree_parm_df2 <- tibble(min_n = seq(10,60,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=1))
tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% 
  full_join(.,tree_parm_df3,by=character())

# 3-fold cross-validation
rec_folds <- vfold_cv(hitters_train, v = 3)

# Workflow
tree_wf <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(HoF ~ Yrs + WAR + WAR7 + JAWS + Jpos + Batter_G + AB + R +
                Batter_H + Batter_HR + RBI + SB + Batter_BB + BA + OBP +
                SLG + OPS)
# Tuning results
tree_res <- tree_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = tree_parm_df
  )

# what is the best value of lambda?
tree_top_acc  <- show_best(tree_res, metric = "accuracy")
tree_best_acc <- select_best(tree_res, metric = "accuracy")
final_tree <- finalize_workflow(tree_wf,
                                tree_best_acc
)
print('*********** TREE **************')
tree_test <- last_fit(final_tree,hitters_split) %>%
  collect_metrics()

tree_test %>% print(n = 1)
tree_top_acc %>% print(n = 1)

# combine results into a nice tibble (for later use)
tree_ans <- tree_top_acc %>% slice(1)







