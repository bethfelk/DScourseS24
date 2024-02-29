# Task 1: Scraping from a website with no API 

library(rvest)
library(tidyverse)

url <- "https://www.baseball-reference.com/awards/hof_2023.shtml"

webpage <- read_html(url)

table <- webpage %>%
  html_nodes(css = "#hof_BBWAA") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
table <- table[[1]]

# Due to how the table displays on the website (with two stacked levels of 
# headers), it put the useful variable # names as row one, so I will convert 
# them to column headers witht the below code

# Assign the first row as column names
colnames(table) <- table[1, ]

# And then remove the first row since it is just the names
table <- table[-1, ]

# Now I make sure that the table is a dataframe like I want
class(table)

# Success!

# Task 2: Scraping from a website with an API
library(jsonlite)
endpoint = "https://statsapi.mlb.com/api/v1/people/134181?hydrate=currentTeam,team,stats(type=[yearByYear,yearByYearAdvanced,careerRegularSeason,careerAdvanced,availableStats](team(league)),leagueListId=mlb_hist)&site=en"
beltre = fromJSON(endpoint)
str(beltre)

# Now I have a very long list with data about Beltre
# I want to extract a table of his career stats
df <- beltre$people$stats[[1]] %>% as_tibble() 
View(df)
# This is a not a helpful table, but I can see that one of the subtables it holds
# (under YearbyYear, splites) does appear to be useful

# Viewing that subtable confirms it is what I want
View(df[[4]][[1]])

# so I will save that subtable as its own dataframe
beltre_yearly_stats <- beltre$people$stats[[1]][[4]][[1]] %>% as_tibble() 
View(beltre_yearly_stats)