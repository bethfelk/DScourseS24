# I will be using a dataset about career stats by year for Adrian Beltre, an
# MLB third baseman who was recently inducted into the Hall of Fame 2024 class
# with 366 votes (out of 385 voters, note that this is much closer to 
# unanimous agreement than the highest vote getter in most years)

# I am going to use the same Beltre stats that I pulled through the MLB API in
# PS5, but I will be pulling it from baseball-reference instead bc the MLB API
# produced a multi-layered mess that R will not let me perform the cleaning
# I want on due to how it is nested

library(rvest)
library(tidyverse)

url <- "https://www.baseball-reference.com/players/b/beltrad01.shtml"

webpage <- read_html(url)

beltre <- webpage %>%
  html_nodes(css = "#batting_standard") %>%
  html_table(fill = TRUE)

# This produces a list which we can turn into a table
beltre_yearly <- beltre[[1]]

# Now I will clean the data and transform it to be optimal for the
# visualizations I want to make

# Because this was already a nicely compiled dataset with no missing values or
# other issues, the only cleaning it needs is removing some unwanted rows. 
# There is a fair bit of transformation necessary for the specific 
# visualizations I want to make, however, which I do below.

# Cleaning: I will remove minor league data and combined team info that also appears
# in the table from baseball reference because I don't want this data
# I will do this by removing specific rows
rows_to_remove <- c(1:3, 7, 18, 27:36)
beltre_yearly <- beltre_yearly[-rows_to_remove, ]

# Transformation for Visualization 1: I will create an All-Star dummy 
# variable which will I use for visualizing batting average against all-star status
beltre_yearly$all_star <- ifelse(beltre_yearly$Year %in% c(2010:2012, 2014), "Yes", "No")

# Transformation for Visualization 2: I will replace the team abbreviations with full 
# team names and tenure for clarity in the labeling 
beltre_yearly$Tm <- gsub("LAD", "Los Angeles Dodgers (7 Years)", beltre_yearly$Tm)
beltre_yearly$Tm <- gsub("SEA", "Seattle Mariners (5 Years)", beltre_yearly$Tm)
beltre_yearly$Tm <- gsub("BOS", "Boston Red Sox (1 Year)", beltre_yearly$Tm)
beltre_yearly$Tm <- gsub("TEX", "Texas Rangers (8 Years)", beltre_yearly$Tm)

# Then I will create a count variable for use in my treemap visualization
team_counts <- beltre_yearly %>%
  group_by(Tm) %>%
  summarise(count = n())

# Transformation for Visualization 3: I want to create a line graph with 
# multiple lines to show parallel trends, so this will require the data to be
# in long format rather than wide format


# Visualization 1: Line graph of yearly batting average, with All Star status
# highlighted
PS6a_Felkner <- ggplot(data = beltre_yearly, aes(x = Year, y = BA)) +
  geom_line(aes(group = 1)) +
  geom_point(aes(color = factor(all_star)), size = 3) + 
  scale_color_manual(values = c("black", "blue")) +
  labs(x = "Year", y = "Batting Average", color = "All Star Status",
       title = "Beltre Batting Average with All-Stars Years Highlighted") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("PS6a_Felkner.png", PS6a_Felkner, width = 8, height = 6, dpi = 300)

# Visualization 2: Treemap of his career by team tenure

# First I will define a custom color palette based on the MLB team colors
custom_palette <- c("#0C2340","#005A9C", "#005C5C","#C0111F")

# Then I will create the treemap
library(treemap)
PS6b_Felkner <- treemap(team_counts, 
        index="Tm",
        vSize = "count",
        type= "index", 
        palette = custom_palette, 
        title= "Beltre MLB Career by Team Tenure", 
        fontsize.title = 32)

ggplot(team_counts, aes(x = "", fill = Tm)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "MLB Teams Pie Chart", fill = "MLB Teams") +
  theme_void()


# Visualization 3: Scatter plot of walks vs on base percentage
PS6c_Felkner <- ggplot(beltre_yearly, aes(x = BB, y = OBP)) + 
  geom_point() +
  labs(title = "Beltre Walks vs On Base Percentage",
       x = "Walks", y = "On Base Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("PS6c_Felkner.png", PS6c_Felkner, width = 8, height = 6, dpi = 300)
  

