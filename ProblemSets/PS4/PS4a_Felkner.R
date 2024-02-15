# load the necessary packages
library(jsonlite)
library(tidyverse)

# downloaded the necessary data
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101& end_date=20240209&lang=en"')

# view the data in the original hard-to-read format
# system("cat dates.json")
# commenting out above line so that it doesn't run bc it is very slow and redundant to later steps
# convert the data into a dataframe

mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# check the class of date variable
class(mydf$date)

# date is a character variable

# Look at first n rows of data (since n is not specified, I am choosing 
# n=10 because that seems reasonable)
head(mydf, n=10)
