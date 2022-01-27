#PSYC 259 Homework 2 - Data Transformation
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")
  
### Question 1 ---------- 

#Use glimpse to check the type of "Year". 
#Then, convert it to a numeric, saving it back to 'ds'
#Use typeof to check that your conversion succeeded

#ANSWER
glimpse(ds$Year)
ds$Year <- as.numeric(ds$Year)
ifelse(typeof(ds$Year)=='double','Yes, I did it!','Oh no...I failed')

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER
ds_lower1<-ds %>% rename_with(tolower)
ds_lower2<-rename_with(ds, tolower) # I'm still used to this one...
  
### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

#ANSWER
ds1 <- ds %>% mutate(
  Year2=floor(Year/10)*10)
ds2 <- mutate(ds, 
              Year2=floor(Year/10)*10)

### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER
ds_arranged<-ds %>% arrange(Rank)

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER
top10 <- ds %>% filter(Rank<=10) %>% select(Song, Artist)

### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

#ANSWER
ds_sum <- ds %>% summarize(Year_min=min(Year[!is.na(Year)]),
                           Year_recent=max(Year[!is.na(Year)]),
                           Year_mean=mean(Year[!is.na(Year)])
                           )

### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year

#ANSWER
hahaha1 <- ds %>% 
  filter(Year==min(Year[!is.na(Year)]) | Year==max(Year[!is.na(Year)]) | Year==round(mean(Year[!is.na(Year)]))) %>% 
  arrange(Year)
# alternative
hahaha2 <- ds %>% 
  filter(Year %in% c(min(Year[!is.na(Year)]), max(Year[!is.na(Year)]), round(mean(Year[!is.na(Year)])))) %>% 
  arrange(Year)

### Question 8 ---------- 

# There's and error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER
ds_correct <- ds %>%
      mutate(Year = ifelse(Song=='Brass in Pocket', 1979, Year))
ds_sum_correct <- ds_correct %>% summarize(Year_min=min(Year[!is.na(Year)]),
                           Year_recent=max(Year[!is.na(Year)]),
                           Year_mean=mean(Year[!is.na(Year)])
)
hahaha_correct <- ds_correct %>% 
  filter(Year==min(Year[!is.na(Year)]) | Year==max(Year[!is.na(Year)]) | Year==round(mean(Year[!is.na(Year)]))) %>% 
  arrange(Year)

### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER
ds %>% filter(!is.na(Year)) %>%
  group_by(Year) %>% 
  summarize(mean_Rank_group=mean(Rank),
      N_group=length(Year))

### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

#ANSWER
hahahaha<-ds %>% 
  count(Year) %>%
  slice_max(n)
  