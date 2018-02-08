# a4-data-wrangling

################################### Set up ###################################

# Install (if not installed) + load dplyr package 

# install.packages(dplyr)
library(dplyr)

# Set your working directory to the appropriate project folder

setwd("~/Desktop/INFO201/a4-data-wrangling-EmilyCheoh")

# Read in `any_drinking.csv` data using a relative path

any.drinking <- read.csv("data/any_drinking.csv", stringsAsFactors=FALSE)

# Read in `binge.drinking.csv` data using a relative path

binge.drinking <- read.csv("data/binge_drinking.csv", stringsAsFactors=FALSE)

# Create a directory (using R) called "output" in your project directory

dir.create("output")

################################### Any drinking in 2012 ###################################

# For this first section, let's just work with the columns `state`, `location`, and the data from 2012
# (from the *any drinking* dataset)
# Create a data.frame that has the `state` and `location` columns, and all columns from 2012

any.drink.2012 <- select(any.drinking, state, location, contains("2012"))

# Using the 2012 data, create a column that has the difference in male and female drinking patterns

any.drink.2012 <- mutate(any.drink.2012, diff_2012 = males_2012 - females_2012)

# Write your 2012 data to a .csv file in your `output/` directory with an expressive filename

write.csv(any.drink.2012, 'output/any_drinking_data_2012.csv', row.names=FALSE)

# Are there any locations where females drink more than males?
# Your answer should be a *dataframe* of the locations, states, and differences for all locations (no extra columns)

females.more.than.male <- filter(any.drink.2012, diff_2012 < 0) %>%
  select(state, location, diff_2012) # nowhere

# What is the location in which male and female drinking rates are most similar (*absolute* difference is smallest)?
# Your answer should be a *dataframe* of the location, state, and value of interest (no extra columns)

similar.drinking.pattern <- any.drink.2012 %>%
  select(state, location, diff_2012) %>%
  filter(abs(diff_2012) == min(abs(diff_2012)))

# As you've (hopefully) noticed, the `location` column includes national, state, and county level estimates. 
# However, many audiences may only be interested in the *state* level data. Given that, you should do the following:
# Create a new variable that is only the state level observations in 2012

state.2012 <- any.drink.2012 %>% 
  filter(location == state) %>% 
  unique() %>%
  select(-location)

# Which state had the **highest** drinking rate for both sexes combined? 
# Your answer should be a *dataframe* of the state and value of interest (no extra columns)

highest.drinking.rate <- state.2012 %>%
  select(state, both_sexes_2012) %>%
  filter(abs(both_sexes_2012) == max(abs(both_sexes_2012)))

# Which state had the **lowest** drinking rate for both sexes combined?
# Your answer should be a *dataframe* of the state and value of interest (no extra columns)

lowest.drinking.rate <- state.2012 %>%
  select(state, both_sexes_2012) %>%
  filter(abs(both_sexes_2012) == min(abs(both_sexes_2012)))

# What was the difference in (any-drinking) prevalence between the state with the highest level of consumption, 
# and the state with the lowest level of consumption?
# Your answer should be a single value (a dataframe storing one value is fine)

diff.between.highest.and.lowest <- highest.drinking.rate$both_sexes_2012 - lowest.drinking.rate$both_sexes_2012

# Write your 2012 state data to an appropriately named file in your `output/` directory

write.csv(state.2012, 'output/2012_state_data.csv', row.names=FALSE)

# Write a function that allows you to specify a state, then saves a .csv file with only observations from that state
# You should use the entire any.drinking dataset for this function
# Make sure the file you save in the `output` directory indicates the state name, and avoid using rownames.

SaveStateData <- function(input.state) {
  information <- any.drinking %>% 
    filter(state == input.state, location == input.state) %>%
    unique() %>%
    select(-location) 
  file.name <- paste0("output/", input.state, "_data.csv")
  write.csv(information, file.name, row.names=FALSE)
  return(information)
}

# Demonstrate your function works by writing 3 .csv files of the states of your choice

washington.data <- SaveStateData("Washington")
delaware.data <- SaveStateData("Delaware")
dc.data <- SaveStateData("District Of Columbia")

################################### Binge drinking Dataset ###################################
# In this section, we'll ask a variety of questions regarding our binge.drinking dataset. 
# In order to ask these questions, you'll need to first prepare a subset of the data for this section:

# Create a dataframe with only the county level observations from the binge_driking dataset 
# (i.e., exclude state/national estimates)
# This should include "county-like" areas such as parishes and boroughs

county.binge <- binge.drinking %>% 
  filter(location != state & location != "United States" ) # doesn't include DC because TA said DC = state
  
# What is the average county level of binge drinking in 2012 for both sexes?

county.average.2012 <- county.binge %>% summarise(mean(both_sexes_2012))

# What is the minimum county level of binge drinking in each state (in 2012 for both sexes)? 
# Your answer should contain 50 values (one for each state), unless there are two counties in a state with the same value
# Your answer should be a *dataframe* with the value of interest, location, and state

min.county.level <- county.binge %>% group_by(state) %>% 
  filter(both_sexes_2012 == min(both_sexes_2012)) %>%
  select(state, location, both_sexes_2012)

# What is the maximum county level of binge drinking in each state (in 2012 for both sexes)? 
# Your answer should be a *dataframe* with the value of interest, location, and state

max.county.level <- county.binge %>% group_by(state) %>% 
  filter(both_sexes_2012 == max(both_sexes_2012)) %>%
  select(state, location, both_sexes_2012)

# What is the county with the largest increase in male binge drinking between 2002 and 2012?
# Your answer should include the county, state, and value of interest

county.biggest.increase <- county.binge %>% filter(males_2012 - males_2002 == max(males_2012 - males_2002)) %>%
  mutate(increase = males_2012 - males_2002) %>%
  select(state, location, males_2002, males_2012, increase)

# How many counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be an integer (a dataframe with only one value is fine)

num.increase.binge.male <- county.binge %>% filter(males_2012 - males_2002 > 0) %>% summarise(num_increased = n())

# What percentage of counties experienced an increase in male binge drinking between 2002 and 2012?
# Your answer should be a fraction or percent (we're not picky)

percent.increased.male <- num.increase.binge.male$num_increased / nrow(county.binge) 
                    
# How many counties observed an increase in female binge drinking in this time period?
# Your answer should be an integer (a dataframe with only one value is fine)

num.increase.binge.female <- county.binge %>% filter(females_2012 - females_2002 > 0) %>% summarise(num_increased = n())

# What percentage of counties experienced an increase in female binge drinking between 2002 and 2012?
# Your answer should be a fraction or percent (we're not picky)

percent.increased.female <- num.increase.binge.female$num_increased / nrow(county.binge)

# How many counties experienced a rise in female binge drinking *and* a decline in male binge drinking?
# Your answer should be an integer (a dataframe with only one value is fine)

num.female.increase.male.decrease <- county.binge %>% filter(females_2012 - females_2002 > 0 & males_2012 - males_2002 < 0) %>%
  summarise(num_female_increase_male_decrease = n())
                                  
################################### Joining Data ###################################
# You'll often have to join different datasets together in order to ask more involved questions of your dataset. 
# In order to join our datasets together, you'll have to rename their columns to differentiate them

# First, rename all prevalence columns in the any.drinking dataset to the have prefix "any."
# Hint: you can get (and set!) column names using the colnames function. This may take multiple lines of code.

colnames(any.drinking) <- paste0("any.", colnames(any.drinking))
colnames(any.drinking)[colnames(any.drinking) == "any.state"] <- "state"
colnames(any.drinking)[colnames(any.drinking) == "any.location"] <- "location"

# Then, rename all prevalence columns in the binge.drinking dataset to the have prefix "binge."
# Hint: you can get (and set!) column names using the colnames function. This may take multiple lines of code.

colnames(binge.drinking) <- paste0("binge.", colnames(binge.drinking))
colnames(binge.drinking)[colnames(binge.drinking) == "binge.state"] <- "state"
colnames(binge.drinking)[colnames(binge.drinking) == "binge.location"] <- "location"

# Then, create a dataframe with all of the columns from both datasets. 
# You can do this by performing a full join on the two datasets by the `location` column

any.and.binge <- full_join(any.drinking, binge.drinking) %>% # joins by state and location automically
  unique()

# Create a column of difference b/w `any` and `binge` drinking for both sexes in 2012

any.and.binge <- any.and.binge %>% mutate(diff_both_sexes_2012 = any.both_sexes_2012 - binge.both_sexes_2012)

# Which location has the greatest *absolute* difference between `any` and `binge` drinking?
# Your answer should be a one row data frame with the state, location, and value of interest (difference)

greatest.difference <- any.and.binge %>% filter(abs(diff_both_sexes_2012) == max(abs(diff_both_sexes_2012))) %>%
  select(state, location, diff_both_sexes_2012)

# Which location has the smallest *absolute* difference between `any` and `binge` drinking?
# Your answer should be a one row data frame with the state, location, and value of interest (difference)

smallest.difference <- any.and.binge %>% filter(abs(diff_both_sexes_2012) == min(abs(diff_both_sexes_2012))) %>%
  select(state, location, diff_both_sexes_2012)

################################### Write a function to ask your own question(s) ###################################
# Even in an entry level data analyst role, people are expected to come up with their own questions of interest
# (not just answer the questions that other people have). For this section, you should *write a function*
# that allows you to ask the same question on different subsets of data. 
# For example, you may want to ask about the highest/lowest drinking level given a state or year. 
# The purpose of your function should be evident given the input parameters and function name. 
# After writing your function, *demonstrate* that the function works by passing in different parameters to your function.

# Returns the highest binge drinking level (for both sexes) among all states of the given year
HighestBingeLevel <- function(year) {
  col.of.interest <- paste0("binge.both_sexes_", year)
  binge.drinking.by.state <- binge.drinking %>% 
    filter(state == location) %>%
    unique()
  max.binge.level <- max(binge.drinking.by.state[,col.of.interest])
  return(max.binge.level)
}

highest.binge.level.2002 <- HighestBingeLevel(2002)
highest.binge.level.2012 <- HighestBingeLevel(2012)

################################### Challenge ###################################

# Using your function from part 1 that wrote a .csv file for given a state name, write a file for all 50 states
# You should be able to do this in a *single line of (concise) code*


all.state <- apply(as.vector(state.2012 %>% select(state)), 1, SaveStateData) # writes one file for each state

# Using a dataframe of your choice from above, write a function that allows you to specify a *year* and *state* of interest, 
# that saves a csv file with observations from that state's counties. 
# It should only write the columns `state`, `location`, and data from the specified year. 
# Before writing the .csv file, you should *sort* the data.frame in descending order
# by the both_sexes drinking rate in the specified year. 
# Again, make sure the file you save in the output directory indicates the year and state. 
# Note, this will force you to confront how dplyr uses *non-standard evaluation*
# Hint: https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html

# Year and state.of.interest should both be strings
# Uses the any.drinking dataframe
StateYearlyData <- function(year, state.of.interest) {
  both.sexes <- paste0("any.both_sexes_", year)
  descending.file <- any.drinking %>% arrange_(paste("-", both.sexes)) %>%
    select(state, location, contains(year)) %>%
    filter(state == state.of.interest) %>%
    unique()
  file.name <- paste0("output/", state.of.interest, "_", year, ".csv")
  write.csv(descending.file, file.name, row.names=FALSE)
  return(descending.file)
}

# finally works!! ><
washington.2012 <- StateYearlyData("2012", "Washington")
