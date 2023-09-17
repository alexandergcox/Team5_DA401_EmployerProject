# 1. Load and explore the data

# Install tidyverse.
install.packages('tidyverse')

# Import the tidyverse library.
library(tidyverse) 
library(stringr)

# Determine your working directory
getwd()

# Change your current directory.
setwd(dir='C:/Users/alexc_dra7fx5/OneDrive/Desktop/LSE/R/Data')

# Import the data set.
goal_stats <- read.csv('goal_stats.csv', header=TRUE) 

# Print the data frame.
View(goal_stats) 
as_tibble(goal_stats)

# View the descriptive statistics.
# View a summary of the data frame.
summary(goal_stats)

# Determine the typeof() of the data frame.
typeof(goal_stats)

# Determine the class() of the data frame.
class(goal_stats)

# Check the dimensions of the data frame.
dim(goal_stats)

# Check data types by variable
glimpse(goal_stats)

# Explore missing values
sum(is.na(goal_stats))

#Determine each columns datatypes
sapply(goal_stats, class)

# Explore unique values in variables
length(unique(goal_stats$`Date`))
# 215 different dates

length(unique(goal_stats$`Campaign`))
# 29 campaigns

length(unique(goal_stats$`Audience`))
# 9 different audiences

length(unique(goal_stats$`Platform`))
# 8 different platforms

length(unique(goal_stats$`Goal`))
# 12 different goals

length(unique(goal_stats$`Completion`))
# 50 different completions

length(unique(goal_stats$`Ad.Format`))
# 12 different ad formats

length(unique(goal_stats$`Campaign.Traffic`))
# 3 different campaign traffic

length(unique(goal_stats$`Days.away.from.max.date`))
# 215 different days

length(unique(goal_stats$`Latest.report`))
# 3 different reports

length(unique(goal_stats$`Creative...Family`))
# 22 different creative family

length(unique(goal_stats$`Creative...Version`))
# 54 different creative versions

# View all colnames
colnames(goal_stats)

# Fill empty cells with NA
goal_stats <- read.csv("goal_stats.csv", header=T, na.strings=c("","NA"))

# View all missing values 
sum(is.na(goal_stats))
# There are 36333 missing values

# Observations missing by column.
sapply(goal_stats, function(x) sum(is.na(x)))
# Above sum of missing values per column.
# Sort in descending order.
sort(sapply(goal_stats, function(x) sum(is.na(x))), decreasing = TRUE)

# To return rows with missing values in any variable of the data frame.
goal_stats[rowSums(is.na(goal_stats)) > 0,]

# Assumption is that general targeting is group 6 and domain targeting.
# Refers to group 5 which is website based. 
goal_stats$Audience <- str_replace(goal_stats$Audience, "General Targetting", "6")
goal_stats$Audience <- str_replace(goal_stats$Audience, "Domain Targeting", "5")
unique(goal_stats$Audience)
# Confirmed changes are made correctly.  We still have the NA values.

# Change column titles for better coding and removing spaces.  
names(goal_stats)
names(goal_stats) <- c("date", "campaign", "audience", "creative_family", "creative_version", 
                     "platform", "ad_format", "goal", "completions", 
                     "campaign_traffic", "days_from_max_date", "latest_report") 
# Check changes made correctly to column names.
names(goal_stats)
# Confirmed correction.

# New dataframe to explore duplicate, counted  values, to explore any possible patterns
goal_duplicates <- goal_stats %>%
  add_count(date, campaign, audience, creative_family, creative_version, platform, ad_format, 
            goal, completions, campaign_traffic, days_from_max_date, latest_report)%>%
  filter(n>1) %>%
  distinct()

# Export to csv
write_csv(goal_duplicates, "goal_duplicates.csv")

#Remove duplicate
goal_stats1 <- goal_stats %>%
  distinct(.keep_all = TRUE)


