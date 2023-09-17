# Exploring and basic cleaning of the raw creative data set.
# Generated on R version 4.3.1 (2023) and RStudio (2023.06.2).

# Working directory set, via settings.
# install.packages(tidyverse)
library(tidyverse)
library(stringr)

creative <- read_csv("creative.csv")

# Check variable names.
names(creative)
# There are 21 variables, they are described in word/PDF document:
# ('creativedata_cleaning').  

# Check dimensions of the data.
dim(creative)
# 102070 rows and 21 variables.

# Check data types.
glimpse(creative)
# This gives the datatypes too by variable.

# Unique values in variables.
length(unique(creative$`Requested URL Path`))
# There are 58 unique values in URL paths.
unique(creative$Campaign)
length(unique(creative$Campaign))
# There are 205 unique Campaigns.
unique(creative$`Ad Format`)
length(unique(creative$`Ad Format`))
# There are 15 unique Ad formats.
unique(creative$Clicks)
length(unique(creative$Clicks))
# There are 87 unique number of clicks.
unique(creative$Spend)
length(unique(creative$Spend))
# There are 70818 values in decimals.
unique(creative$`Creative - Size`)
length(unique(creative$`Creative - Size`))
# There are 100 unique Creative sizes.
unique(creative$Date)
length(unique(creative$Date))
# There are 221 unique dates.
unique(creative$Impressions)
length(unique(creative$Impressions))
# There are 5499 unique numeric values of impressions.
unique(creative$Reach)
length(unique(creative$Reach))
# 2927 unique reach values.
unique(creative$Audience)
# 7 unique results that appear as categories.
unique(creative$Platform)
# 9 unique platforms.
unique(creative$`Creative - Family`)
length(unique(creative$`Creative - Family`))
# 14 unique results.  
unique(creative$`Creative - Version`)
length(unique(creative$`Creative - Version`))
# 40 unique results for Creative - Version.  Includes NA.
unique(creative$`100% Video Views`)
length(unique(creative$`100% Video Views`))
# 1409 unqiue values for 100% video views.
unique(creative$`Video Starts`)
length(unique(creative$`Video Starts`))
# 1618 unique video start values.
unique(creative$Follows)
length(unique(creative$Follows))
# 13 unique values for follows, includes NA.  
unique(creative$`Days away from max date`)
length(unique(creative$`Days away from max date`))
# 221 unique days away from max date, includes NA.
unique(creative$`Latest report?`)
# 3 results for latest report, binary zero or 1, includes NA.
unique(creative$CTR)
length(unique(creative$CTR))
# 7131 unique values.  Unknown if includes NA as yet.
unique(creative$`CTR Score`)
# 4 unique values, for CTR score, includes NAs.
unique(creative$`Weighted CTR Score`)
length(unique(creative$`Weighted CTR Score`))
# 5569 unique values, for weighted CTR, unclear if includes NAs as yet.


# Exploring total number of missing values in df.
sum(is.na(creative))
# There are 466027 missing values in the dataframe.

# Observations missing by column.
sapply(creative, function(x) sum(is.na(x)))
# Above sum of missing values per column.
# Sort in descending order.
sort(sapply(creative, function(x) sum(is.na(x))), decreasing = TRUE)



# To return rows with missing values in any variable of the data frame.
creative[rowSums(is.na(creative)) > 0,]
# There are 102,070 rows with missing values.

# To check for '-999' values that may be used as NA.
length(which(creative$Clicks == -999))
# None
length(which(creative$Spend == -999))
# None
length(which(creative$`Creative - Size` == -999))
# None
length(which(creative$Date == -999))
# None
length(which(creative$Impressions == -999))
# None
length(which(creative$Reach == -999))
# None
length(which(creative$Audience == -999))
# None
length(which(creative$`100% Video Views` == -999))
# None
length(which(creative$`Video Starts` == -999))
# None
length(which(creative$Follows == -999))
# None
length(which(creative$`Latest report?` == -999))
# None
length(which(creative$CTR == -999))
# None
length(which(creative$`CTR Score` == -999))
# None
length(which(creative$`Weighted CTR Score` == -999))
# None
# None of the columns had the number -999 used instead of NA.


# Data cleaning before returning to exploration and visualisation.
# Replace several equivalent mis-spelt values inc column 'Creative - Version'.
creative$`Creative - Version` <- str_replace(creative$`Creative - Version`, 
                                             "OnePage", "1Page")
creative$`Creative - Version` <- str_replace(creative$`Creative - Version`, 
                                             "1page", "1Page")
length(unique(creative$`Creative - Version`))
# 38 unique values.
# This confirms two of the specified errors were changed to the same string 
# category to "1Page".

# Check incorrect categories in Audience column.
unique(creative$Audience)  
# Assumption is that general targeting is group 6 and domain targeting.
# Refers to group 5 which is website based. 
creative$Audience <- str_replace(creative$Audience, "General Targetting", "6")
creative$Audience <- str_replace(creative$Audience, "Domain Targeting", "5")
unique(creative$Audience)
# Confirmed changes are made correctly.  We still have the NA values.

# Check data type of Spend column.
class(creative$Spend)
# Comes up as character.  We need to change to numeric.
creative$Spend <- as.numeric(creative$Spend)
# Check data type.
class(creative$Spend)
# Confirmed numeric.

# Make formatting of categories consistent.
# In Ad Format column change non abbreviation words in capitals to consistent.
# format.
# Confirm spelling of unique observations.
unique(creative$`Ad Format`)
# Change formatting to initial letter in caps only except abbreviations.
creative$`Ad Format` <- str_replace(creative$`Ad Format`, "MOBILE", "Mobile")
creative$`Ad Format` <- str_replace(creative$`Ad Format`, "TABLET", "Tablet")
creative$`Ad Format` <- str_replace(creative$`Ad Format`, "DESKTOP", "Desktop")
# Check changes to observation format.
unique(creative$`Ad Format`)
# Successful change of format.

# Change column titles for better coding and removing spaces.  
names(creative)
names(creative) <- c("URL", "campaign", "ad_format", "clicks", "spend", 
                     "creative_size", "date", "impressions", "reach", 
                     "audience", "platform", "creative_family", 
                     "creative_version", "full_video_views", "video_starts", 
                     "follows", "days_from_max", "latest_report", "CTR", 
                     "CTR_score", "weighted_CTR_score")
# Check changes made correctly to column names.
names(creative)
# Confirmed correction.

# Checking for duplicates before removing unnecessary columns or correcting NAs.
# Make sure you know the column names and include them all.

# New data frame to explore duplicate, counted  values, to explore any possible
# patterns/process rationale. 
creative_duplicates <- creative %>%
  add_count(URL, campaign, ad_format, clicks, spend, creative_size, date, 
            impressions, reach, audience, platform, creative_family, 
            creative_version, full_video_views, video_starts, follows, 
            days_from_max, latest_report, CTR, CTR_score, weighted_CTR_score)%>%
  filter(n>1) %>%
  distinct()
# Use environment to check creative_duplicates data frame, to check before 
# deleting.
# Looks like there are 196 duplicates.  These will be exported as csv to view 
# in excel for ease.  The new column at the end counts how many copies there 
# are of each line, which is helpful for insight. 
write_csv(creative_duplicates, "creative_duplicates.csv")
# Check unique values for new column n in new dataframe. See discussion in 
# Technical report.
# Remove these duplicates.
creative1 <- creative %>%
  distinct(.keep_all = TRUE)
# Check updated status of dataframe.
# This leaves us with 101,874 rows and 21 variables.
glimpse(creative1)

# Drop unnecessary columns to focus on the missing data and what to do about it.
# As advised by Agent3.
creative2 <- subset(creative1, select = -c(URL, days_from_max))
glimpse(creative2)
# Confirmed 19 columns left.

# Observations missing by column.
sapply(creative1, function(x) sum(is.na(x)))
# Above sum of missing values per column.
# Sort in descending order.
sort(sapply(creative1, function(x) sum(is.na(x))), decreasing = TRUE)

# Filter out rows that are completely NA.
creative1 %>%
  filter(clicks == "NA" & spend == "NA" & reach == "NA" & 
           full_video_views == "NA" & video_starts == "NA" & follows == "NA" &
           CTR == "NA")
# There are no observations that have all the above variables missing.
# To return rows with missing values in any variable of the data frame.
creative1[rowSums(is.na(creative1)) > 0,]
creative2[rowSums(is.na(creative2)) >0,]
# Exploring total number of missing values in df.
sum(is.na(creative))
sum(is.na(creative1))
sum(is.na(creative2))

# If you choose to export dataframes:
# write_csv(creative, "creative_duplicates.csv")
# write_csv(creative1, "creative1.csv")
# Above just has duplicates removed.
# write_csv(creative2, "creative2.csv")
# Above has duplicates and url and 'days_from_max' variables removed.

# Export dataframe for others in the team to use.  Creative1 includes all 
# columns and variables, is tidied in terms of names. It excludes duplicates.  
# It includes NAs,original decimals and outliers until a consensus decision is 
# made.    
# write.csv(creative1, "creative1.csv", 
#row.names = TRUE)

# ***************************************************************************
# we will discuss as a team re handling NA values. I will send out a 
# survey re decimals decisions.
# We have chosen to keep NA values 9use workarounds in code script and 
# visualisations), all decimals to be kept true (can round as needed).
# ***************************************************************************



# ***** Basic statistics section.********************************************

library(psych)
library(moments)

# Basic statistics check. Describe also informs re kurtosis and skew.
# Includes skew, sd, se and kurtosis (from the psych package in R).  Note that 
# the data is probably not fully meaningful for categorical data but can explore 
# the frequency of categorical data.
# Use range, mean, median data as a measure of central tendency and dispersion.
# Summary() also includes quartile information.

describe(creative1)
describe(creative2)
summary(creative1)
summary(creative2)


# Define function that calculates mode.
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# Calculate mode for each variable.
sapply(creative1, find_mode)
sapply(creative2, find_mode)

# Data types for all variables to use for frequency table of categorical data.
# Use sapply() to check data types for all columns
data_types <- sapply(creative1, class)
print(data_types)

# Descriptive statistics for categorical data, summary frequency of values.
sort(table(creative1$campaign), decreasing = TRUE)
sort(table(creative1$ad_format), decreasing = TRUE)
sort(table(creative1$creative_size), decreasing = TRUE)
sort(table(creative1$audience), decreasing = TRUE)
sort(table(creative1$platform), decreasing = TRUE)
sort(table(creative1$creative_family), decreasing = TRUE)
# sort(table(creative1$creative_version), decreasing = TRUE)


# Frequency bar plots.
barplot(sort(table(creative1$campaign), ylab = 'freq campaigns'), 
        decreasing = TRUE)
# This plots is too busy and would need to be broken down to be useful (above).
# Frequency tables are more helpful above.
barplot(sort(table(creative1$ad_format), ylab = 'freq campaigns'), 
        decreasing = TRUE)
barplot(sort(table(creative1$creative_size), ylab = 'freq campaigns'), 
        decreasing = TRUE)
# Not very helpful above - use frequency tables or break up.
barplot(sort(table(creative1$audience), ylab = "freq campaigns"), 
        decreasing = TRUE)
barplot(sort(table(creative1$platform), ylab = "freq campaigns"), 
        decreasing = TRUE)
barplot(sort(table(creative1$creative_family), ylab = "freq campaigns"), 
        decreasing = TRUE)
barplot(sort(table(creative1$creative_version), ylab = "freq campaigns"), 
        decreasing = TRUE)

# Looking for outliers.
# Checking on spend first.
hist(creative1$spend, xlab = "Spend", main = "Histogram of spend")
# All values on first column almost.  No real tail as such. If anything could 
# be called right skewed. 
# Checking skew and kurtosis of spend data:
skewness(creative1$spend, na.rm = TRUE)
# Result is: 30.43653.  
kurtosis(creative1$spend, na.rm = TRUE)
# Results is: 1494.258.
# These values show that this data is not normally distributed.
# The worry here is that this would cause big problems in terms of non normal 
# distribution and possibly non linear data and therefore requiring non 
# parametric analysis.  
# Chcek using boxplot for outliers:
boxplot(creative1$spend, ylab = "Spend", main = "Boxplot for spend")
# The result is very flat with a lot of outliers. On researching, this may be 
# because When a boxplot is flat but has lots of outliers, it means that there 
# are many data points that are significantly different from other data points 
# in the dataset, but they are distributed evenly across the range of values. 
# This could happen when there is a large amount of noise in the data or when 
# there are multiple subgroups within the dataset that have different 
# distributions.  Research also shows that financial data can often be skewed.



# ***** Basic associations section.********************************************

# Explore relationships between categorical variables - considering audiences.

View(table(creative1$audience, creative1$campaign))
# Can be ordered as descending in View window.
# Export table as csv for better analysis.
# Generate the table
rel_audience_campaign <- table(creative1$audience, creative1$campaign)
# Convert the table to a data frame for easier export
rel_audience_campaign_df <- as.data.frame(rel_audience_campaign)
# Export the data frame to a CSV file
write.csv(rel_audience_campaign_df, "rel_audience_campaign_df.csv", 
          row.names = TRUE)
# *************************this will show the targeting of adverts by audience.

table(creative1$audience, creative1$ad_format)
table(creative1$audience, creative1$platform)
table(creative1$audience, creative1$creative_family)
table(creative1$audience, creative1$creative_version)


# Average spend by audience.
creative1 |>
  group_by(audience)|>
  summarize (avg_spend = mean(spend, na.rm = TRUE))
# This indicates which audience groups the campaign strategy has, in effect, 
# spent most on. 

# Average full video view by audience.
creative1 |>
  group_by(audience)|>
  summarize (avg_full_video = mean(full_video_views, na.rm = TRUE))
# This indicates how successful video advertising is with each audience group 
# in terms of advertising.

# Average clicks by audience group.
creative1 |>
  group_by(audience)|>
  summarize (avg_clicks = mean(clicks, na.rm = TRUE))
# Using clicks as an indicator, suggests how successful advertising has been 
# by audience group.   

# Average CTR by audience group.
creative1 |>
  group_by(audience)|>
  summarize (avg_CTR = mean(CTR, na.rm = TRUE))
# In terms of advertising success where CTR is described as clicks divided by 
# impressions and therefore how successful the targeting of advertising is by 
# audience group.

# Average follows by audience group.
creative1 |>
  group_by(audience)|>
  summarize (avg_follows = mean(follows, na.rm = TRUE))
# Consider if follows were considered a mark of success or conversion, view 
# impact in order.  

# Average weighted CTR score by audience group.
creative1 |>
  group_by(audience)|>
  summarize (avg_weighted_CTR = mean(weighted_CTR_score, na.rm = TRUE))
# Presumably this is weighted in a way that interests A3 and CW.  
# If this were a mark of success/conversion, consider results in order.

# Average impressions by audience group.
creative1 |>
  group_by(audience)|>
  summarize (avg_impressions = mean(impressions, na.rm = TRUE))
# If this were interpreted as the number of opportunities for engagement 
# created by advertising approach per audience group.

# Average reach by audience group.
creative1 |>
  group_by(audience)|>
  summarize (avg_reach = mean(reach, na.rm = TRUE))
# If this were interpreted as exposure by individuals.  

# ************* I have not included video starts, but I can.
options(dplyr.print_max = 30)  
# Set the maximum number of printed rows

# Exploring campaign by average spend.
creative1 |>
  group_by(campaign)|>
  summarize(avg_spend_camp = mean(spend, na.rm = TRUE)) |>
  arrange(desc(avg_spend_camp)) |>
  print(n=30)
# Top 30 highest spend ads.  Note 418 are not accounted for due to NA.  

# Exploring average clicks by campaign.
creative1 |>
  group_by(campaign)|>
  summarize(avg_clicks_camp = mean(clicks, na.rm = TRUE)) |>
  arrange(desc(avg_clicks_camp)) |>
  print(n=30)
# Consider in terms of campaign success.

# Exploring average impressions by campaign.
creative1 |>
  group_by(campaign)|>
  summarize(avg_impressions_camp = mean(impressions, na.rm = TRUE)) |>
  arrange(desc(avg_impressions_camp)) |>
  print(n=30)
# An indication of top 30 exposure of campaigns.  

# Exploring the avg reach by campaign.
creative1 |>
  group_by(campaign)|>
  summarize(avg_reach_camp = mean(reach, na.rm = TRUE)) |>
  arrange(desc(avg_reach_camp)) |>
  print(n=30)
# Consider top 30 exposure by campaign.

# Exploring full video views by campaign.
creative1 |>
  group_by(campaign)|>
  summarize(avg_fullvideo_camp = mean(full_video_views, na.rm = TRUE)) |>
  arrange(desc(avg_fullvideo_camp)) |>
  print(n=30)
# Looks like either there are only 11 videos or only 11 marketing videos 
# have been fully viewed.  

# Exploring follows by campaign.
creative1 |>
  group_by(campaign)|>
  summarize(avg_follows_camp = mean(follows, na.rm = TRUE)) |>
  arrange(desc(avg_follows_camp)) |>
  print(n=30)
# Looks like either only two campaigns generated follows or were offered on 
# platforms where customers might follow.  None of these seem to be targeted 
# by a particular audience group.

# Exploring CTR by campaign.
creative1 |>
  group_by(campaign)|>
  summarize(avg_CTR_camp = mean(CTR, na.rm = TRUE)) |>
  arrange(desc(avg_CTR_camp)) |>
  print(n=30)
# Looks like all targeted campaigns to audiences are represented in top 30 
# except audience 6. NA is in 19th position.  

# Exploring weighted CTR by campaign, assuming this is weighted by A3 priorities
creative1 |>
  group_by(campaign)|>
  summarize(avg_wCTR_camp = mean(weighted_CTR_score , na.rm = TRUE)) |>
  arrange(desc(avg_wCTR_camp)) |>
  print(n=30)
# Audiences 4 and 5 seem to be the most successfully targeted in the top 30, 
# followed on by audience 3 and 1. The No Lock campaign is the most successful.

# Exploring clicks by creative_family adverts.
creative1 |>
  group_by(creative_family)|>
  summarize(avg_clicks_family = mean(clicks, na.rm = TRUE)) |>
  arrange(desc(avg_clicks_family)) |>
  print(n=14)
# These all seem to be clickable, digital adverts. Some are targeted to 
# audiences,but only audiences 1, 2 and 3.  The most successfully targeted seems
# to be audience 1.

# Exploring spend by creative_family adverts.
creative1 |>
  group_by(creative_family)|>
  summarize(avg_spend_family = mean(spend, na.rm = TRUE)) |>
  arrange(desc(avg_spend_family)) |>
  print(n=14)
# This generally reflects the order of success by clicks, but the spend on SEM 
# adverts is less than the return by clicks, for example. 

# Exploring impressions by creative_family adverts.
creative1 |>
  group_by(creative_family)|>
  summarize(avg_impressions_family = mean(impressions, na.rm = TRUE)) |>
  arrange(desc(avg_impressions_family)) |>
  print(n=14)
# If impressions are the number of times users saw the advert, then the spend 
# on Ad1 Domain as the top spend was worth it.  Similarly for Carousel Ad1. 

# Exploring reach by creative_family adverts.
creative1 |>
  group_by(creative_family)|>
  summarize(avg_reach_family = mean(reach, na.rm = TRUE)) |>
  arrange(desc(avg_reach_family)) |>
  print(n=14)
# Unfortunately the adverts that are unrecorded hold the highest reach as an 
# unlabelled group.  Otherwise, the highest reach (total number of unique 
# accounts that have seen the advert) corresponds with CompetitiveOpportunity, 
# followed on by ClosetTwins and CloseFaster in similar third place.

# Exploring follows by creative_family adverts.
creative1 |>
  group_by(creative_family)|>
  summarize(avg_follows_family = mean(follows, na.rm = TRUE)) |>
  arrange(desc(avg_follows_family)) |>
  print(n=14)
# There is no useworthy information here as the adverts that allowed for 
# following remain un-labelled.  

# Exploring CTR by creative_family adverts.
creative1 |>
  group_by(creative_family)|>
  summarize(avg_CTR_family = mean(CTR, na.rm = TRUE)) |>
  arrange(desc(avg_CTR_family)) |>
  print(n=14)
#  SEM ads with the lowest spend on them had the highest CTR.  
# CompetitiveOpportunity which had one of the highest click numbers, here has 
# one of the lowest CTR numbers.  

# Exploring weighted_CTR_score by creative_family adverts.
creative1 |>
  group_by(creative_family)|>
  summarize(avg_wCTR_family = mean(weighted_CTR_score, na.rm = TRUE)) |>
  arrange(desc(avg_wCTR_family)) |>
  print(n=14)

# Make sure necessary libraries.
library(dplyr)
#install.packages("magrittr")
# Not needed.
# Check for correlation across dataframe creative1.  This is only possible for 
# numeric, not categoric data.
# Organise new df with only numeric data.
for_corr <- creative1 |>
  select(clicks, spend, impressions, reach, full_video_views, follows, CTR, 
         CTR_score, weighted_CTR_score)

cor(for_corr, use = "complete.obs")
# Upload correct library for visualisations.
# install.packages("corrplot")
library(corrplot)
# Remove rows with missing values
complete_for_corr <- na.omit(for_corr)
# Create a correlation matrix using complete data
corr_matrix <- cor(complete_for_corr)
plot.new()
dev.off()
# Create a correlation plot using corrplot
corrplot(corr_matrix, method = "color", tl.cex = 0.7)
# Positive and negative associations described in further detail in technical 
# document.
corrplot(corr_matrix, method="number", tl.cex = 0.7)


# Using the Cramer V function to assess correlation between categorical values.
# Install packages.
#install.packages("vcd")
# Upload library for use.
library(vcd)
# Loading required package: grid, means the grid package has been installed too.
# Create a contingency table
audience_campaign <- table(creative1$campaign, creative1$audience)
# Calculate Cramér's V
cramers_v <- assocstats(audience_campaign)$cramer
cramers_v
# 0.9692853, suggests there is a very strong association between audience and 
# campaign.

# Check association between audience and ad_format.
adformat_audience <- table(creative1$ad_format, creative1$audience)
# Calculate Cramér's V
cramers_v <- assocstats(adformat_audience)$cramer
cramers_v
# 0.4496978, there is no significant association between audience and ad_format.

# Check association between audience and creative_size.
audience_creativesize <- table(creative1$creative_size, creative1$audience)
# Calculate Cramér's V
cramers_v <- assocstats(audience_creativesize)$cramer
cramers_v
# 0.4998995, there is no significant association between audience and 
# creative_size.

# Check association between audience and platform.
audience_platform <- table(creative1$platform, creative1$audience)
# Calculate Cramér's V
cramers_v <- assocstats(audience_platform)$cramer
cramers_v
# 0.4667332, There is no significant association between audience and platform.

# Check association between audience and creative_family.
audience_creativefamily <- table(creative1$creative_family, creative1$audience)
# Calculate Cramér's V
cramers_v <- assocstats(audience_creativefamily)$cramer
cramers_v
# 0.3163758, There is no significant association between audience and 
# creative_family.

# Check association between audience and creative_version.
audience_creativeversion <- 
  table(creative1$creative_version, creative1$audience)
# Calculate Cramér's V
cramers_v <- assocstats(audience_creativeversion)$cramer
cramers_v
# 0.4712812, There is no significant association between audience and 
# creative_version.

# Check association between date and campaign.
campaign_date <- 
  table(creative1$campaign, creative1$date)
campaign_date
# Calculate Cramér's V
cramers_v <- assocstats(campaign_date)$cramer
cramers_v
# 0.09615338.
# There is only one strong association and that is between the campaign and the 
# audience.  This suggests that in terms of marketing strategy, categorical 
# variables, other than campaign, don’t seem to play a role in targeting the 
# audience.

# Explore creative_family and audience relationship.
creative1 %>%
  ggplot(aes(creative_family, fill = audience))+
  geom_bar(alpha = 0.5)+
  coord_flip()+
  theme_bw()
# Indicates how limited the data is for audience in creative_family.  
# Shows efforts to target audience by campaign proxy creative_family and that in
# the creative data set, there are creative_family variables that may not be 
# very informative and might not be necessary to include.

# Look for ad_format exposure (impressions) and audience associations.
creative1 %>%
  ggplot(aes(ad_format, impressions, fill = audience))+
  geom_col(alpha = 0.5)+
  coord_flip()+
  theme_bw()
# Advert exposure by audience type differs depending on the ad_format.  
# Some ad_formats are very specific to an audience – (audience 6 exclusively 
# targeted through TV, Tablet, mobile, follower ads, desktop and CPC, while the 
# remaining platforms address a mixture of audience groups).  Generally, 
# audience 6 have their own exclusive ad_format exposures, except Display, which 
# they are also addressed through with others. 

#It looks like the No Lock Campaign has snuck into ad_platform, so that will 
# need cleaning. 1 row of data was removed due to missing values.

# Look for platform exposure and audience associations.
creative1 %>%
  ggplot(aes(platform, impressions, fill = audience))+
  geom_col(alpha = 0.5)+
  coord_flip()+
  theme_bw()
# 1 row was removed due to missing values. 
# Audience 6 has its own exclusive platforms with Google SEM, OTT and Trade 
# Media, although they are also exposed through Domain Display and LinkedIn.  
# The largest amount of exposure as defined by impressions is through Domain 
# Display across the board, followed on by User ID Display. Audience 2 seems to 
# be reached out to the least across audiences.  1 row was automatically removed 
# due to missing values.  

# Visualise spend by audience group.
creative1 %>%
  ggplot(aes(audience, spend))+
  geom_col(alpha = 0.5)+
  coord_flip()+
  theme_bw()
# 3970 rows were removed due to missing values.  The highest spend was on the 
# audience 6 group, followed in descending order by audience 4, 5, 1, 3, 2. This
# is slightly different to average spend by audience group. This doesn't look at
# which campaigns were used per audience group and what wa sspent on them.

# Explore clicks by audience, by campaign.
creative1 %>%
  ggplot(aes(creative_family, clicks, colour = audience))+
  geom_point(alpha = 0.5)+
  theme_bw()+
  coord_flip()
# Beware that due to many missing values, we may have biased results.  
# Otherwise, clear distinction between audience clicks on creative_family.  
# I would prefer to do this with campaigns, but because there are so many, I 
# will need to splice them.

# To explore impact of campaign on other success variables, including the
# audience, we may need to split the campaigns in different plots.
# Check the number of campaigns again.
length(unique(creative1$campaign))
# There are 205 campaigns.  It looks like 50 variables on x or y axis is more 
# easy to view. The alternative is to change the size of the plot when we save
# the plot.  Using geom_col instead of geom_bar will allow us to make sure we 
# are not using a frequency of x campaign represented as y, but the value of 
# variables like spend or clicks.

# Visualising relationship between campaigns, spend and audience.
# One plot for first 50 unique campaigns, spend and audience - see filter.
# This syntax and plot removes missing values.
filtered_x <- creative1 %>%
  distinct(campaign) %>%
  head(50) %>%
  pull(campaign)

f50_creative <- creative1 %>%
  filter(campaign %in% filtered_x)

f50_creative %>%
  ggplot(aes(x = campaign, y = spend, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 3347 rows of missing values were removed.

# Extract the second set of 50 unique campaigns, spend and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(51:101) %>%  
  pull(campaign)

f51_101_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f51_101_creative %>%
  ggplot(aes(x = campaign, y = spend, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 525 rows with missing values were removed.

# Extract the third set of 50 unique campaigns, spend and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(102:151) %>%  
  pull(campaign)

f102_151_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f102_151_creative %>%
  ggplot(aes(x = campaign, y = spend, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# No rows with missing values were found in this section.

# Extract the fourth set of 50 unique campaigns, spend and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(152:203) %>%  
  pull(campaign)

f152_203_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f152_203_creative %>%
  ggplot(aes(x = campaign, y = spend, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 98 rows with missing values were removed.

# Remaining 2 campaigns.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(204:205) %>%  
  pull(campaign)

f204_205_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f204_205_creative %>%
  ggplot(aes(x = campaign, y = spend, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# No rows were removed due to NAs.

# Visualising campaign, clicks and audience.  First set.
filtered_x <- creative1 %>%
  distinct(campaign) %>%
  head(50) %>%
  pull(campaign)

f50_creative <- creative1 %>%
  filter(campaign %in% filtered_x)

f50_creative %>%
  ggplot(aes(x = campaign, y = clicks, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 1 row of missing values were removed automatically.

# Extract the second set of 50 unique campaigns, clicks and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(51:101) %>%  
  pull(campaign)

f51_101_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f51_101_creative %>%
  ggplot(aes(x = campaign, y = clicks, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 3421 rows with missing values were automatically removed.

# Extract the third set of 50 unique campaigns, clicks and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(102:151) %>%  
  pull(campaign)

f102_151_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f102_151_creative %>%
  ggplot(aes(x = campaign, y = clicks, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 7231 rows with missing values were found in this section and automatically 
# removed.

# Extract the fourth set of 50 unique campaigns, clicks and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(152:203) %>%  
  pull(campaign)

f152_203_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f152_203_creative %>%
  ggplot(aes(x = campaign, y = clicks, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 1223 rows with missing values were removed.

# Remaining 2 campaigns.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(204:205) %>%  
  pull(campaign)

f204_205_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f204_205_creative %>%
  ggplot(aes(x = campaign, y = clicks, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 2 rows removed due to missing data.



# Visualising campaign, follows and audience.  First set.
filtered_x <- creative1 %>%
  distinct(campaign) %>%
  head(50) %>%
  pull(campaign)

f50_creative <- creative1 %>%
  filter(campaign %in% filtered_x)

f50_creative %>%
  ggplot(aes(x = campaign, y = follows, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 60023 rows of missing values were removed automatically.

# Extract the second set of 50 unique campaigns, follows and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(51:101) %>%  
  pull(campaign)

f51_101_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f51_101_creative %>%
  ggplot(aes(x = campaign, y = follows, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 24659 rows with missing values were automatically removed.

# Extract the third set of 50 unique campaigns, follows and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(102:151) %>%  
  pull(campaign)

f102_151_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f102_151_creative %>%
  ggplot(aes(x = campaign, y = follows, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 7231 rows with missing values were found in this section and automatically 
# removed.

# Extract the fourth set of 50 unique campaigns, follows and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(152:203) %>%  
  pull(campaign)

f152_203_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f152_203_creative %>%
  ggplot(aes(x = campaign, y = follows, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 1559 rows with missing values were removed.

# Remaining 2 campaigns.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(204:205) %>%  
  pull(campaign)

f204_205_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f204_205_creative %>%
  ggplot(aes(x = campaign, y = follows, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 2 rows removed due to missing data.




# Visualising campaign, CTR and audience.  First set.
filtered_x <- creative1 %>%
  distinct(campaign) %>%
  head(50) %>%
  pull(campaign)

f50_creative <- creative1 %>%
  filter(campaign %in% filtered_x)

f50_creative %>%
  ggplot(aes(x = campaign, y = CTR, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 7 rows of missing values were removed automatically.

# Extract the second set of 50 unique campaigns, CTR and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(51:101) %>%  
  pull(campaign)

f51_101_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f51_101_creative %>%
  ggplot(aes(x = campaign, y = CTR, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 4337 rows with missing values were automatically removed.

# Extract the third set of 50 unique campaigns, CTR and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(102:151) %>%  
  pull(campaign)

f102_151_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f102_151_creative %>%
  ggplot(aes(x = campaign, y = CTR, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 7231 rows with missing values were found in this section and automatically 
# removed.

# Extract the fourth set of 50 unique campaigns, CTR and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(152:203) %>%  
  pull(campaign)

f152_203_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f152_203_creative %>%
  ggplot(aes(x = campaign, y = CTR, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 1223 rows with missing values were removed.

# Remaining 2 campaigns.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(204:205) %>%  
  pull(campaign)

f204_205_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f204_205_creative %>%
  ggplot(aes(x = campaign, y = CTR, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 2 rows removed due to missing data.




# Visualising campaign, CTR_score and audience.  First set.
filtered_x <- creative1 %>%
  distinct(campaign) %>%
  head(50) %>%
  pull(campaign)

f50_creative <- creative1 %>%
  filter(campaign %in% filtered_x)

f50_creative %>%
  ggplot(aes(x = campaign, y = CTR_score, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 6561 rows of missing values were removed automatically.

# Extract the second set of 50 unique campaigns, CTR_score and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(51:101) %>%  
  pull(campaign)

f51_101_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f51_101_creative %>%
  ggplot(aes(x = campaign, y = CTR_score, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 11844 rows with missing values were automatically removed.

# Extract the third set of 50 unique campaigns, CTR_score and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(102:151) %>%  
  pull(campaign)

f102_151_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f102_151_creative %>%
  ggplot(aes(x = campaign, y = CTR_score, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 7231 rows with missing values were found in this section and automatically 
# removed.

# Extract the fourth set of 50 unique campaigns, CTR_score and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(152:203) %>%  
  pull(campaign)

f152_203_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f152_203_creative %>%
  ggplot(aes(x = campaign, y = CTR_score, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 1223 rows with missing values were removed.

# Remaining 2 campaigns.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(204:205) %>%  
  pull(campaign)

f204_205_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f204_205_creative %>%
  ggplot(aes(x = campaign, y = CTR_score, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 2 rows removed due to missing data.



# Visualising campaign, wighted_CTR_score and audience.  First set.
filtered_x <- creative1 %>%
  distinct(campaign) %>%
  head(50) %>%
  pull(campaign)

f50_creative <- creative1 %>%
  filter(campaign %in% filtered_x)

f50_creative %>%
  ggplot(aes(x = campaign, y = weighted_CTR_score, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 6561 rows of missing values were removed automatically.

# Extract the second set of 50 unique campaigns, weighted_CTR_score and 
# audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(51:101) %>%  
  pull(campaign)

f51_101_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f51_101_creative %>%
  ggplot(aes(x = campaign, y = weighted_CTR_score, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 11844 rows with missing values were automatically removed.

# Extract the third set of 50 unique campaigns, weighted_CTR_score and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(102:151) %>%  
  pull(campaign)

f102_151_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f102_151_creative %>%
  ggplot(aes(x = campaign, y = weighted_CTR_score, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 7231 rows with missing values were found in this section and automatically 
# removed.

# Extract the fourth set of 50 unique campaigns, weighted_CTR_score and 
# audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(152:203) %>%  
  pull(campaign)

f152_203_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f152_203_creative %>%
  ggplot(aes(x = campaign, y = weighted_CTR_score, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 1223 rows with missing values were removed.

# Remaining 2 campaigns.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(204:205) %>%  
  pull(campaign)

f204_205_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f204_205_creative %>%
  ggplot(aes(x = campaign, y = weighted_CTR_score, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 2 rows removed due to missing data.



# Visualising campaign, impressions and audience.  First set.
filtered_x <- creative1 %>%
  distinct(campaign) %>%
  head(50) %>%
  pull(campaign)

f50_creative <- creative1 %>%
  filter(campaign %in% filtered_x)

f50_creative %>%
  ggplot(aes(x = campaign, y = impressions, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 1 row of missing values were removed automatically.

# Extract the second set of 50 unique campaigns, impressions and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(51:101) %>%  
  pull(campaign)

f51_101_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f51_101_creative %>%
  ggplot(aes(x = campaign, y = impressions, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# No rows with missing values were automatically removed.

# Extract the third set of 50 unique campaigns, impressions and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(102:151) %>%  
  pull(campaign)

f102_151_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f102_151_creative %>%
  ggplot(aes(x = campaign, y = impressions, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 0 rows with missing values were found in this section and automatically 
# removed.

# Extract the fourth set of 50 unique campaigns, impressions and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(152:203) %>%  
  pull(campaign)

f152_203_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f152_203_creative %>%
  ggplot(aes(x = campaign, y = impressions, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 0 rows with missing values were removed.

# Remaining 2 campaigns.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(204:205) %>%  
  pull(campaign)

f204_205_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f204_205_creative %>%
  ggplot(aes(x = campaign, y = impressions, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 0 rows removed due to missing data.



# Visualising campaign, reach and audience.  First set.
filtered_x <- creative1 %>%
  distinct(campaign) %>%
  head(50) %>%
  pull(campaign)

f50_creative <- creative1 %>%
  filter(campaign %in% filtered_x)

f50_creative %>%
  ggplot(aes(x = campaign, y = reach, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 12574 row of missing values were removed automatically.

# Extract the second set of 50 unique campaigns, reach and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(51:101) %>%  
  pull(campaign)

f51_101_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f51_101_creative %>%
  ggplot(aes(x = campaign, y = reach, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 8472 rows with missing values were automatically removed.

# Extract the third set of 50 unique campaigns, reach and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(102:151) %>%  
  pull(campaign)

f102_151_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f102_151_creative %>%
  ggplot(aes(x = campaign, y = reach, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 0 rows with missing values were found in this section and automatically 
# removed.

# Extract the fourth set of 50 unique campaigns, reach and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(152:203) %>%  
  pull(campaign)

f152_203_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f152_203_creative %>%
  ggplot(aes(x = campaign, y = reach, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 5 rows with missing values were removed.

# Remaining 2 campaigns.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(204:205) %>%  
  pull(campaign)

f204_205_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f204_205_creative %>%
  ggplot(aes(x = campaign, y = reach, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 0 rows removed due to missing data.



# Campaign by audience group.
# Visualising campaign and audience.  First set.
filtered_x <- creative1 %>%
  distinct(campaign) %>%
  head(50) %>%
  pull(campaign)

f50_creative <- creative1 %>%
  filter(campaign %in% filtered_x)

f50_creative %>%
  ggplot(aes(x = campaign, fill = audience)) +
  geom_bar(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 0 row of missing values were removed automatically.

# Extract the second set of 50 unique campaigns and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(51:101) %>%  
  pull(campaign)

f51_101_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f51_101_creative %>%
  ggplot(aes(x = campaign, fill = audience)) +
  geom_bar(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 0 rows with missing values were automatically removed.

# Extract the third set of 50 unique campaigns and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(102:151) %>%  
  pull(campaign)

f102_151_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f102_151_creative %>%
  ggplot(aes(x = campaign, fill = audience)) +
  geom_bar(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 0 rows with missing values were found in this section and automatically 
# removed.

# Extract the fourth set of 50 unique campaigns and audience.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(152:203) %>%  
  pull(campaign)

f152_203_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f152_203_creative %>%
  ggplot(aes(x = campaign, fill = audience)) +
  geom_bar(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 0 rows with missing values were removed.

# Remaining 2 campaigns.
filtered_y <- creative1 %>%
  distinct(campaign) %>%
  slice(204:205) %>%  
  pull(campaign)

f204_205_creative <- creative1 %>%
  filter(campaign %in% filtered_y)

f204_205_creative %>%
  ggplot(aes(x = campaign, fill = audience)) +
  geom_bar(alpha = 0.5) +
  theme_bw() +
  coord_flip()
# 0 rows removed due to missing data.


########################## Exploring date variable further.
# Check variable names.
names(creative1)
unique(creative1$date)
length(unique(creative1$date))
# There are 221 dates per advertising campaign.
# See the frequency of dates applied to advertising.
table(creative1$date, useNA = 'always')
# See first and last dates.
# Frequency bar chart of the dates, to also view campaign would need filtering, 
# but by audience:
barplot(table(creative1$date, useNA = 'always'), decreasing = TRUE)
creative1 %>%
  drop_na(date) %>%
  ggplot(aes(date, fill = audience))+
  geom_bar(alpha = 0.5)+
  theme_bw()+
  coord_flip()
# There is only one NA value in the date variable.  The dates of advertising run
# from 26th March 2022 until the 31st October of the same year.  The initial 
# campaign focus seems to be audience 6 primarily, although they are targeted 
# throughout.  There seems to be a spike in targeting audience 4 in October, at 
# the end of the advertising period. 

creative1 %>%
  drop_na(date) %>%
  filter(date == "2022-03-26") %>%
  ggplot(aes(date, fill = campaign)) +
  geom_bar() +
  theme_bw()
# Just a huge number of campaigns on even one date.  Unclear if date refers to 
# date campaign released to public or just data collected, or whether this even
# matters.  When cross referencing campaign names with other databases, it 
# appears that actually many of these so-called campaign names are likely just 
# where teh campaigns were advertised.
# Check for distinct 'campaigns' again:
unique(creative1$campaign)
# If you remove the programmes and the stations where the campaigns are 
# advertised, it may be that there are approximately 83 campaigns here, this is 
# still much more than in other datasets.
creative3 <- creative1
# This creates a creative1 copy as creative3 which we can correct in terms of 
# campaign names.  All analysis with campaigns so far will need repeating with 
# this new dataframe creative3.
  
# Replace multiple values with a single value using ifelse
creative3$campaign <- ifelse(creative3$campaign %in% 
                                        c("NBC", "TLC", "History Channel",
                                          "Comedy Central", "FuboTV", "LOGO", 
                                          "MTV", "Disney Channel", "Nat Geo TV",
                                          "Newsy", "Food Network", "ESPN", 
                                          "ABC News", "Xumo", "FOX Sports", 
                                          "NFL", "ABC", "Cartoon Network", 
                                          "NBA", "The CW", "Yahoo Sports", "FX",
                                          "Motor Trend", "MSN News", "A&E", 
                                          "Discovery Velocity", "Pluto", "HGTV",
                                          "NCAA", "VH1", "CNBC", "MSNBC", 
                                          "NBC Sports", "Bloomberg", "DIRECTV", 
                                          "FYI", "The Weather Channel", "Philo",
                                          "We TV", "Travel Channel", "PBS", 
                                          "Lifetime Movie Club", "OWN tv", 
                                          "Cooking Channel", "CBS News", 
                                          "USA Today", "CBS", "CNN", 
                                          "CBS Sports", "Syfy", "Crackle", 
                                          "The CW Seed", "MLB", 
                                          "Paramount Network", "USA Network", 
                                          "Tubi", "Discovery", "Bravo", 
                                          "Hallmark Channel", "TNT", "FOX News",
                                          "Ovation TV", "Lifetime", "Encore", 
                                          "Sundance TV", "DIY Network", 
                                          "Discovery Destination America", "AMC",
                                          "Sling", "PGA Tour", "NHL", "IFC", 
                                          "BET", "CMT", "Discovery TLC", 
                                          "FOX Business", "truTV", "TBS", "TMZ",
                                          "Discovery AHC", "Discovery ID", 
                                          "Science Channel", "NBC News", "Spike",
                                          "FOX", "Discovery Life", "AXS TV", 
                                          "Comet TV", 
                                          "Hallmark Movies & Mysteries", 
                                          "Animal Planet", "Boomerang", "MTV2", 
                                          "BBC", "BBC America", "Oxygen", 
                                          "TV Land", "Game Show Network", 
                                          "NBC Golf", "Fuse", "HLN", "ESPN 2", 
                                          "VICELAND", "Outside TV", 
                                          "A&E History Vault", "AccuWeather", 
                                          "TSN Sports", "E!", "VICE", 
                                          "Hallmark Drama", 
                                          "Lifetime Movie Network", 
                                          "FOX Sports 2", "Tennis Channel", 
                                          "Freeform - ABC Family", 
                                          "FOX Sports 1", "Outdoor Channel", 
                                          "Nat Geo Wild", "FXX", 
                                          "Sony Movie Channel", "History AHC", 
                                          "Nickelodeon", "NBC Today"), 
                                      "wrongcampg", 
                                      creative3$campaign)  
unique(creative3$campaign)  
length(unique(creative3$campaign))
# 85 unique campaigns listed in the campaign variable.  Seems to include 
# version and platform information too. When you compare to Google data for 
# campaigns which shows 8 campaigns, there is still too much difference and not 
# enough hints in the 'campaigns' included to correct them.  
# Recheck other unique variables:
unique(creative1$ad_format)
# "No Lock Campaign" option is incorrect and will need to be tweaked out.  
# Otherwise ok.
table(creative1$ad_format)
# There are 28 values in ad_format that appear as 'No lock campaign' which is 
# probably wrong.I will change these to blank values.
class(creative1$ad_format)
# This is character data type. Change 'No lock campaign' to a blank value.
creative1$ad_format <- str_replace(creative1$ad_format, "No lock campaign", 
                                   "")
table(creative1$ad_format)
# Export updated data set as csv.  It shouldn't have made a huge difference to 
# recent analysis but it can be repeated (only 28 values in error and they have 
# been made blank).
#write_csv(creative1, "creative1.csv")
unique(creative1$creative_size)
# Now I can see other datasets, looks like about half this variable's unique 
# values are not related to size.  If we can figure out if they mean something, 
# they could be added into the correct column, but it's not obvious.  
# We need to decide whether it matters or not first.
unique(creative1$audience)
# This looks fine.
unique(creative1$platform)
# Looks ok, I have added what OTT means in the reference notes.
unique(creative1$creative_family)
# Doesn't look completely right, will need to request what should be the unique 
# entries for each of these variables and also we can compare with other 
# datasets.
unique(creative1$creative_version)
# This may be better than others.  Compare to other datasets.
names(creative1)




