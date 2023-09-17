#Upload necessary libraries.
library(tidyverse)
library(stringr)
library(plotly)
library(ggplot2)
# install.packages("vcd")
library(vcd)
install.packages(car)
library(car)
library(moments)
library(RColorBrewer)

dem_final <- read_csv("dem_final.csv")
creative_final <- read_csv("creative_final.csv")
glimpse(dem_final)
glimpse(creative_final)
dem_final$audience <- as.character(dem_final$audience)
creative_final$audience <- as.character(creative_final$audience)
min(dem_final$date)
max(dem_final$date)

# Count unique number of companies.
length(unique(dem_final$company))
# there are 12,455 companies.

# Unique campaigns:
unique(dem_final$creative_family)

# Breakdown of companies by audience type.
desired_creative_family_values <- c("UnfairAdvantage", 
                                    "CompetitiveOpportunity", "CloserTwins", 
                                    "CloseFaster")

company_audience <- dem_final %>%
  filter(creative_family %in% desired_creative_family_values) %>%
  drop_na(audience, creative_family) %>%
  group_by(audience, creative_family) %>%
  summarize(total_companies = n_distinct(company))

companies_by_aud <- ggplot(company_audience,
                           aes(x = audience, y = total_companies, 
                               fill = creative_family)) +
  geom_col() +
  theme_bw() +
  labs(title = "Number of unique companies by audience and campaign targetting",
       x = "Audience",
       y = "Number of companies by audience group") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral")

companies_by_aud
# This shows that there always were many more companies recorded under 
# audience groups 4 and 5 (6 were presumably found online, as unregistered?) and
# 4 are registered brokerages with unregistered loan officers. 

# Compare this result with the creative_final dataset for reach for before and 
# after sense fo audience volume. I have excluded SEM campaign.

reach_aud_camp <- creative_final %>%
  drop_na(creative_family, reach, audience) %>%
  filter(creative_family %in% c("UnfairAdvantage", 
                                "CompetitiveOpportunity", "CloserTwins", 
                                "CloseFaster")) %>%
  group_by(audience, creative_family) %>%
  summarize(total_reach = sum(reach)) %>%
  ggplot(aes(x = audience, y = total_reach, fill = creative_family)) +
  geom_col() +
  theme_bw() +
  labs(title = "Total Reach by Audience and Campaign Proxy (excl SEM)",
       x = "Audience",
       y = "Total Reach") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral")

reach_aud_camp

# Check what unique values appear under campaign in this dataset.
unique(dem_final$campaign)
# This is no use.

# Check how many unique dates:
length(unique(dem_final$date))
# there are 137 unique dates.

# Which platforms do audiences prefer?

clicks_platf_aud <- dem_final %>%
  drop_na(platform, clicks, audience) %>%
  group_by(platform, audience) %>%
  summarize(total_clicks = sum(clicks)) %>%
  ggplot(aes(x = platform, y = total_clicks, fill = audience)) +
  geom_col() +
  theme_bw() +
  labs(title = "Platform interest by clicks and audience (company)",
       x = "Platform",
       y = "Total clicks by platform") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral")

clicks_platf_aud
# Please note - THIS DATA ABOVE IS DIFFERENT FROM OTHER DATA SETS - likely 
# because it's for registered companies.

# Explore creative_version and ad_format:

unique(dem_final$creative_version)
unique(dem_final$ad_format)

# Which ad_formats do audiences prefer, using clicks:

clicks_format_aud <- dem_final %>%
  drop_na(ad_format, clicks, audience) %>%
  group_by(ad_format, audience) %>%
  summarize(total_clicks = sum(clicks)) %>%
  ggplot(aes(x = ad_format, y = total_clicks, fill = audience)) +
  geom_col() +
  theme_bw() +
  labs(title = "Ad format interest by clicks & audience (Co info)",
       x = "Ad format",
       y = "Total clicks by ad format") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral")

clicks_format_aud

# Reflect on potential of creative_size:

unique(dem_final$creative_size)
# Doesn't look very helpful.