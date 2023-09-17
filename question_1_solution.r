# This is the final R analysis used for question 1 of business questions.

# Exploring and basic cleaning of the raw creative data set.
# Generated on R version 4.3.1 (2023) and RStudio (2023.06.2).

#Upload necessary libraries.
library(tidyverse)
library(stringr)
library(plotly)
library(ggplot2)
# install.packages("vcd")
library(vcd)
# install.packages(car)
library(car)
library(moments)
library(RColorBrewer)
# install.packages("treemapify")
library(treemapify)

################################################# Import relevant data sets:
# Import first dataset creative_final.
creative_final <- read_csv("creative_final.csv")
creative_final$audience <- as.character(creative_final$audience)
class(creative_final$audience)
# Import merged doc for creative and ggoals.
creative_goal_merge_0209 <- read.csv("Creative_Goal_Merged_2ndSept.csv")
# Import the final datasets for final presentation (cost per completion, etc.).
spend_amalgamated <- read.csv("amalgamated_spend_campaign.csv")
# Import google goals dataset:
ggoals_final <- read_csv("ggoals_final.csv")
glimpse(ggoals_final)
# Correct data type for audience.
ggoals_final$audience <- as.character(ggoals_final$audience)
class(ggoals_final$audience)

########################################################### Useful context:
# Calculate total digital spend.
total_spend <- sum(creative_final$spend, na.rm = TRUE)
total_spend
# 725,179.7 USD (could be millions or 100s of thousands, not clear).

# Understanidng spend by audience group.
# Spend number too large to be easily understood, divide by 1000.
creative_final$spend <- creative_final$spend/1000

# Set ggplot in arranged spend by audience by descending.
summarized_data <- creative_final %>%
  drop_na(spend, audience) %>%
  group_by(audience) %>%
  summarise(total_spend = sum(spend)) %>%
  arrange(desc(total_spend))

summarized_data$audience <- factor(summarized_data$audience, 
                                   levels = summarized_data$audience)

ggplot(summarized_data, aes(audience, total_spend, fill = audience)) +
  geom_col(alpha = 0.5) +
  theme_bw() +
  labs(title = "Spend by Audience", x = "Audience", 
       y = "Total Spend in thousands")
# This is the spend by audience group in descending order.  

# Total spend by audience group in calculated figures:
# Total spend on audience 6.
total_spend6 <- creative_final %>%
  filter(audience == "6") %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
total_spend6
# Total spend USD$ 287,212.4 on audience 6 advertising.
# Total spend on audience 5.
total_spend5 <- creative_final %>%
  filter(audience == "5") %>%
  drop_na(spend, audience) %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
total_spend5
# Total spend on audience 5 is $142,107.1.
# Total spend on audience 4.
total_spend4 <- creative_final %>%
  filter(audience == "4") %>%
  drop_na(spend, audience) %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
total_spend4
# Total spend on audience 4 is $172,723.2.
# Total spend on audience 3.
total_spend3 <- creative_final %>%
  filter(audience == "3") %>%
  drop_na(spend, audience) %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
total_spend3
# Total spend on audience 3 is $30,576.88.
# Total spend on audience 2.
total_spend2 <- creative_final %>%
  filter(audience == "2") %>%
  drop_na(spend, audience) %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
total_spend2
# Total spend on audience 2 is $17,408.54.
# Total spend on audience 1.
total_spend1 <- creative_final %>%
  filter(audience == "1") %>%
  drop_na(spend, audience) %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
total_spend1
# Total spend on audience 1 is $45,754.4.

# Visualisation: check platform by completions and audience.
# Total completions.
plat_comp_audience <- ggoals_final %>%
  drop_na(completions, audience, platform) %>%
  ggplot(aes(platform, completions, fill = audience))+
  geom_col()+
  theme_bw()+
  coord_flip()+
  labs(title = "Completions by platform and customer group",
       x = "Advert platform",
       y = "Completions")
plat_comp_audience

# Visualisation: average completions for advert platform.
avplat_comp_audience <- ggoals_final %>%
  drop_na(completions, audience, platform) %>%
  filter(campaign_traffic == "Campaign") %>%
  group_by(platform, audience) %>%
  summarise(avg_completions = mean(completions)) %>%
  ggplot(aes(platform, avg_completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average Completions by Advert Platform and Customer Group",
       x = "Advert platform",
       y = "Average Completions (Campaign)")
avplat_comp_audience

# Visualisation: average completions for general traffic data.
avplat_comp_audience2 <- ggoals_final %>%
  drop_na(completions, audience, platform) %>%
  filter(campaign_traffic == "General traffic") %>%
  group_by(platform, audience) %>%
  summarise(avg_completions = mean(completions)) %>%
  ggplot(aes(platform, avg_completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average Completions by Advert Platform and Customer Group",
       x = "Advert platform",
       y = "Average Completions (General traffic")
avplat_comp_audience2
# All non campaign data is for audience 6 and it is through Trade Media 
# platform.  

# Visualisation: # Comparing creative_family across two datasets, ggoals and 
# creative.  
# Creative_final: average CTR by campaign proxy and audience.
CTR_creativefam_audience <- creative_final %>%
  drop_na(CTR, audience, creative_family) %>%
  group_by(creative_family, audience) %>%
  summarise(average_CTR = mean(CTR)) %>%
  ggplot(aes(creative_family, average_CTR, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average CTR by advert family and audience",
       x = "Advert family",
       y = "Average CTR")
CTR_creativefam_audience
# Ggoals_final: average completions by campaign proxy and audience.
completions_creativefam_audience <- ggoals_final %>%
  filter(campaign_traffic == "Campaign") %>%
  drop_na(completions, audience, creative_family) %>%
  group_by(creative_family, audience) %>%
  summarise(average_completions = mean(completions)) %>%
  ggplot(aes(creative_family, average_completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average completions by advert family and audience",
       x = "Advert family",
       y = "Average completions (campaign)")
completions_creativefam_audience

# Visualisation: Campaign completions by month, campaign proxy is creative 
# family. Looking for any trends.
family_months_completion <- ggoals_final %>%
  drop_na(completions, creative_family, date) %>%
  group_by(creative_family) %>%
  ggplot(aes(date, completions, fill = creative_family)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Completions over time by campaign proxy",
       x = "Date",
       y = "Completions")
family_months_completion
# Looks like maybe a peak beginning and end, similar size for UnfairAdvantage.
# SEM ads always offer the highest completions and we know these are from 
# audience 6.

# Visualisation: Check completions by campaign proxy and audience.
family_audience_completion <- ggoals_final %>%
  drop_na(completions, creative_family, audience) %>%
  group_by(creative_family, audience) %>%
  ggplot(aes(creative_family, completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Completions by campaign proxy and audience",
       x = "Campaign proxy",
       y = "Completions")
family_audience_completion
# Results obscured by how many completions by audience 6 on SEM Ads.

# Visualisation: Check completions by campaign proxy and audience except SEM ads
# so we can see better.
family_audience_completion_filteroutSEM <- ggoals_final %>%
  drop_na(completions, creative_family, audience) %>%
  filter(creative_family != "SEM Ads") %>%
  group_by(creative_family, audience) %>%
  ggplot(aes(creative_family, completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Completions by campaign proxy and audience (excl SEM)",
       x = "Campaign proxy",
       y = "Completions")
family_audience_completion_filteroutSEM
# Best campaign proxies for audiences to be seen here (note SEM Ads all aud 6).
# This shows which campaigns were run by audience group as well as the success 
# by completions - which campaigns worked best for which audiences.

# Visualisation: Check completions by platforms per audience.
platform_audience_completion <- ggoals_final %>%
  drop_na(completions, platform, audience) %>%
  group_by(platform, audience) %>%
  ggplot(aes(platform, completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Completions by platform and audience",
       x = "Platform",
       y = "Completions")
platform_audience_completion 
# Next we will exclude Google SEM as it is obscuring the results.

# Visualisation: Excluding Google SEM but looking at completions by platform and 
# audience.
platform_audience_completion_exclSEM <- ggoals_final %>%
  drop_na(completions, platform, audience) %>%
  filter(platform != "Google SEM") %>%
  group_by(platform, audience) %>%
  ggplot(aes(platform, completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Completions by platform and audience (excl SEM)",
       x = "Platform",
       y = "Completions")
platform_audience_completion_exclSEM 

#Visualisation:  Impressions over time by campaign proxy.
imp_camp_time <- creative_final %>%
  drop_na(impressions, date, creative_family) %>%
  filter(creative_family %in% c("CloseFaster", "CloserTwins", 
                                "CompetitiveOpportunity", 
                                "UnfairAdvantage")) %>%
  group_by(creative_family, date) %>%
  summarise(total_impressions = sum(impressions)) %>%
  ggplot(aes(date, total_impressions, group = creative_family)) + 
  geom_bar(stat = "identity") +  
  theme_bw() +
  labs(title = "Impressions over time by campaign proxy",
       x = "Date",
       y = "Total impressions") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~creative_family)
imp_camp_time
# This does look reliable when considering the weight of the data on both sides.
# It shows that there was a trend towards increasing impressions over time for 
# all campaign proxies (creative_family) except CloserTwins.

# Impressions over time by audience (cross reference with above visualisation 
# for reliability).
imp_audience_time <- creative_final %>%
  drop_na(impressions, date, audience) %>%
  group_by(audience, date) %>%
  summarise(total_impressions = sum(impressions)) %>%
  ggplot(aes(date, total_impressions, group = audience)) + 
  geom_bar(stat = "identity") +  
  theme_bw() +
  labs(title = "Impressions over time by audience",
       x = "Date",
       y = "Total impressions") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~audience)
imp_audience_time
# This is an interesting cross reference to visualisation directly above. It 
# suggests that if all audience trend lines are increase over time (except 6) 
# then, previous trend line may be unreliable.

# Visualisation: Try to see trend line for reach over time, by audience and by campaign 
# proxy.
reach_camp_time <- creative_final %>%
  drop_na(reach, date, creative_family) %>%
  filter(creative_family %in% c("CloseFaster", "CloserTwins", 
                                "CompetitiveOpportunity", 
                                "UnfairAdvantage")) %>%
  group_by(creative_family, date) %>%
  summarise(total_reach = sum(reach)) %>%
  ggplot(aes(date, total_reach, group = creative_family)) + 
  geom_bar(stat = "identity") +  
  theme_bw() +
  labs(title = "Reach over time by campaign proxy",
       x = "Date",
       y = "Total reach") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~creative_family)
reach_camp_time
# CloseFaster reach increases over time and so does UnfairAdvantage.  
# CloserTwins reduces over time.  Care must be taken with these trend lines - 
# see and cross reference with below. 

# Reach by audience over time.
reach_aud_time <- creative_final %>%
  drop_na(reach, date, audience) %>%
  group_by(audience, date) %>%
  summarise(total_reach = sum(reach)) %>%
  ggplot(aes(date, total_reach, group = audience)) + 
  geom_bar(stat = "identity") +  
  theme_bw() +
  labs(title = "Reach over time by audience",
       x = "Date",
       y = "Total reach") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~audience)
reach_aud_time
# Increased for audience 1, 3, 4 and reduced for 6 and maybe 5. As a 
# cross-reference opportunity, this may not correspond with visualisation above.
# Visualisation: Remove audience 6 to see other audience reach better over time.
reach_aud_time2 <- creative_final %>%
  drop_na(reach, date, audience) %>%
  filter(audience %in% c("1", "2", "3", "4", "5")) %>%
  group_by(audience, date) %>%
  summarise(total_reach = sum(reach)) %>%
  ggplot(aes(date, total_reach, group = audience)) + 
  geom_bar(stat = "identity") +  
  theme_bw() +
  labs(title = "Reach over time by audience",
       x = "Date",
       y = "Total reach") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~audience)
reach_aud_time2
# Increases for everyone except audiences 5 and 6. This may counter previous 
# trend lines by campaign proxy.

# Visuaalisations: Check CTR over time as a proxy for emission of campaign 
#proxy.
avCTR_camp_time <- creative_final %>%
  drop_na(CTR, date, creative_family) %>%
  filter(creative_family %in% c("CloseFaster", "CloserTwins", 
                                "CompetitiveOpportunity", 
                                "UnfairAdvantage")) %>%
  group_by(creative_family, date) %>%
  summarise(av_CTR = mean(CTR)) %>%
  ggplot(aes(date, av_CTR, group = creative_family)) + 
  geom_bar(stat = "identity") +  
  theme_bw() +
  labs(title = "Av CTR (proxy for camgn emission),over time",
       x = "Date",
       y = "AvCTR") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~creative_family)
avCTR_camp_time
# Av CTR in CloseFaster and CloserTwins increase over time,suggests advertising 
# running throughout the period, except for CompetitiveOpportunity. 
# This would need to be checked using periods aggregations.This had originally 
# been thought to be reliable, but no longer.  This could be useful to analyse
# further to confirm if there is a trend by period aggregations.

#################### Using most reliable aggregation across ggoals and creative.
glimpse(spend_amalgamated)
spend_amalgamated$audience <- as.character(spend_amalgamated$audience)
spend_amalgamated$tot_spend_byaud_group <- 
  as.numeric(spend_amalgamated$tot_spend_byaud_group)
# treemaps are in python and due to their poor visualisation, they were then 
# drawn in using that visusalisation.

# This is the most reliable - spend by completion.
ggplot(spend_amalgamated, aes(x = audience)) +
  geom_line(aes(y = ct_spend_per_completion, 
                color = "Closer Twins"), group = 1) +
  geom_line(aes(y = cf_spend_per_completion, 
                color = "Close Faster"), group = 1) +
  geom_line(aes(y = ua_spend_per_completion, 
                color = "Unfair Advantage"), group = 1) +
  geom_line(aes(y = co_spend_per_completion, 
                color = "Competitive Opportunity"), group = 1) +
  geom_line(aes(y = sem_spend_per_completion, color = "SEM Ads"), group = 1) +
  labs(title = "Spend per completion by campaign proxy (creative family)", 
       x = "audience", y = "Campaign proxy cost ($) of each completion") +
  theme_minimal() +
  scale_color_manual(values = c("Closer Twins" = "blue",
                                "Close Faster" = "red",
                                "Unfair Advantage" = "orange",
                                "Competitive Opportunity" = "green",
                                "SEM Ads" = "purple"))
# Line chart was used because stacked and grouped versions were thought to be 
# more difficult to compare, however, this is not the correct way and will be 
# converted into bar charts. The colours had to be specified for reliability and 
# consistency across charts and also to match with the python visualisations in 
# the presentation.

# Below a better grouped bar chart way of showing the same information:
# Make sure audience is string or category or character:
spend_amalgamated$audience <- as.character(spend_amalgamated$audience)
unique(spend_by_audience_long$audience)
class(spend_by_audience_long$audience)

spend_by_aud_completion <- spend_amalgamated %>%
  select(audience, ct_spend_per_completion, cf_spend_per_completion, 
         ua_spend_per_completion, co_spend_per_completion, 
         sem_spend_per_completion)

spend_by_aud_completion

spend_by_aud_completions_long <- spend_by_aud_completion %>%
  pivot_longer(cols = 2:6, 
               names_to = "campaign", values_to = "spend_by_completion")


spend_by_aud_completions_long

ggplot(spend_by_aud_completions_long, aes(x = audience, y = spend_by_completion, 
                                   fill = campaign)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Audience", y = "Spend") +
  ggtitle("Spend by completion by campaign and audience")
# This is a better representation than the line chart above and shows all the 
# data including SEM and audience 6.  This is what should have been used in the 
# presentation.  

# Deep dive into completions and spend to compare options and predict best 
# choices.
# Completions by campaign and audience.
ggplot(spend_amalgamated, aes(x = audience)) +
  geom_line(aes(y = ct_tot_completions_byaud_group, 
                color = "Closer Twins"), group = 1) +
  geom_line(aes(y = cf_tot_completions_byaud_group, 
                color = "Close Faster"), group = 1) +
  geom_line(aes(y = ua_tot_completions_byaud_group, 
                color = "Unfair Advantage"), group = 1) +
  geom_line(aes(y = co_tot_completions_byaud_group, 
                color = "Competitive Opportunity"), group = 1) +
  labs(title = 
         "Total completions by campaign (creative family) & audience (exc sem)", 
       x = "audience", y = "Campaign proxy total completions") +
  theme_minimal() +
  scale_color_manual(values = c("Closer Twins" = "blue",
                                "Close Faster" = "red",
                                "Unfair Advantage" = "orange",
                                "Competitive Opportunity" = "green"))
# Line chart was used because stacked and grouped versions were thought to be 
# more difficult to compare, however, this is not the correct way and will be 
# converted into bar charts. The colours had to be specified for reliability and 
# consistency across charts and also to match with the python visualisations in 
# the presentation.

# Spend by audience group.
ggplot(spend_amalgamated, aes(x = audience)) +
  geom_line(aes(y = ct_spend_byaud_group, 
                color = "Closer Twins"), group = 1) +
  geom_line(aes(y = cf_spend_byaud_group, 
                color = "Close Faster"), group = 1) +
  geom_line(aes(y = ua_spend_byaud_group, 
                color = "Unfair Advantage"), group = 1) +
  geom_line(aes(y = co_spend_byaud_group, 
                color = "Competitive Opportunity"), group = 1) +
  geom_line(aes(y = sem_spend_byaud_group, color = "SEM Ads"), group = 1)+
  labs(title = 
         "Spend by campaign (creative family) & audience", 
       x = "audience", y = "Spend in USD") +
  theme_minimal() +
  scale_color_manual(values = c("Closer Twins" = "blue",
                                "Close Faster" = "red",
                                "Unfair Advantage" = "orange",
                                "Competitive Opportunity" = "green",
                                "SEM Ads" = "purple"))
# Line chart was used because stacked and grouped versions were thought to be 
# more difficult to compare, however, this is not the correct way and will be 
# converted into bar charts. The colours had to be specified for reliability and 
# consistency across charts and also to match with the python visualisations in 
# the presentation.

# The better grouped bar chart version of above is (use pivot_longer):
spend_amalgamated$audience <- as.character(spend_amalgamated$audience)
unique(spend_by_audience_long$audience)
class(spend_by_audience_long$audience)

spend_by_audience <- spend_amalgamated %>%
  select(audience, ct_spend_byaud_group, cf_spend_byaud_group, 
         ua_spend_byaud_group, co_spend_byaud_group, sem_spend_byaud_group)
spend_by_audience

spend_by_audience_long <- spend_by_audience %>%
  pivot_longer(cols = 2:6, 
               names_to = "campaign", values_to = "spend_by_audience")

spend_by_audience_long

ggplot(spend_by_audience_long, aes(x = audience, y = spend_by_audience, 
                                   fill = campaign)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Audience", y = "Spend") +
  ggtitle("Spend by campaign and audience")
# If this were used for a presentation, the original dataframe 
# spend_by_audience_long campaign proxy names would be relabelled.


# Looking at best fit campaigns by audience using avCTR score.
ggplot(spend_amalgamated, aes(x = audience)) +
  geom_line(aes(y = ct_avctrscore, 
                color = "Closer Twins"), group = 1) +
  geom_line(aes(y = cf_avctrscore, 
                color = "Close Faster"), group = 1) +
  geom_line(aes(y = ua_avctrscore, 
                color = "Unfair Advantage"), group = 1) +
  geom_line(aes(y = co_avctrscore, 
                color = "Competitive Opportunity"), group = 1) +
  geom_line(aes(y = sem_avctrscore, color = "SEM Ads"), group = 1)+
  labs(title = 
         "Av CTR score by campaign (creative family) & audience", 
       x = "audience", y = "Av CTR score") +
  theme_minimal() +
  scale_color_manual(values = c("Closer Twins" = "blue",
                                "Close Faster" = "red",
                                "Unfair Advantage" = "orange",
                                "Competitive Opportunity" = "green",
                                "SEM Ads" = "purple"))
# Line chart was used because stacked and grouped versions were thought to be 
# more difficult to compare, however, this is not the correct way and will be 
# converted into bar charts. The colours had to be specified for reliability and 
# consistency across charts and also to match with the python visualisations in 
# the presentation.



# Cost per reach by campaign and audience.
ggplot(spend_amalgamated, aes(x = audience)) +
  geom_line(aes(y = ct_cost_per_reach, 
                color = "Closer Twins"), group = 1) +
  geom_line(aes(y = cf_cost_per_reach, 
                color = "Close Faster"), group = 1) +
  geom_line(aes(y = ua_cost_per_reach, 
                color = "Unfair Advantage"), group = 1) +
  geom_line(aes(y = co_cost_per_reach, 
                color = "Competitive Opportunity"), group = 1) +
  geom_line(aes(y = sem_cost_per_reach, color = "SEM Ads"), group = 1)+
  labs(title = 
         "Spend per reach by campaign (creative family) & audience", 
       x = "audience", y = "Spend per reach") +
  theme_minimal() +
  scale_color_manual(values = c("Closer Twins" = "blue",
                                "Close Faster" = "red",
                                "Unfair Advantage" = "orange",
                                "Competitive Opportunity" = "green",
                                "SEM Ads" = "purple"))
# Obviously this assumes with more spend there would be more reach.
# Line chart was used because stacked and grouped versions were thought to be 
# more difficult to compare, however, this is not the correct way and will be 
# converted into bar charts. The colours had to be specified for reliability and 
# consistency across charts and also to match with the python visualisations in 
# the presentation.

# Spend by click for each audience and campaign.
ggplot(spend_amalgamated, aes(x = audience)) +
  geom_line(aes(y = ct_spend_by_click, 
                color = "Closer Twins"), group = 1) +
  geom_line(aes(y = cf_spend_by_click, 
                color = "Close Faster"), group = 1) +
  geom_line(aes(y = ua_spend_by_click, 
                color = "Unfair Advantage"), group = 1) +
  geom_line(aes(y = co_spend_by_click, 
                color = "Competitive Opportunity"), group = 1) +
  geom_line(aes(y = sem_spend_by_click, color = "SEM Ads"), group = 1)+
  labs(title = 
         "Spend per click by campaign (creative family) & audience (excl aud6)", 
       x = "audience", y = "Spend per click") +
  theme_minimal() +
  scale_color_manual(values = c("Closer Twins" = "blue",
                                "Close Faster" = "red",
                                "Unfair Advantage" = "orange",
                                "Competitive Opportunity" = "green",
                                "SEM Ads" = "purple"))
# Exclude audience 6 below as very high at ~ 3500 USD per click for closer 
# twins.
# Line chart was used because stacked and grouped versions were thought to be 
# more difficult to compare, however, this is not the correct way and will be 
# converted into bar charts. The colours had to be specified for reliability and 
# consistency across charts and also to match with the python visualisations in 
# the presentation.

# Spend by click for each audience and campaign (excl closer twins aud 6).
filtered_data <- spend_amalgamated %>%
  filter(audience != "6")

ggplot(filtered_data, aes(x = audience))+
  geom_line(aes(y = ct_spend_by_click, 
                color = "Closer Twins"), group = 1) +
  geom_line(aes(y = cf_spend_by_click, 
                color = "Close Faster"), group = 1) +
  geom_line(aes(y = ua_spend_by_click, 
                color = "Unfair Advantage"), group = 1) +
  geom_line(aes(y = co_spend_by_click, 
                color = "Competitive Opportunity"), group = 1) +
  geom_line(aes(y = sem_spend_by_click, color = "SEM Ads"), group = 1)+
  labs(title = 
         "Spend per click by campaign (creative family) & audience (excl aud6)", 
       x = "audience", y = "Spend per click (USD)") +
  theme_minimal() +
  scale_color_manual(values = c("Closer Twins" = "blue",
                                "Close Faster" = "red",
                                "Unfair Advantage" = "orange",
                                "Competitive Opportunity" = "green",
                                "SEM Ads" = "purple"))
# Line chart was used because stacked and grouped versions were thought to be 
# more difficult to compare, however, this is not the correct way and will be 
# converted into bar charts. The colours had to be specified for reliability and 
# consistency across charts and also to match with the python visualisations in 
# the presentation.

# Below would have been the better way of presenting the same data.
spend_by_click <- spend_amalgamated %>%
  select(audience, ct_spend_by_click, cf_spend_by_click, 
         ua_spend_by_click, co_spend_by_click, sem_spend_by_click)
spend_by_click

spend_by_click_long <- spend_by_click %>%
  pivot_longer(cols = 2:6, 
               names_to = "campaign", values_to = "spend_by_click")

spend_by_click_long

ggplot(spend_by_audience_long, aes(x = audience, y = spend_by_audience, 
                                   fill = campaign)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Audience", y = "Spend") +
  ggtitle("Spend by click, campaign and audience")
# This would have been a better way of presenting the data at the final 
# presentation.