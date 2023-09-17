# This is the R working prep for question 1 of business questions.

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

# Import first dataset creative_final.
creative_final <- read_csv("creative_final.csv")
creative_final$audience <- as.character(creative_final$audience)
class(creative_final$audience)
#View(creative_harmony)
ggoals_final <- read_csv("ggoals_final.csv")
glimpse(ggoals_final)
ggoals_final$audience <- as.character(ggoals_final$audience)
class(ggoals_final$audience)
# View(ggoals_harmony)


# Fixing dates formats.
# post_merged1 <- read_csv("post_merged.csv")
# glimpse(post_merged1)
# post_merged1$date <- as.Date(post_merged1$date, 
#                             format = "%d/%m/%Y")
# post_merged1$audience <- as.character(post_merged1$audience)
# glimpse(post_merged1)
# write.csv(post_merged1, "post_merged1.csv")
# We have learnt you have to fix data types in the programme importing the csv.

# pre_goal_general1 <- read.csv("Pre_Goal_General.csv")
# glimpse(pre_goal_general1)
# pre_goal_general1$audience <- as.character(pre_goal_general1$audience)
# pre_goal_general1$date <- as.Date(pre_goal_general1$date, format = "%d/%m/%Y")
# glimpse(pre_goal_general1)
# write_csv(pre_goal_general1, "pre_goal_general1.csv")
# We have learnt you have to fix data types in the programme importing the csv.

# Import the google merge.
# goal_general_merge_final <- read_csv("Goal_General_Merge_Final.csv")
# names(goal_general_merge_final)
# glimpse(goal_general_merge_final)
# goal_general_merge_final$date <- as.Date(goal_general_merge_final$date, 
#                                         format = "%d/%m/%Y")
# We have learnt you have to fix data types in the programme importing the csv.

# glimpse(goal_general_merge_final)
# goal_general_merge_final$audience <- 
#   as.character(goal_general_merge_final$audience)
# glimpse(goal_general_merge_final)
# write_csv(goal_general_merge_final, "goal_general_merge_final2.csv")
# We have learnt you have to fix data types in the programme importing the csv.


# Import merged doc for creative and ggoals.
creative_goal_merge_0209 <- read.csv("Creative_Goal_Merged_2ndSept.csv")

# Import the final datasets for final presentation (cost per completion, etc.).
spend_amalgamated <- read.csv("amalgamated_spend_campaign.csv")

# Calculate total digital spend.
total_spend <- sum(creative_final$spend, na.rm = TRUE)
total_spend
# 725,179.7 USD (could be millions or 100s of thousands, not clear).



############################################## Concept: spend by audience.
# If the reach is the count of individuals who have seen the advert, and 
# impression is how often the advert has been viewed, can we use the spend and 
# completion variables to appreciate the cost of successful advertising for each
# audience group with current formula and then check cost of more successful 
# profile?

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

############################################# Concept: reach by audience.
# This effectively counts the number of unique audience members by group who 
# have seen the advert.

total_reach6 <- creative_final %>%
  filter(audience == "6") %>%
  drop_na(reach, audience) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
total_reach6
# Total number of customers in audience 6 is 5,739,097.

total_reach5 <- creative_final %>%
  filter(audience == "5") %>%
  drop_na(reach, audience) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
total_reach5
# Total number of customers in audience 5 is 1,092,087.

total_reach4 <- creative_final %>%
  filter(audience == "4") %>%
  drop_na(reach, audience) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
total_reach4
# Total number of customers in audience 4 is 4,713,331.

total_reach3 <- creative_final %>%
  filter(audience == "3") %>%
  drop_na(reach, audience) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
total_reach3
# Total number of customers in audience 3 is 1,303,794.

total_reach2 <- creative_final %>%
  filter(audience == "2") %>%
  drop_na(reach, audience) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
total_reach2
# Total number of customers in audience 2 is 347,237.

total_reach1 <- creative_final %>%
  filter(audience == "1") %>%
  drop_na(reach, audience) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
total_reach1
# Total number of customers in audience 1 is 1,506,795.

############################################## Concept: impressions by audience

total_imp6 <- creative_final %>%
  filter(audience == "6") %>%
  drop_na(impressions, audience) %>%
  summarise(total_impressions = sum(impressions)) %>%
  pull(total_impressions)
total_imp6
# Total impressions in audience 6 is 9605114.

total_imp5 <- creative_final %>%
  filter(audience == "5") %>%
  drop_na(impressions, audience) %>%
  summarise(total_impressions = sum(impressions)) %>%
  pull(total_impressions)
total_imp5
# Total impressions in audience 5 is 10539067.

total_imp4 <- creative_final %>%
  filter(audience == "4") %>%
  drop_na(impressions, audience) %>%
  summarise(total_impressions = sum(impressions)) %>%
  pull(total_impressions)
total_imp4
# Total impressions in audience 4 is 18223228.

total_imp3 <- creative_final %>%
  filter(audience == "3") %>%
  drop_na(impressions, audience) %>%
  summarise(total_impressions = sum(impressions)) %>%
  pull(total_impressions)
total_imp3
# Total impressions in audience 3 is 3381018.

total_imp2 <- creative_final %>%
  filter(audience == "2") %>%
  drop_na(impressions, audience) %>%
  summarise(total_impressions = sum(impressions)) %>%
  pull(total_impressions)
total_imp2
# Total impressions in audience 2 is 1053412.

total_imp1 <- creative_final %>%
  filter(audience == "1") %>%
  drop_na(impressions, audience) %>%
  summarise(total_impressions = sum(impressions)) %>%
  pull(total_impressions)
total_imp1
# Total impressions in audience 1 is 4191035.


############################################## Concept: completions by audience.

names(ggoals_final)

total_com6 <- ggoals_final %>%
  filter(audience == "6" & campaign_traffic == "Campaign") %>%
  drop_na(completions, audience) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
total_com6
# Total completions in audience 6 is 5448.

total_com5 <- ggoals_final %>%
  filter(audience == "5" & campaign_traffic == "Campaign") %>%
  drop_na(completions, audience) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
total_com5
# Total completions in audience 5 is 155.

total_com4 <- ggoals_final %>%
  filter(audience == "4" & campaign_traffic == "Campaign") %>%
  drop_na(completions, audience) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
total_com4
# Total completions in audience 4 is 108.

total_com3 <- ggoals_final %>%
  filter(audience == "3" & campaign_traffic == "Campaign") %>%
  drop_na(completions, audience) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
total_com3
# Total completions in audience 3 is 21.

total_com2 <- ggoals_final %>%
  filter(audience == "2" & campaign_traffic == "Campaign") %>%
  drop_na(completions, audience) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
total_com2
# Total completions in audience 2 is 23.

total_com1 <- ggoals_final %>%
  filter(audience == "1" & campaign_traffic == "Campaign") %>%
  drop_na(completions, audience) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
total_com1
# Total completions in audience 1 is 69.


# Check creative_family by completions and audience.

creativefam_comp_audience <- ggoals_final %>%
  drop_na(completions, audience, creative_family) %>%
  ggplot(aes(creative_family, completions, fill = audience))+
  geom_col()+
  theme_bw()+
  coord_flip()+
  labs(title = "Completions by advert and customer type",
       x = "Advert type",
       y = "Completions")
creativefam_comp_audience


# Check ad_format by completions and audience.  
adform_comp_audience <- ggoals_final %>%
  drop_na(completions, audience, ad_format) %>%
  ggplot(aes(ad_format, completions, fill = audience))+
  geom_col()+
  theme_bw()+
  coord_flip()+
  labs(title = "Completions by advert format and customer type",
       x = "Advert format",
       y = "Completions")
adform_comp_audience

# Check platform by completions and audience.
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

# Average completions in platforms and audience data.
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


# Average completions for general traffic data.
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


############################################### Choose best shared information.
unique(creative_final$ad_format)
unique(ggoals_final$ad_format)
unique(creative_final$platform)
unique(ggoals_final$platform)
unique(creative_final$creative_family)
unique(ggoals_final$creative_family)
names(creative_final)
names(ggoals_final)

# Comparing creative_family across two datasets, ggoals and creative.  
# Creative_final.
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

###################################################### Spend by advert family.

spend_family1 <- creative_final %>%
  filter(creative_family == "CloseFaster") %>%
  drop_na(creative_family, spend) %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
spend_family1
# Spend by CloseFaster = 56.92391 x 1000 (I had originally divided by this)

spend_family2 <- creative_final %>%
  filter(creative_family == "UnfairAdvantage") %>%
  drop_na(creative_family, spend) %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
spend_family2
# Spend by UnfairAdvantage =  210.4809x 1000 (I had originally divided by this)

spend_family3 <- creative_final %>%
  filter(creative_family == "CloserTwins") %>%
  drop_na(creative_family, spend) %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
spend_family3
# Spend by CloserTwins =  388.0432 x 1000 (I had originally divided by this)

spend_family4 <- creative_final %>%
  filter(creative_family == "CompetitiveOpportunity") %>%
  drop_na(creative_family, spend) %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
spend_family4
# Spend by CompetitiveOpportunity =  3.261038 x 1000 (I had originally 
# divided by this)

spend_family5 <- creative_final %>%
  filter(creative_family == "SEM Ads") %>%
  drop_na(creative_family, spend) %>%
  summarise(total_spend = sum(spend)) %>%
  pull(total_spend)
spend_family5
# Spend by SEM Ads =  8.67435 x 1000 (I had originally 
# divided by this)


############################################ Completions by advert family.

completions_family1 <- ggoals_final %>%
  filter(creative_family == "CloseFaster" & campaign_traffic == "Campaign") %>%
  drop_na(creative_family, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_family1
# 127 completions in CloseFaster campaign.

completions_family2 <- ggoals_final %>%
  filter(creative_family == "UnfairAdvantage" & 
           campaign_traffic == "Campaign") %>%
  drop_na(creative_family, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_family2
# 333 completions in UnfairAdvantage campaign.

completions_family3 <- ggoals_final %>%
  filter(creative_family == "CloserTwins" & 
           campaign_traffic == "Campaign") %>%
  drop_na(creative_family, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_family3
# 322 completions in CloseTwins campaign.

completions_family4 <- ggoals_final %>%
  filter(creative_family == "CompetitiveOpportunity" & 
           campaign_traffic == "Campaign") %>%
  drop_na(creative_family, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_family4
# 15 completions in CompeitiveOpportunity.

completions_family5 <- ggoals_final %>%
  filter(creative_family == "SEM Ads" & 
           campaign_traffic == "Campaign") %>%
  drop_na(creative_family, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_family5
# 5102 completions in SEM Ads.


################################################### reach by advert family.

reach_family1 <- creative_final %>%
  filter(creative_family == "CloseFaster") %>%
  drop_na(creative_family, reach) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
reach_family1
# 2289129 reach by CloseFaster campaign.

reach_family2 <- creative_final %>%
  filter(creative_family == "CloserTwins") %>%
  drop_na(creative_family, reach) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
reach_family2
# 7905550 reach by CloserTwins campaign.

reach_family3 <- creative_final %>%
  filter(creative_family == "CompetitiveOpportunity") %>%
  drop_na(creative_family, reach) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
reach_family3
# 56018 reach by CompetitiveOpportunity campaign.

reach_family4 <- creative_final %>%
  filter(creative_family == "UnfairAdvantage") %>%
  drop_na(creative_family, reach) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
reach_family4
# 4118194 reach by UnfairAdvantage campaign.

reach_family5 <- creative_final %>%
  filter(creative_family == "SEM Ads") %>%
  drop_na(creative_family, reach) %>%
  summarise(total_reach = sum(reach)) %>%
  pull(total_reach)
reach_family5
# 4118194 reach by UNfairAdvantage campaign.


###################################################### Avg CTR by advert family.

avctr_family1 <- creative_final %>%
  filter(creative_family == "CloseFaster") %>%
  drop_na(creative_family, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
avctr_family1
# 0.003377704 av CTR by CloseFaster campaign.

avctr_family2 <- creative_final %>%
  filter(creative_family == "CloserTwins") %>%
  drop_na(creative_family, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
avctr_family2
# 0.002671387 av CTR by CloserTwins campaign.

avctr_family3 <- creative_final %>%
  filter(creative_family == "CompetitiveOpportunity") %>%
  drop_na(creative_family, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
avctr_family3
# 0.001939112 av CTR by CompetitiveOpportunity campaign.

avctr_family4 <- creative_final %>%
  filter(creative_family == "UnfairAdvantage") %>%
  drop_na(creative_family, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
avctr_family4
# 0.002245306 av CTR by UnfairAdvantage campaign.

avctr_family5 <- creative_final %>%
  filter(creative_family == "SEM Ads") %>%
  drop_na(creative_family, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
avctr_family5
# 0.1389163 av CTR by SEM Ads campaign.

########################################################## Concept ad_format.
# it's impossible to use completions in ggoals_final there is such a disparity
# across datasets in ad_format compared to creative_final.  Creative_final is 
# more digital marketing based and the ggoals version doesn't appear 
# comprehensive and may have some errors.  It would help if we could directly 
# compare them.

# Av CTR by ad_format.
avCTR_adformat_audience <- creative_final %>%
  drop_na(CTR, audience, ad_format) %>%
  group_by(ad_format, audience) %>%
  summarise(average_CTR = mean(CTR)) %>%
  ggplot(aes(ad_format, average_CTR, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average CTR by ad_format and audience",
       x = "Ad_format",
       y = "Average CTR")
avCTR_adformat_audience


# Av spend by ad_format.
avspend_adformat_audience <- creative_final %>%
  drop_na(spend, audience, ad_format) %>%
  group_by(ad_format, audience) %>%
  summarise(average_spend = mean(spend)) %>%
  ggplot(aes(ad_format, average_spend, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average spend by ad_format and audience",
       x = "Ad_format",
       y = "Average spend")
avspend_adformat_audience

# Av impressions per ad_format.
# Check differences between ggoals_final and creative_final ad format again:
unique(ggoals_final$ad_format)
names(ggoals_final)
unique(creative_final$ad_format)
names(creative_final)

avimp_adformat_audience <- creative_final %>%
  drop_na(impressions, audience, ad_format) %>%
  group_by(ad_format, audience) %>%
  summarise(average_impressions = mean(impressions)) %>%
  ggplot(aes(ad_format, average_impressions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average impressions by ad_format and audience",
       x = "Ad_format",
       y = "Average impressions")
avimp_adformat_audience

# Av completions per ad_format.
avcom_adformat_audience <- ggoals_final %>%
  drop_na(completions, audience, ad_format) %>%
  group_by(ad_format, audience) %>%
  summarise(average_completions = mean(completions)) %>%
  ggplot(aes(ad_format, average_completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average completions by ad_format and audience",
       x = "Ad_format",
       y = "Average completions")
avcom_adformat_audience

# Av reach per ad_format.

avreach_adformat_audience <- creative_final %>%
  drop_na(reach, audience, ad_format) %>%
  group_by(ad_format, audience) %>%
  summarise(average_reach = mean(reach)) %>%
  ggplot(aes(ad_format, average_reach, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average reach by ad_format and audience",
       x = "Ad_format",
       y = "Average reach")
avreach_adformat_audience

####################################################### Concept platform.

unique(ggoals_final$platform)
unique(creative_final$platform)

# Av CTR.
avCTR_platform_audience <- creative_final %>%
  drop_na(CTR, audience, platform) %>%
  group_by(platform, audience) %>%
  summarise(average_CTR = mean(CTR)) %>%
  ggplot(aes(platform, average_CTR, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average CTR by platform and audience",
       x = "Platform",
       y = "Average CTR")
avCTR_platform_audience

# Av spend. 
avspend_platform_audience <- creative_final %>%
  drop_na(spend, audience, platform) %>%
  group_by(platform, audience) %>%
  summarise(average_spend = mean(spend)) %>%
  ggplot(aes(platform, average_spend, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average spend (USD) by platform and audience",
       x = "Platform",
       y = "Average spend")
avspend_platform_audience

# Av completions.
avcom_platform_audience <- ggoals_final %>%
  drop_na(completions, audience, platform) %>%
  group_by(platform, audience) %>%
  summarise(average_completions = mean(completions)) %>%
  ggplot(aes(platform, average_completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average completions by platform and audience",
       x = "PLatform",
       y = "Average completions")
avcom_platform_audience

# Av reach.
avreach_platform_audience <- creative_final %>%
  drop_na(reach, audience, platform) %>%
  group_by(platform, audience) %>%
  summarise(average_reach = mean(reach)) %>%
  ggplot(aes(platform, average_reach, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average reach by platform and audience",
       x = "Platform",
       y = "Average reach")
avreach_platform_audience

# Av impressions.
avimp_platform_audience <- creative_final %>%
  drop_na(impressions, audience, platform) %>%
  group_by(platform, audience) %>%
  summarise(average_impressions = mean(impressions)) %>%
  ggplot(aes(platform, average_impressions, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average impressions by platform and audience",
       x = "Platform",
       y = "Average impressions")
avimp_platform_audience

# Calculate total spend by platform.
tot_spend_platform <- creative_final %>%
  filter(platform == "Google SEM") %>%
  drop_na(platform, spend) %>%
  summarise(sum_spend = sum(spend)) %>%
  pull(sum_spend)
tot_spend_platform
# Sum spend is 8.67435 needs multiplying by 1000.

# Calculate total spend by Trade Media.
tot_spend_platform <- creative_final %>%
  filter(platform == "Trade Media") %>%
  drop_na(platform, spend) %>%
  summarise(sum_spend = sum(spend)) %>%
  pull(sum_spend)
tot_spend_platform
# Sum spend is 10.8 (x 1000) for Trade Media.

# Calculate total spend by LinkedIn.
tot_spend_platform <- creative_final %>%
  filter(platform == "LinkedIn") %>%
  drop_na(platform, spend) %>%
  summarise(sum_spend = sum(spend)) %>%
  pull(sum_spend)
tot_spend_platform
# Sum spend is 247.5812 (x 1000) for LinkedIn.

# Calculate total spend by Domain Display.
tot_spend_platform <- creative_final %>%
  filter(platform == "Domain Display") %>%
  drop_na(platform, spend) %>%
  summarise(sum_spend = sum(spend)) %>%
  pull(sum_spend)
tot_spend_platform
# Sum spend is 147.0646 (x 1000).

# Calculate total spend by User ID Display.
tot_spend_platform <- creative_final %>%
  filter(platform == "User ID Display") %>%
  drop_na(platform, spend) %>%
  summarise(sum_spend = sum(spend)) %>%
  pull(sum_spend)
tot_spend_platform
# Sum spend is 55.17846 (x 1000).

# Calculate total spend by User Facebook.
tot_spend_platform <- creative_final %>%
  filter(platform == "Facebook") %>%
  drop_na(platform, spend) %>%
  summarise(sum_spend = sum(spend)) %>%
  pull(sum_spend)
tot_spend_platform
# Sum spend is 5.023483 (x 1000).

# Calculate total spend by User Instagram.
tot_spend_platform <- creative_final %>%
  filter(platform == "Instagram") %>%
  drop_na(platform, spend) %>%
  summarise(sum_spend = sum(spend)) %>%
  pull(sum_spend)
tot_spend_platform
# Sum spend is 1.023678 (x 1000).

# Total completions by Google SEM.
completions_platform1 <- ggoals_final %>%
  filter(platform == "Google SEM" & 
           campaign_traffic == "Campaign") %>%
  drop_na(platform, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_platform1
# 5102 completions in Google SEM Ads.

# Total completions by Trade Media.
completions_platform2 <- ggoals_final %>%
  filter(platform == "Trade Media" & 
           campaign_traffic == "Campaign") %>%
  drop_na(platform, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_platform2 
# 323 completions.

# Total completions by LinkedIn.
completions_platform3 <- ggoals_final %>%
  filter(platform == "LinkedIn" & 
           campaign_traffic == "Campaign") %>%
  drop_na(platform, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_platform3 
# 469 completions.

# Total completions by Domain Display.
completions_platform4 <- ggoals_final %>%
  filter(platform == "Domain Display" & 
           campaign_traffic == "Campaign") %>%
  drop_na(platform, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_platform4 
# 108 completions.

# Total completions by User ID Display.
completions_platform5 <- ggoals_final %>%
  filter(platform == "User ID Display" & 
           campaign_traffic == "Campaign") %>%
  drop_na(platform, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_platform5
# 63 completions.

# Total completions by Facebook.
completions_platform6 <- ggoals_final %>%
  filter(platform == "Facebook" & 
           campaign_traffic == "Campaign") %>%
  drop_na(platform, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_platform6
# 73 completions.

# Total completions by OTT.
completions_platform7 <- ggoals_final %>%
  filter(platform == "OTT" & 
           campaign_traffic == "Campaign") %>%
  drop_na(platform, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_platform7
# 15 completions.

# Total completions by Instagram.
completions_platform8 <- ggoals_final %>%
  filter(platform == "Instagram" & 
           campaign_traffic == "Campaign") %>%
  drop_na(platform, completions) %>%
  summarise(total_completions = sum(completions)) %>%
  pull(total_completions)
completions_platform8
# 0 completions.

# Calculate total reach by Google SEM.
tot_reach_platform1 <- creative_final %>%
  filter(platform == "Google SEM") %>%
  drop_na(platform, reach) %>%
  summarise(sum_reach = sum(reach)) %>%
  pull(sum_reach)
tot_reach_platform1
# Sum reach is 0.

# Calculate total reach by Trade Media.
tot_reach_platform2 <- creative_final %>%
  filter(platform == "Trade Media") %>%
  drop_na(platform, reach) %>%
  summarise(sum_reach = sum(reach)) %>%
  pull(sum_reach)
tot_reach_platform2
# Sum reach is 0.

# Calculate total reach by LinkedIn.
tot_reach_platform3 <- creative_final %>%
  filter(platform == "LinkedIn") %>%
  drop_na(platform, reach) %>%
  summarise(sum_reach = sum(reach)) %>%
  pull(sum_reach)
tot_reach_platform3
# Sum reach is 1368493.

# Calculate total reach by Domain Display.
tot_reach_platform4 <- creative_final %>%
  filter(platform == "Domain Display") %>%
  drop_na(platform, reach) %>%
  summarise(sum_reach = sum(reach)) %>%
  pull(sum_reach)
tot_reach_platform4
# Sum reach is 1944402.

# Calculate total reach by User ID Display.
tot_reach_platform5 <- creative_final %>%
  filter(platform == "User ID Display") %>%
  drop_na(platform, reach) %>%
  summarise(sum_reach = sum(reach)) %>%
  pull(sum_reach)
tot_reach_platform5
# Sum reach is 5850493.

# Calculate total reach by Facebook.
tot_reach_platform6 <- creative_final %>%
  filter(platform == "Facebook") %>%
  drop_na(platform, reach) %>%
  summarise(sum_reach = sum(reach)) %>%
  pull(sum_reach)
tot_reach_platform6
# Sum reach is 159565.

# Calculate total reach by Instagram.
tot_reach_platform7 <- creative_final %>%
  filter(platform == "Instagram") %>%
  drop_na(platform, reach) %>%
  summarise(sum_reach = sum(reach)) %>%
  pull(sum_reach)
tot_reach_platform7
# Sum reach is 29759.

# Total impressions by Google SEM.
tot_imp_platform1 <- creative_final %>%
  filter(platform == "Google SEM") %>%
  drop_na(platform, impressions) %>%
  summarise(sum_impressions = sum(impressions)) %>%
  pull(sum_impressions)
tot_imp_platform1
# Sum impressions is 66477.

# Total impressions by Trade Media.
tot_imp_platform2 <- creative_final %>%
  filter(platform == "Trade Media") %>%
  drop_na(platform, impressions) %>%
  summarise(sum_impressions = sum(impressions)) %>%
  pull(sum_impressions)
tot_imp_platform2
# Sum impressions is 566118.

# Total impressions by LinkedIn.
tot_imp_platform3 <- creative_final %>%
  filter(platform == "LinkedIn") %>%
  drop_na(platform, impressions) %>%
  summarise(sum_impressions = sum(impressions)) %>%
  pull(sum_impressions)
tot_imp_platform3
# Sum impressions is 5231962.

# Total impressions by Domain Display.
tot_imp_platform4 <- creative_final %>%
  filter(platform == "Domain Display") %>%
  drop_na(platform, impressions) %>%
  summarise(sum_impressions = sum(impressions)) %>%
  pull(sum_impressions)
tot_imp_platform4
# Sum impressions is 22609144.

# Total impressions by User ID Display.
tot_imp_platform5 <- creative_final %>%
  filter(platform == "User ID Display") %>%
  drop_na(platform, impressions) %>%
  summarise(sum_impressions = sum(impressions)) %>%
  pull(sum_impressions)
tot_imp_platform5
# Sum impressions is 11031248.

# Total impressions by User Facebook.
tot_imp_platform6 <- creative_final %>%
  filter(platform == "Facebook") %>%
  drop_na(platform, impressions) %>%
  summarise(sum_impressions = sum(impressions)) %>%
  pull(sum_impressions)
tot_imp_platform6
# Sum impressions is 262991.

# Total impressions by Instagram.
tot_imp_platform7 <- creative_final %>%
  filter(platform == "Instagram") %>%
  drop_na(platform, impressions) %>%
  summarise(sum_impressions = sum(impressions)) %>%
  pull(sum_impressions)
tot_imp_platform7
# Sum impressions is 56001.

# Av CTR by Google SEM.
av_CTR_platform1 <- creative_final %>%
  filter(platform == "Google SEM") %>%
  drop_na(platform, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
tot_imp_platform1
# Av CTR is 66477.

# Av CTR by Trade Media.
av_CTR_platform2 <- creative_final %>%
  filter(platform == "Trade Media") %>%
  drop_na(platform, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
tot_imp_platform2
# Av CTR is 566118.

# Av CTR by LinkedIn.
av_CTR_platform3 <- creative_final %>%
  filter(platform == "LinkedIn") %>%
  drop_na(platform, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
tot_imp_platform3
# Av CTR is 5231962.

# Av CTR by Domain Display.
av_CTR_platform4 <- creative_final %>%
  filter(platform == "Domain Display") %>%
  drop_na(platform, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
tot_imp_platform4
# Av CTR is 22609144.

# Av CTR by User ID Display.
av_CTR_platform5 <- creative_final %>%
  filter(platform == "User ID Display") %>%
  drop_na(platform, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
tot_imp_platform5
# Av CTR is 11031248

# Av CTR by Facebook.
av_CTR_platform6 <- creative_final %>%
  filter(platform == "Facebook") %>%
  drop_na(platform, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
tot_imp_platform6
# Av CTR is 262991

# Av CTR by Instagram.
av_CTR_platform7 <- creative_final %>%
  filter(platform == "Instagram") %>%
  drop_na(platform, CTR) %>%
  summarise(av_CTR = mean(CTR)) %>%
  pull(av_CTR)
tot_imp_platform7
# Av CTR is 56001.

################################## Exploring the CRM data.
# In order to use predictive analysis, we either need to merge the separate 
# datasets by finding matching values or we bring sums of data together (not 
# possible for linear regression) or create separate linear relationships and 
# discuss how they might come together.

unique(creative_final$creative_family)
# what sort of data do creative_final, creative_family results offer?
CRM_data <- creative_final %>%
  filter(creative_family == "Carousel Ad2 - CRM Audience 2")
glimpse(CRM_data)
View(CRM_data)
# we only lose 8 rows of data if we exclude.  Contains very limited spend. 
# information.  Contains very little CTR information.  
# Check other CRM data:
CRM_data <- creative_final %>%
  filter(creative_family == "Carousel Ad1 - CRM Audience3")
glimpse(CRM_data)
View(CRM_data)
# Only contains 7 rows. Very little CTR information.  Affects audience 3, as 
# expected. Contains very minimal spend information.  
# Check CRM audience 1:
CRM_data <- creative_final %>%
  filter(creative_family == "Carousel Ad1 - CRM Audience 1")
glimpse(CRM_data)
View(CRM_data)
# Only 10 rows lost if removed.  Unfortunately it does include spend info.
# As expected affects audience1.

# check total amount of observations for both datasets (in preparation for 
# possible merging).
glimpse(creative_final)
# 21 columns, 101,874 rows.  Date is date format. Audience is character.
glimpse(ggoals_final)
# 13 columns, 16,511 rows.

# Joining these datasets may lose a lot of data due to the difference in their 
# sizes. or they may not match if they did not come from the same datasets.

merg_creative_ggoals <- inner_join(creative_final, ggoals_final, 
                                   by = c("date", "creative_family", 
                                          "days_away", "latest_report", 
                                          "audience"))
glimpse(merg_creative_ggoals)
# This has resulted in many more rows than either dataset had originally 
#(n =165,355).  The R warning shows that it is detecting that the data seems to 
# be a 'many to many' type possibly.
# The options is to only use distinct values from each dataset and accept them 
# as samples or try a different type of join.

key_columns <- c("creative_family", "days_away", "date", "latest_report", 
                 "audience")
left_creative_ggoals <- left_join(creative_final, ggoals_final, 
                                  by = key_columns)
glimpse(left_creative_ggoals)
# again this has added extra rows instead of reducing (n=165,355).  
# What if we try to only take a sample using distinct (random effect to 
# some extent).

distinct_creative_final <- distinct(creative_final, latest_report, days_away, 
                                    creative_family, audience, date)
distinct_ggoals_final <- distinct(ggoals_final, latest_report, days_away, 
                                  creative_family, audience, date)
distinct_inner_merged <- inner_join(distinct_creative_final, 
                                    distinct_ggoals_final, 
                                    by = c("date", "creative_family", 
                                           "days_away", "latest_report"))
glimpse(distinct_inner_merged)
# This has produced only 6 columns (so spend and completions are missing) and 
# just under 3000 rows.
# Try again with left join:
distinct_left_merged <- left_join(distinct_creative_final, 
                                    distinct_ggoals_final, 
                                    by = c("date", "creative_family", 
                                           "days_away", "latest_report"))
glimpse(distinct_left_merged)
# Produces near 4,500 rows but only has 6 columns and missing spend and 
# completions. The problem with using aggregation to simplify the issue with 
# the unmatching rows is that it would then be difficult to create a merge and 
# then additionally the aggregation and then manipulating the data to choose a 
# specific smaller number that matched both sets would risk biasing the data 
# and causing problems with the linear regression model.
# Another option might be to do separate linear regression models but only have 
# completions in one dataset and look at spend in another and then discuss the 
# results.

# Attempt to have individual regression models based on filtered data.  We will 
# use individual audiences from each dataset (ggoals_final and creative_final),
# and also filter by campaign.  I have checked and ggoals creative_family non 
# matching proxy campaigns are not significant, so use only shared 
# creative_family. Problem is that this still doesn't resolve the issue where 
# we need need to predict how spend and completions vary together.

# Alternative option is to use the dates 

glimpse(creative_final)
# 101,874 rows and 21 columns.
glimpse(ggoals_final)
# 16,511 rows, 13 columns. So the size of the data is a 6th of creative_final.  

# Campaign by month (completion), campaign proxy is creative family.
# Looking for any trends.
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

# Check completions by campaign proxy and audience.
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

# Check completions by campaign proxy and audience except SEM ads so we can see 
# better.
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
# Best campaign proxies for audiences to be seen here (note SEM Ads all aud 6)

# Check completions by platforms per audience.
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
# exclude Google SEM as obscuring results.

# Exclude Google SEM.
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

############################################### Looking at CTR score.
# Using campaign proxy.
CTRscore_creativefam_audience <- creative_final %>%
  drop_na(CTR_score, audience, creative_family) %>%
  group_by(creative_family, audience) %>%
  summarise(average_CTRscore = mean(CTR_score)) %>%
  ggplot(aes(creative_family, average_CTRscore, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Average CTR score by advert family and audience",
       x = "Advert family",
       y = "Average CTR score")
CTRscore_creativefam_audience

# Using platform and audience.
CTRscore_platform_audience <- creative_final %>%
  drop_na(CTR_score, audience, creative_family) %>%
  group_by(platform, audience) %>%
  summarise(average_CTRscore = mean(CTR_score)) %>%
  ggplot(aes(platform, average_CTRscore, fill = audience)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) + 
  labs(title = "Average CTR score by platform and audience",
       x = "Platform",
       y = "Average CTR score")
CTRscore_platform_audience

# Using platform and campaign proxy.
CTRscore_platform_camp <- creative_final %>%
  drop_na(CTR_score, platform, creative_family) %>%
  filter(creative_family %in% c("CloseFaster", "CloserTwins", 
         "CompetitiveOpportunity", "UnfairAdvantage")) %>%
  group_by(platform, creative_family) %>%
  summarise(average_CTRscore = mean(CTR_score)) %>%
  ggplot(aes(platform, average_CTRscore, fill = creative_family)) + 
  geom_col() +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) + 
  labs(title = "Average CTR score by platform and campaign proxy",
       x = "Platform",
       y = "Average CTR score")
CTRscore_platform_camp

# looking at total reach by platform and audience.
reach_plat_aud <- creative_final %>%
  drop_na(reach, platform, audience) %>%
  group_by(platform, reach, audience) %>%
  ggplot(aes(platform, reach, fill = audience)) + 
  geom_col() +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) + 
  labs(title = "Reach by platfrom and audience",
       x = "Platform",
       y = "Reach")
reach_plat_aud

# looking at total impressions by platform and audience.
imp_plat_aud <- creative_final %>%
  drop_na(impressions, platform, audience) %>%
  group_by(platform, impressions, audience) %>%
  ggplot(aes(platform, impressions, fill = audience)) + 
  geom_col() +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) + 
  labs(title = "Impressions by platform and audience",
       x = "Platform",
       y = "Total Impressions")
imp_plat_aud

# Completions by date. Tried scatterplot to create trend line but doesn't work.
com_date_creat <- ggoals_final %>%
  drop_na(completions, date, creative_family) %>%
  group_by(creative_family, date) %>%
  ggplot(aes(date, completions, fill = creative_family)) + 
  geom_col() +
  theme_bw() +
  coord_flip() +
  labs(title = "Completions over time by campaign proxy",
       x = "Date",
       y = "Total completions")
com_date_creat

# Try a different way to see trend lines over time re completions:
com_date_creat2 <- ggoals_final %>%
  drop_na(completions, date, creative_family) %>%
  group_by(creative_family, date) %>%
  summarise(total_completions = sum(completions)) %>%
  ggplot(aes(date, total_completions, group = creative_family)) + 
  geom_bar(stat = "identity") +  
  theme_bw() +
  labs(title = "Completions over time by campaign proxy",
       x = "Date",
       y = "Total completions") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~creative_family)
com_date_creat2
# This is fantastic, you can see none of the completions increase except 
# UnfairAdvantage slightly.  Also shows when campaign proxies ran.
# Try the same by audiences.
com_date_audience <- ggoals_final %>%
  drop_na(completions, date, audience) %>%
  group_by(audience, date) %>%
  summarise(total_completions = sum(completions)) %>%
  ggplot(aes(date, total_completions, group = audience)) + 
  geom_bar(stat = "identity") +  
  theme_bw() +
  labs(title = "Completions over time by audience",
       x = "Date",
       y = "Total completions") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~audience)
com_date_audience
# There is no real increase in completions over time, marginally for 
# audience 4.

# Remove audience 6 to see the other completions over time by audience better.
com_date_audience2 <- ggoals_final %>%
  drop_na(completions, date, audience) %>%
  filter(audience %in% c("1", "2", "3", "4", "5")) %>%
  group_by(audience, date) %>%
  summarise(total_completions = sum(completions)) %>%
  ggplot(aes(date, total_completions, group = audience)) + 
  geom_bar(stat = "identity") +  
  theme_bw() +
  labs(title = "Completions over time by audience",
       x = "Date",
       y = "Total completions") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~audience)
com_date_audience2
# We have been advised to be very careful with trend lines like this.  Of course
# they are only an indicator but it may be worth cross checking this using 
# periodic aggregations.

# Try to see if % conversions can be done as percentage.
#com_sessions_merge <- goal_general_merge_final %>%
#  drop_na(completions, date, creative_family, total_sessions) %>%
#  group_by(date, creative_family) %>%
#  summarise(conversions_percent = sum(completions / total_sessions)) %>%
#  ggplot(aes(date, conversions_percent, group = creative_family)) + 
#  geom_line() + 
#  theme_bw() +
#  scale_y_continuous(breaks = seq(0, 8, by = 0.8)) + 
#  labs(title = "Conversions over time by campaign proxy",
#       x = "Date",
#       y = "Total conversions") +
#  geom_smooth(method = "lm", se = FALSE, color = "red") +  
#  facet_wrap(~creative_family)
#com_sessions_merge
# Doesn't work reliably even with all variations: change in excel and upload.

# Impressions over time by campaign proxy.
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

# Impressions over time by audience.
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

# Try to see trend line for impressions over time, by audience and by campaign 
# proxy.
imp_audience_time <- creative_final %>%
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
imp_audience_time

# Try to see trend line for reach over time, by audience and by campaign 
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
# Remove audience 6 to see other audience reach better over time.
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

# Check CTR over time as a proxy for emission of campaign proxy.
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

############################### Prepare data for MLR by campaign and audience.
# Data for linear regression is NOT RELIABLE as data is NOT NORMALLY 
# DISTRIBUTED. This was studied but later NOT USED.
# Create filtered datasets.
names(creative_goal_merge_0209)
glimpse(creative_goal_merge_0209)
creative_goal_merge_0209$audience <- 
  as.character(creative_goal_merge_0209$audience)
creative_goal_merge_0209$date <- as.Date(creative_goal_merge_0209$date,
                                         format = "%d/%m/%Y")
glimpse(creative_goal_merge_0209)
# Check unique campaign proxies.
unique(creative_goal_merge_0209$creative_family)
########################################### lmr for CloseFaster by audience 1.
closefaster_1 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "1") %>%
  drop_na(creative_family, audience)

closefaster_1_model <- lm(spend ~ completions + clicks + impressions + reach, 
                          data = closefaster_1)
summary(closefaster_1_model)
# completions are borderline significant, impressions are statistically 
# significant.  Reach has a negative association but not statistically 
# significant.  Adjusted R squared is good at 0.9116.
# Try again without reach and clicks.
closefaster_1_modelb <- lm(spend ~ completions + impressions, 
                          data = closefaster_1)
summary(closefaster_1_modelb)
# Slightly better R squared and completions are statistically significant.
# Adj R squared 0.912.

# Plot the relationship and draw correlation line.
avPlots(closefaster_1_modelb)

# Is it possible to just have a single, significant linear model?
closefaster_1_modelc <- lm(spend ~ completions, data = closefaster_1)
summary(closefaster_1_modelc)
# No you need impressions.
# Maybe test if completions can be predicted from impressions:
closefaster_1_modeld <- lm(completions ~ impressions, data = closefaster_1)
summary(closefaster_1_modeld)
# So not possible to do direct linear regression to predict completions here.

############################################ lmr CloseFaster by audience 2. 

closefaster_2 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "2")%>%
  drop_na(creative_family, audience)

closefaster_2_model <-lm(spend ~ completions + clicks + impressions + reach, 
                         data = closefaster_2) 
summary(closefaster_2_model)
# Here reach and impressions are significant.  I will remove clicks and test.

closefaster_2_modelb <- lm(spend ~ completions + impressions + reach, 
                           data = closefaster_2)
summary(closefaster_2_modelb)
avPlots(closefaster_2_modelb)
# Negative association between reach and spend.  Reach and impressions are 
# significant but completion is not.  Adj R squared great at 0.91.

############################################# lr for CloseFaster by audience 3.

closefaster_3 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "3")%>%
  drop_na(creative_family, audience) 

closefaster_3_model <- lm (spend ~ completions, data = closefaster_3)
summary(closefaster_3_model)
# completions are not statistically significant on their own.  
closefaster_3_modelb <- lm(spend ~ completions + impressions + reach, 
                           data = closefaster_3)
summary(closefaster_3_modelb)
# Can predict significantly with impressions and reach, but not completions.
# Hopefully with larger datasets we can. R squared 0.996, very good.


############################################## lr for CloseFaster by audience 4.

closefaster_4 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "4")%>%
  drop_na(creative_family, audience) 

closefaster_4_model <- lm(spend ~ completions + reach + impressions, 
                          data = closefaster_4)
summary(closefaster_4_model)
# thank goodness, completions is significant and so is impressions.
# Try again with just completions.

closefaster_4_modelb <- lm (spend ~ completions, data = closefaster_4)
summary(closefaster_4_modelb)
# We can predict spend by the completions we want through CloseFaster for 
# audience 4, but the model is weak at R squared 0.009.

############################################# lr for CloseFaster by audience 5.

closefaster_5 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "5")%>%
  drop_na(creative_family, audience) 

closefaster_5_model <- lm(spend ~ completions, data = closefaster_5)
summary(closefaster_5_model)
# Hallelujah.  But it has a very low R squared.

closefaster_5_modelb <- lm(spend ~ completions + reach + impressions, 
                           data = closefaster_5)
summary(closefaster_5_modelb)
# Very low R squared of 0.31.  Poor predicted value although the p values are 
# good.

############################################# lr for CloseFaster by audience 6.

closefaster_6 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "6") %>%
  drop_na(creative_family, audience)  

closefaster_6_model <- lm(spend ~ completions + reach + impressions, 
                          data = closefaster_6)
summary(closefaster_6_model)
# no results here.

############################################ lr for CloserTwins by audience 1.

closertwins_1 <-creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "1") %>%
  drop_na(creative_family, audience)  

closertwins_1_model <- lm(spend ~ completions + reach + impressions, 
                          data = closertwins_1)
summary(closertwins_1_model)
# All variables are statistically significant but Adj R is only 0.64.
# Check value of completions:
closertwins_1_modelb <- lm(spend ~ completions, 
                          data = closertwins_1)
summary(closertwins_1_modelb)
# Minuscule adj R squared at 0.0034.

############################################ lr for CloserTwins by audience 2.

closertwins_2 <-creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "2") %>%
  drop_na(creative_family, audience)  

closertwins_2_model <- lm(spend ~ completions + reach + impressions, 
                          data = closertwins_2)
summary(closertwins_2_model)
# This is the smallest audience group and although all variables are 
# statistically significant, the adj R squared is very small at 0.2443.

############################################## lr for CloserTwins by aud 3.

closertwins_3 <-creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "3") %>%
  drop_na(creative_family, audience)  

closertwins_3_model <- lm(spend ~ completions + reach + impressions, 
                          data = closertwins_3)
summary(closertwins_3_model)
# Completions not statistically significant but reach and impressions are.
# Adj R squared is much better at 0.8691.

############################################# lr for CloserTwins by aud 4.

closertwins_4 <-creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "4") %>%
  drop_na(creative_family, audience)  

closertwins_4_model <- lm(spend ~ completions + reach + impressions, 
                          data = closertwins_4)
summary(closertwins_4_model)
# All variables are statistically significant, adj R squared is 0.793.

########################################### lr for CloserTwins by aud 5.

closertwins_5 <-creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "5") %>%
  drop_na(creative_family, audience)  

closertwins_5_model <- lm(spend ~ completions + reach + impressions, 
                          data = closertwins_5)
summary(closertwins_5_model)
# All variables statistically significant, Adj R squared not too bad at 0.72.

########################################### lr for CloserTwins by aud 6.

closertwins_6 <-creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "6") %>%
  drop_na(creative_family, audience)  

closertwins_6_model <- lm(spend ~ completions + reach + impressions, 
                          data = closertwins_5)
summary(closertwins_6_model)
# All varibales statistically significant and Adj R squared not bad at 0.72.

########################################### lr for UnFairAdvantage by aud 1.

unfairadvantage_1 <-creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "1") %>%
  drop_na(creative_family, audience)  

unfairadvantage_1_model <- lm(spend ~ completions + reach + impressions, 
                          data = unfairadvantage_1)
summary(unfairadvantage_1_model)
# None of the variables are statistically significant.

############################################ lr for UnFairAdvantage by aud 2.

unfairadvantage_2 <-creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "2") %>%
  drop_na(creative_family, audience)  

unfairadvantage_2_model <- lm(spend ~ completions + reach + impressions, 
                              data = unfairadvantage_2)
summary(unfairadvantage_2_model)
# Only reach and impressions significant, adj R squared low at 0.47.

############################################ lr for UnFairAdvantage by aud 3.

unfairadvantage_3 <-creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "3") %>%
  drop_na(creative_family, audience)  

unfairadvantage_3_model <- lm(spend ~ completions + reach + impressions, 
                              data = unfairadvantage_3)
summary(unfairadvantage_3_model)
# Completions and impressions alone are statistically significant, adj r is 
# 0.48.
# Try again without reach:
unfairadvantage_3_modelb <- lm(spend ~ completions + impressions, 
                              data = unfairadvantage_3)
summary(unfairadvantage_3_modelb)
# No real improvement.

########################################### lr for UnFairAdvantage by aud 4.

unfairadvantage_4 <-creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "4") %>%
  drop_na(creative_family, audience)  

unfairadvantage_4_model <- lm(spend ~ completions + reach + impressions, 
                              data = unfairadvantage_4)
summary(unfairadvantage_4_model)
# All variables significant but adj R squared is 0.43.

########################################### lr for UnFairAdvantage by aud 5.

unfairadvantage_5 <-creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "5") %>%
  drop_na(creative_family, audience)  

unfairadvantage_5_model <- lm(spend ~ completions + reach + impressions, 
                              data = unfairadvantage_5)
summary(unfairadvantage_5_model)
# All variables significant but adj r squared terrible at 0.123.

########################################## lr for UnFairAdvantage by aud 6.

#unfairadvantage_6 <-creative_goal_merge_0209 %>%
#  filter(creative_family == "UnfairAdvantage" & audience == "6") %>%
#  drop_na(creative_family, audience)  

#unfairadvantage_6_model <- lm(spend ~ completions + reach + impressions, 
#                              data = unfairadvantage_6)
#summary(unfairadvantage_6_model)
# There is no Unfairadvantage with audience 6.

###################################### lr for CompetitiveOpportunity by aud 6.

competitiveopp_6 <-creative_goal_merge_0209 %>%
  filter(creative_family == "CompetitiveOpportunity" & audience == "6") %>%
  drop_na(creative_family, audience)  

competitiveopp_6_model <- lm(spend ~ completions + reach + impressions, 
                              data = competitiveopp_6)
summary(competitiveopp_6_model)
# reach and impressions are significant but adj R squared is very low at 0.2.

####################################### lr for SEM Ads by audience 6.

semads_6 <-creative_goal_merge_0209 %>%
  filter(creative_family == "SEM Ads" & audience == "6") %>%
  drop_na(creative_family, audience)  

semads_6_model <- lm(spend ~ completions + reach + impressions, 
                             data = semads_6)
summary(semads_6_model)
# Reach is not available, but completions and impressions are significant.
# Adj R squared is 0.78.
# Try without reach:
semads_6_modelb <- lm(spend ~ completions + impressions, 
                     data = semads_6)
summary(semads_6_modelb)
# No real difference.
# Check with only completions:
semads_6_modelc <- lm(spend ~ completions, 
                      data = semads_6)
summary(semads_6_modelc)
# This can be predicted with completions wish alone and offers R squared of 70%.

################################################ lr Trade Media Ads by aud 6.
# There are no Trade Media Ads represented. 

###################### Check cost of each completion by aud and campaign proxy.
###################### Aud 1, CloserTwins
# Filter the dataset for CloserTwins creative family and audience 1
filter_ct_1 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "1") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ct_1$spend)
total_completions <- sum(filter_ct_1$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 1, CloserTwins): $", comprice)
# Total Cost: $ 7381.555 
# Total Completions:  45 
# Cost per Completion (Audience 1, CloserTwins): $ 164.0346

######################## Aud 2, CloserTwins
filter_ct_2 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "2") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ct_2$spend)
total_completions <- sum(filter_ct_2$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 2, CloserTwins): $", comprice)
# Total Cost: $ 7381.555 
# Total Completions:  14 
# Cost per Completion (Audience 2, CloserTwins): $ 527.2539

######################## Aud 3, CloserTwins
filter_ct_3 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "3") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ct_3$spend)
total_completions <- sum(filter_ct_3$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 3, CloserTwins): $", comprice)
# Total Cost: $ 4581.747
# Total Completions:  10 
# Cost per Completion (Audience 3, CloserTwins): $ 458.1747

####################### Aud 4, CloserTwins.
filter_ct_4 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "4") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ct_4$spend)
total_completions <- sum(filter_ct_4$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 4, CloserTwins): $", comprice)
# Total Cost: $ 38450.73 
# Total Completions:  30 
# Cost per Completion (Audience 4, CloserTwins): $ 1281.691

####################### Aud 5, CloserTwins.
filter_ct_5 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "5") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ct_5$spend)
total_completions <- sum(filter_ct_5$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 5, CloserTwins): $", comprice)
# Total Cost: $ 19758.78 
# Total Completions:  35 
# Cost per Completion (Audience 5, CloserTwins): $ 564.5366

####################### Aud 6, CloserTwins.
filter_ct_6 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloserTwins" & audience == "6") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ct_6$spend)
total_completions <- sum(filter_ct_6$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 6, CloserTwins): $", comprice)
# Total Cost: $ 165762.7 
# Total Completions:  14 
# Cost per Completion (Audience 6, CloserTwins): $ 11840.19

####################### Aud 1, CloseFaster.
filter_cf_1 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "1") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_cf_1$spend)
total_completions <- sum(filter_cf_1$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 1, CloseFaster): $", comprice)
# Total Cost: $ 4691.513 
# Total Completions:  4 
# Cost per Completion (Audience 1, CloseFaster): $ 1172.878

####################### Aud 2, CloseFaster.
filter_cf_2 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "2") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_cf_2$spend)
total_completions <- sum(filter_cf_2$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 2, CloseFaster): $", comprice)
# Total Cost: $ 839.7364 
# Total Completions:  5 
# Cost per Completion (Audience 2, CloseFaster): $ 167.9473

####################### Aud 3, CloseFaster.
filter_cf_3 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "3") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_cf_3$spend)
total_completions <- sum(filter_cf_3$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 3, CloseFaster): $", comprice)
# Total Cost: $ 3840.733 
# Total Completions:  3
# Cost per Completion (Audience 3, CloseFaster): $ 1280.244

####################### Aud 4, CloseFaster.
filter_cf_4 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "4") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_cf_4$spend)
total_completions <- sum(filter_cf_4$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 4, CloseFaster): $", comprice)
# Total Cost: $ 19262.24
# Total Completions:  21 
# Cost per Completion (Audience 4, CloseFaster): $ 917.2494

####################### Aud 5, CloseFaster.
filter_cf_5 <- creative_goal_merge_0209 %>%
  filter(creative_family == "CloseFaster" & audience == "5") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_cf_5$spend)
total_completions <- sum(filter_cf_5$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 5, CloseFaster): $", comprice)
# Total Cost: $ 16385.3 
# Total Completions:  35
# Cost per Completion (Audience 5, CloseFaster): $ 468.1514

####################### Aud 1, UnfairAdvantage.
filter_ua_1 <- creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "1") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ua_1$spend)
total_completions <- sum(filter_ua_1$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 1, UnfairAdvantage): $", comprice)
# Total Cost: $ 4078.666 
# Total Completions:  10 
# Cost per Completion (Audience 1, UnfairAdvantage): $ 407.8666

####################### Aud 2, UnfairAdvantage.
filter_ua_2 <- creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "2") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ua_2$spend)
total_completions <- sum(filter_ua_2$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 2, UnfairAdvantage): $", comprice)
# Total Cost: $ 2061.945 
# Total Completions:  2 
# Cost per Completion (Audience 2, UnfairAdvantage): $ 1030.972

####################### Aud 3, UnfairAdvantage.
filter_ua_3 <- creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "3") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ua_3$spend)
total_completions <- sum(filter_ua_3$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 3, UnfairAdvantage): $", comprice)
# Total Cost: $ 1429.39 
# Total Completions:  3
# Cost per Completion (Audience 3, UnfairAdvantage): $ 476.4635

####################### Aud 4, UnfairAdvantage.
filter_ua_4 <- creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "4") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ua_4$spend)
total_completions <- sum(filter_ua_4$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 4, UnfairAdvantage): $", comprice)
# Total Cost: $ 79983.4 
# Total Completions:  45
# Cost per Completion (Audience 4, UnfairAdvantage): $ 1777.409

####################### Aud 5, UnfairAdvantage.
filter_ua_5 <- creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "5") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_ua_5$spend)
total_completions <- sum(filter_ua_5$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 5, UnfairAdvantage): $", comprice)
# Total Cost: $ 70562.61
# Total Completions:  77
# Cost per Completion (Audience 5, UnfairAdvantage): $ 916.3976

####################### Aud 6, CompetitiveOpportunity.
filter_co_6 <- creative_goal_merge_0209 %>%
  filter(creative_family == "UnfairAdvantage" & audience == "6") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_co_6$spend)
total_completions <- sum(filter_co_6$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 6, CompetitiveOpportunity): $", comprice)
# no results but I have top line data that will work as only affects aud 6..

########################################## Aud 6, Trade Media ads.
filter_tm_6 <- creative_goal_merge_0209 %>%
  filter(creative_family == "Trade Media Ads" & audience == "6") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_tm_6$spend)
total_completions <- sum(filter_tm_6$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 6, Trade Media Ads): $", comprice)
# No results.  

########################################## Aud 6, SEM Ads.
filter_sa_6 <- creative_goal_merge_0209 %>%
  filter(creative_family == "SEM Ads" & audience == "6") %>%
  drop_na(creative_family, audience, spend, completions)
# Total cost and total completions
total_spend <- sum(filter_sa_6$spend)
total_completions <- sum(filter_sa_6$completions)
# Cost per completion (comprice)
comprice <- total_spend / total_completions
# Print the result
cat("Total Cost: $", total_spend, "\n")
cat("Total Completions: ", total_completions, "\n")
cat("Cost per Completion (Audience 6, SEM Ads): $", comprice)
# Total Cost: $ 6875.2 
# Total Completions:  4711 
# Cost per Completion (Audience 6, SEM Ads): $ 1.459393.
# Lowest cost per completion across all options.

###################################################### visualisations merge.
# Import and prepare dataset. 
#################### This dataset was not used as original data was aggregated 
# comparing audience and campaign spend and completions in excel.
#spend_results <- read_csv("results_merge_completions_spend.csv")
#names(spend_results)
#glimpse(spend_results)
#spend_results$audience <- as.character(spend_results$audience)
#glimpse(spend_results)

#ggplot(spend_results, aes(x = audience)) +
#  geom_line(aes(y = closer_twins_cost, 
#                color = "Closer Twins Cost"), group = 1) +
#  geom_line(aes(y = close_faster_cost, 
#                color = "Close Faster Cost"), group = 1) +
#  geom_line(aes(y = unfair_advantage_cost, 
#                color = "Unfair Advantage Cost"), group = 1) +
#  geom_line(aes(y = competitive_opportunity_cost, 
#                color = "Competitive Opportunity Cost"), group = 1) +
#  geom_line(aes(y = SEM_ads_cost, color = "SEM Ads Cost"), group = 1) +
#  labs(title = "Cost per completion by campaign proxy", 
#       x = "audience", y = "Campaign proxy cost of each completion") +
#  theme_minimal() +
#  scale_color_manual(values = c("Closer Twins Cost" = "blue",
#                                "Close Faster Cost" = "red",
#                                "Unfair Advantage Cost" = "orange",
#                                "Competitive Opportunity Cost" = "green",
#                                "SEM Ads Cost" = "purple"))

# Difficult to visualise audience 6 due to huge disparity between options.
# CO and SEM Ads only apply to audience 6.
# Reshape the data from wide to long format
#spend_results_long <- spend_results %>%
#  drop_na() %>%
#  filter(audience == "6")%>%
#  gather(variable, completions, closer_twins_cost, 
#         competitive_opportunity_cost, SEM_ads_cost)
#glimpse(spend_results_long)

# Create the ggplot with multiple variable columns as x-axis
#ggplot(spend_results_long, aes(x = variable, y = completions)) +
#  geom_col() +
#  theme_bw() +
#  coord_flip() +
#  labs(title = "Cost per completion for audience 6",
#       x = "Advert type",
#       y = "Cost per completion")
#### the above code needs fixing the long versions has not worked.

####### Below plot of spend per campaign proxy and audience.
########################################### This were not used as they were re-
# calculated for reliability as aggregates in excel.
#ggplot(spend_results, aes(x = audience)) +
#  geom_line(aes(y = closer_twins_spend, 
#                color = "Closer Twins Spend"), group = 1) +
#  geom_line(aes(y = close_faster_spend, 
#                color = "Close Faster Spend"), group = 1) +
#  geom_line(aes(y = unfair_advantage_spend, 
#                color = "Unfair Advantage Spend"), group = 1) +
#  geom_line(aes(y = competitive_opportunity_spend, 
#                color = "Competitive Opportunity Spend"), group = 1) +
#  geom_line(aes(y = SEM_ads_spend, color = "SEM Ads Spend"), group = 1) +
#  labs(title = "Spend per campaign proxy by audience", 
#       x = "audience", y = "Total spend per campaign proxy") +
#  theme_minimal() +
#  scale_color_manual(values = c("Closer Twins Spend" = "blue",
#                                "Close Faster Spend" = "red",
#                                "Unfair Advantage Spend" = "orange",
#                                "Competitive Opportunity Spend" = "green",
#                                "SEM Ads Spend" = "purple"))
# Issues with visualising audience 6.
######## Below plot for number of completions per campaign, by audience.

#ggplot(spend_results, aes(x = audience)) +
#  geom_line(aes(y = closer_twins_completions, 
  #               color = "Closer Twins Completions"), group = 1) +
  # geom_line(aes(y = close_faster_completions, 
  #               color = "Close Faster Completions"), group = 1) +
  # geom_line(aes(y = unfair_advantage_completions, 
  #               color = "Unfair Advantage Completions"), group = 1) +
  # geom_line(aes(y = competitive_opportunity_completions, 
  #               color = "Competitive Opportunity Completions"), group = 1) +
  # geom_line(aes(y = SEM_ads_completions, color = "SEM Ads Completions"), group = 1) +
  # labs(title = "Total completions per campaign proxy by audience", 
  #      x = "Audience", y = "Total completions per campaign proxy") +
  # theme_minimal() +
  # scale_color_manual(values = c("Closer Twins Completions" = "blue",
  #                               "Close Faster Completions" = "red",
  #                               "Unfair Advantage Completions" = "orange",
  #                               "Competitive Opportunity Completions" = "green",
  #                               "SEM Ads Completions" = "purple")) +
  # scale_y_continuous(limits = c(0, 80))


######################################### Checks for linearity.
names(creative_goal_merge_0209)
unique(creative_goal_merge_0209$creative_family)
glimpse(creative_goal_merge_0209)
unique(creative_goal_merge_0209$audience)

################ Relationship between spend and completions by audience.
completions_spend_aud <- creative_goal_merge_0209 %>%
  drop_na(spend, completions, audience) %>%
  ggplot(aes(spend, completions, color = audience)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() +
  labs(title = "Completions and spend by audience",
       x = "Spend ($)",
       y = "Completions") +
  scale_color_discrete()

completions_spend_aud
# These relationships are NOT RELIABLE for linear modelling.

# Relationship between spend and completions by audience - exclude audience 6.
completions_spend_aud_ex6 <- creative_goal_merge_0209 %>%
  filter(audience != "6") %>%
  drop_na(spend, completions, audience) %>%
  ggplot(aes(spend, completions, color = audience)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() +
  labs(title = "Completions and spend by audience",
       x = "Spend ($)",
       y = "Completions") +
  scale_color_discrete()

completions_spend_aud_ex6
# This only shows smooth lines for audiences 4 and 5.  I will separate filters.
# These relationships are NOT RELIABLE for linear modelling.

# Relationship between spend and completions by audiences 1 to 3.
completions_spend_aud_1t3 <- creative_goal_merge_0209 %>%
  filter(audience %in% c("1", "2", "3")) %>%
  drop_na(spend, completions, audience) %>%
  ggplot(aes(spend, completions, color = audience)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = audience)) + 
  theme_bw() +
  labs(title = "Completions and spend by audiences 1 to 3",
       x = "Spend ($)",
       y = "Completions") +
  scale_color_discrete()

completions_spend_aud_1t3
# There is no real relationship between completions and audiences in groups 1
# to 3.
# These relationships are NOT RELIABLE for linear modelling.

###################### Relationship between spend and completions by camp proxy

# completions_spend_camp <- creative_goal_merge_0209 %>%
#   drop_na(spend, completions, creative_family) %>%
#   ggplot(aes(spend, completions, color = creative_family)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE, aes(group = creative_family)) + 
#   theme_bw() +
#   labs(title = "Completions and spend by campaign proxies",
#        x = "Spend ($)",
#        y = "Completions") +
#   scale_color_discrete()
# 
# completions_spend_camp
# This shows that although very little money was spent on SEM ads, 
# it worked perfectly for audience 6, but as we know, CloserTwins had the 
# highest amount spent on it but proportionately fewer completions.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

####################### Relationship between reach and completions by audience.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, SO NOT USED.
completions_reach_aud <- creative_goal_merge_0209 %>%
  drop_na(spend, completions, audience) %>%
  ggplot(aes(spend, completions, color = audience)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() +
  labs(title = "Completions and reach by audience",
       x = "Reach",
       y = "Completions") +
  scale_color_discrete()

completions_reach_aud
# No relationship between reach and audience 6 completions due to advertising 
# format.
# Filter audience 6 out for better view.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

completions_reach_aud_ex6 <- creative_goal_merge_0209 %>%
  drop_na(spend, completions, audience) %>%
  filter(audience != "6") %>%
  ggplot(aes(spend, completions, color = audience)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() +
  labs(title = "Completions and reach by audience exc 6",
       x = "Reach",
       y = "Completions") +
  scale_color_discrete()

completions_reach_aud_ex6
# Some relationship between audiences 5 and 4 re reach and completions.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

###################################### impressions and completions by audience

completions_imp_aud <- creative_goal_merge_0209 %>%
  drop_na(spend, completions, audience) %>%
  ggplot(aes(spend, completions, color = audience)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() +
  labs(title = "Completions and impressions by audience",
       x = "Impressions",
       y = "Completions") +
  scale_color_discrete()

completions_imp_aud
# this is irrelevant for audience 6 and also obscures view for others.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

completions_imp_aud_ex6 <- creative_goal_merge_0209 %>%
  drop_na(spend, completions, audience) %>%
  filter(audience != "6") %>%
  ggplot(aes(spend, completions, color = audience)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw() +
  labs(title = "Completions and impressions by audience excl 6",
       x = "Impressions",
       y = "Completions") +
  scale_color_discrete()

completions_imp_aud_ex6  
# so relationship between audiences 4 and 5 between reach and completions as 
# well as impressions and completion.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

############################################## Check normality of data.
########################### Spend.
# QQ plot of spend.
qqnorm(creative_goal_merge_0209$spend)
qqline(creative_goal_merge_0209$spend)
# disordered qqplot.  
# Will need to transform data and consider removing outliers.
# S shaped indicating heavier tails.

# Histogram.
hist(creative_goal_merge_0209$spend)
# Data is very skewed.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

# Shapiro test for normality.
# shapiro.test(creative_goal_merge_0209$spend)
# Our dataset is too big for shapiro, qq should be good enough.

# Check for skew.
skewness(creative_goal_merge_0209$spend)
# very high positive 22, meaning very long right tail.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

# Check for kurtosis.
kurtosis(creative_goal_merge_0209$spend)
# very high 561 when it should be no more than 3.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

# Try filter out audienec 6.
excl_6 <- creative_goal_merge_0209 %>%
  filter(audience != "6")

qqnorm(excl_6$spend)
qqline(excl_6$spend)
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

######################### Completions.
# QQ plot of completions.
qqnorm(creative_goal_merge_0209$completions)
qqline(creative_goal_merge_0209$completions)
# disordered qqplot.  
# Will need to transform data and consider removing outliers.
# S shaped indicating heavier tails.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

# Histogram.
hist(creative_goal_merge_0209$completions)
# again very skewed.  
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

######################### impressions.

# QQ plot of impressions.
qqnorm(creative_goal_merge_0209$impressions)
qqline(creative_goal_merge_0209$impressions)
# disordered qqplot.  
# Will need to transform data and consider removing outliers.
# S shaped indicating heavier tails.

# histogram
hist(creative_goal_merge_0209$impressions)
# again very skewed.  
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

####################### reach.

# QQ plot of reach.
qqnorm(creative_goal_merge_0209$reach)
qqline(creative_goal_merge_0209$reach)
# disordered qqplot.  
# Will need to transform data and consider removing outliers.
# S shaped indicating heavier tails.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

# histogram.
hist(creative_goal_merge_0209$reach)
# again very skewed.  
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.

################################################ platform by campaign?
# Check spend by platform and audience.
# Summarize spend by platform
platform_audience_spend_summary <- creative_final %>%
  drop_na(spend, platform, audience) %>%
  group_by(platform, audience) %>%
  summarize(total_spend = sum(spend))
# Spend by platform and audience.
spend_plat_aud <- ggplot(platform_audience_spend_summary, 
                         aes(x = platform, y = total_spend, fill = audience)) +
  geom_col() +
  theme_bw() +
  labs(title = "Spend by Platform and Audience",
       x = "Platform",
       y = "Total Spend (per $1000)") +
  coord_flip()+
  scale_fill_brewer(palette = "Spectral")
# This different palette was used to better match others' python code and excel 
# palette.

spend_plat_aud

# total spend.
total_spend <- creative_final %>%
  drop_na(spend)%>%
  summarise(total_spend = sum(spend))

total_spend
# NOte that this was earlier divided by 1000 for better visualisation.
# Summarize spend by platform in merged doc.
platform_audience_spend_summary <- creative_goal_merge_0209 %>%
  drop_na(spend, platform, audience) %>%
  group_by(platform, audience) %>%
  summarize(total_spend = sum(spend))
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.
# Spend by platform and audience in merged doc.
spend_plat_aud <- ggplot(platform_audience_spend_summary, 
                         aes(x = platform, y = total_spend, fill = audience)) +
  geom_col() +
  theme_bw() +
  labs(title = "Spend by Platform and Audience",
       x = "Platform",
       y = "Total Spend (per $1000)") +
  coord_flip()+
  scale_fill_brewer(palette = "Spectral")

spend_plat_aud
# Both look the same.

###################################### Check completions by platform & audience
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.
# Summarize completions by platform
completions_platform_audience_summary <- creative_goal_merge_0209 %>%
  drop_na(completions, platform, audience) %>%
  group_by(platform, audience) %>%
  summarize(total_completions = sum(completions))

# Spend by platform and audience.
compl_plat_aud <- ggplot(completions_platform_audience_summary, 
                         aes(x = platform, 
                             y = total_completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  labs(title = "Completions by Platform and Audience",
       x = "Platform",
       y = "Total Completions") +
  coord_flip()+
  scale_fill_brewer(palette = "Spectral")

compl_plat_aud

############################## Do it again with no Google SEM.
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.
# Summarize completions by platform excl Google SEM.
completions_platform_audience_summary_exg <- creative_goal_merge_0209 %>%
  filter(platform != "Google SEM") %>%
  drop_na(completions, platform, audience) %>%
  group_by(platform, audience) %>%
  summarize(total_completions = sum(completions))

# Spend by platform and audience.
compl_plat_aud <- ggplot(completions_platform_audience_summary_exg, 
                         aes(x = platform, 
                             y = total_completions, fill = audience)) +
  geom_col() +
  theme_bw() +
  labs(title = "Completions by Platform and Audience (ex SEM)",
       x = "Platform",
       y = "Total Completions") +
  coord_flip()+
  scale_fill_brewer(palette = "Spectral")

compl_plat_aud

# completions by audience:
# This data merge was only useful for correlation checks for spend and 
# completions and is less reliable than the original data, so not used.
completions_audience_summary <- creative_goal_merge_0209 %>%
  drop_na(completions, audience) %>%
  group_by(audience) %>%
  summarize(total_completions = sum(completions))

# Completions by platform and audience.
compl_aud <- ggplot(completions_audience_summary, 
                         aes(x = audience, 
                             y = total_completions)) +
  geom_col() +
  theme_bw() +
  labs(title = "Completions by Audience",
       x = "Audience",
       y = "Total Completions") +
  coord_flip()+
  scale_fill_brewer(palette = "Spectral")

compl_aud

####################################### weighted CTR score campaigns.
desired_creative_family_values <- c("UnfairAdvantage", 
                                    "CompetitiveOpportunity", "CloserTwins", 
                                    "CloseFaster")
weightedCTR_audience_summary <- creative_final %>%
  filter(creative_family %in% desired_creative_family_values) %>%
  drop_na(weighted_CTR_score, creative_family, audience) %>%
  group_by(creative_family, audience) %>%
  summarize(total_weightedCTR = sum(weighted_CTR_score))
# Plot
weight_plat_aud <- ggplot(weightedCTR_audience_summary, 
                          aes(x = creative_family, y = total_weightedCTR, 
                              fill = audience)) +
  geom_col() +
  theme_bw() +
  labs(title = "Weighted CTR by campaign proxy and audience",
       x = "Campaign proxy",
       y = "Total weighted CTR score") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral")
weight_plat_aud
# Weighted CTR score was not later used.
####################################### impressions by campaigns.
desired_creative_family_values <- c("UnfairAdvantage", 
                                    "CompetitiveOpportunity", "CloserTwins", 
                                    "CloseFaster")
imp_camp_audience_summary <- creative_final %>%
  filter(creative_family %in% desired_creative_family_values) %>%
  drop_na(impressions, creative_family, audience) %>%
  group_by(creative_family, audience) %>%
  summarize(total_imp = sum(impressions))

imp_camp_aud <- ggplot(imp_camp_audience_summary, 
                          aes(x = creative_family, y = total_imp, 
                              fill = audience)) +
  geom_col() +
  theme_bw() +
  labs(title = "Total impressions by campaign proxy and audience",
       x = "Campaign proxy",
       y = "Total impressions") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral")

imp_camp_aud
# Unfortunately these aren't measured by cost per impression or similar value 
# and so limited in their planning of campaign use.
####################################### reach by campaigns.
desired_creative_family_values <- c("UnfairAdvantage", 
                                    "CompetitiveOpportunity", "CloserTwins", 
                                    "CloseFaster")

reach_camp_audience_summary <- creative_final %>%
  filter(creative_family %in% desired_creative_family_values) %>%
  drop_na(reach, creative_family, audience) %>%
  group_by(creative_family, audience) %>%
  summarize(total_reach = sum(reach))

reach_camp_aud <- ggplot(reach_camp_audience_summary, 
                       aes(x = creative_family, y = total_reach, 
                           fill = audience)) +
  geom_col() +
  theme_bw() +
  labs(title = "Total reach by campaign proxy and audience",
       x = "Campaign proxy",
       y = "Total reach") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral")

reach_camp_aud
# Unfortunately these aren't measured by cost per impression or similar value 
# and so limited in their planning of campaign use. This is a bit misleading as 
# it can't include SEM ads. 
names(creative_final)
correlation_spend_imp <- creative_final %>%
  drop_na(impressions, spend)

corr <- cor(correlation_spend_imp$spend, 
                            correlation_spend_imp$impressions)
corr
# 0.5355537 correlation.  

####################################### reviewed data, for final presentation
######################################### MOST RELIABLE NUMBERS.
# Glimpse new data set.
glimpse(spend_amalgamated)
spend_amalgamated$audience <- as.character(spend_amalgamated$audience)
spend_amalgamated$tot_spend_byaud_group <- 
  as.numeric(spend_amalgamated$tot_spend_byaud_group)

## Treemaps are in python.

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

# This would have been a better way of presenting the above data (see below).
# Creating a grouped bar chart for the aggregated results.
# Create pivot longer version of data:
# First create new data frame:
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

# Re-do facet wrap impressions with better scale axis, so divide by 1000 to
# represent K:
# Impressions over time by campaign proxy.
imp_camp_time <- creative_final %>%
  drop_na(impressions, date, creative_family) %>%
  filter(creative_family %in% c("CloseFaster", "CloserTwins", 
                                "CompetitiveOpportunity", 
                                "UnfairAdvantage")) %>%
  group_by(creative_family, date) %>%
  summarise(total_impressions = sum(impressions/1000)) %>%
  ggplot(aes(date, total_impressions, group = creative_family)) + 
  geom_bar(stat = "identity") +  
  theme_bw() +
  labs(title = "Impressions over time by campaign proxy",
       x = "Date",
       y = "Total impressions in thousands (k)") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  facet_wrap(~creative_family)
imp_camp_time
# Care must be taken with this trend lines.  Although they look correct, further 
# analysis could be done to confirm by periodic aggregations and by audience 
# response within campaigns.




