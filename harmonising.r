# This script aims to harmonise key unique values and use of syntax and 
# nomenclature across datasets.  All strings to be lowercase and words separated 
# by underscore. Abbreviated words can remain as capitals to aid understanding.

# This script is accompanied by its own tech report that describes the decisions
# made and outcomes: 'harmonising_datasets_technical_report' (word/pdf).

# Generated on R version 4.3.1 (2023) and RStudio (2023.06.2).


# Libraries deployed.
# install.packages(tidyverse)
library(tidyverse)
library(stringr)

# Working on creative database first.
creative_harmony <- read_csv("creative1.csv")
ggeneral_harmony <- read_csv("googlegeneral.csv")
ggoals_harmony <- read_csv("googlegoals.csv")
ganalytics_harmony <- read_csv("ganalytics.csv")
dem_harmony <- read_csv("dem.csv")

# List variables in creative dataset.
names(creative_harmony)
names(ggeneral_harmony)
names(ggoals_harmony)
names(ganalytics_harmony)
names(dem_harmony)

########################################## Harmonisation changes.
# Change formatting of column variables for ganalytics_harmony.
names(ganalytics_harmony) <- c("...1", "audience", "campaign", "date", 
                               "platform", "ad_format", "creative_family", 
                               "creative_version", "total_sessions", 
                               "days_away", "latest_report", "city")
names(ganalytics_harmony)
# Change this variable name in creative_harmony to match other datasets.
colnames(creative_harmony)[colnames(creative_harmony) == "days_from_max"] <-
  "days_away"
names(creative_harmony)
# Change variables names in dem_harmony below to match the other datasets.
names(dem_harmony) <- c("url", "campaign", "creative_size", "audience_num", 
                        "audience", "ad_format", "date", "creative_family", 
                        "creative_version", "platform", "company", 
                        "impressions", "clicks", "days_away", "latest_report")
names(dem_harmony)


# Check data types by variable.
glimpse(creative_harmony)
glimpse(ggeneral_harmony)
glimpse(ggoals_harmony)
glimpse(ganalytics_harmony)
glimpse(dem_harmony)


####################################### Harmonisation changes.
# Changes: audience will be changed to character value for harmony.
ggeneral_harmony$audience <- as.character(ggeneral_harmony$audience)
class(ggeneral_harmony$audience)
ggoals_harmony$audience <- as.character(ggoals_harmony$audience)
class(ggoals_harmony$audience)
ganalytics_harmony$audience <- as.character(ganalytics_harmony$audience)
class(ganalytics_harmony$audience)
dem_harmony$audience <- as.character(dem_harmony$audience)
class(dem_harmony$audience)
creative_harmony$audience <- as.character(creative_harmony$audience)
class(creative_harmony$audience)
# dem_harmony$date <- as.Date(dem_harmony$date)
# class(dem_harmony$date)
# We learnt that data type changes do not persist on csv exports.

# Check unique values in shared variable columns: creative_family, platform, 
# ad_format, creative_size, audience, creative_version.
# We are no longer using 'campaign' as advised by A3, and for now likely leaving 
# creative_size aside, as we think unlikely to add much.  
unique(creative_harmony$ad_format)
unique(creative_harmony$creative_family)
unique(creative_harmony$platform)
unique(creative_harmony$audience)
unique(creative_harmony$creative_version)
unique(creative_harmony$creative_size)

unique(ggeneral_harmony$ad_format)
unique(ggeneral_harmony$creative_family)
unique(ggeneral_harmony$platform)
unique(ggeneral_harmony$audience)
unique(ggeneral_harmony$creative_version)
# There is no creative_size variable.

unique(ggoals_harmony$ad_format)
unique(ggoals_harmony$creative_family)
unique(ggoals_harmony$platform)
unique(ggoals_harmony$audience)
unique(ggoals_harmony$creative_version)
unique(ggoals_harmony$creative_size)
# There is no creative_size variable.

unique(ganalytics_harmony$ad_format)
unique(ganalytics_harmony$creative_family)
unique(ganalytics_harmony$platform)
unique(ganalytics_harmony$audience)
unique(ganalytics_harmony$creative_version)
unique(ganalytics_harmony$creative_size)
# There is no creative_size variable.

unique(dem_harmony$ad_format)
unique(dem_harmony$creative_family)
unique(dem_harmony$platform)
unique(dem_harmony$audience)
unique(dem_harmony$creative_version)
unique(dem_harmony$creative_size)

############################################### Harmonisation changes.
# Replace empty cells with NA.  Creative_harmony has empty cells.
creative_harmony$ad_format <- ifelse(creative_harmony$ad_format == "", 
                                     NA, creative_harmony$ad_format)
unique(creative_harmony$ad_format)
# In retrospect, we think this doesn't correct the csv exported and then 
# re-imported in any programme (unless used within this same script).

############################################### Harmonising changes(ad_format)
# All values repeated across datasets (same spelling) except the following:
# TV, Mobile, Tablet, Desktop, Nmn_partner_insight_1, Nmn_partner_insight_2
# which appeared in one dataset only.  The first four in creative_harmony and 
# the last two in Ggeneral_harmony.  See other dataset specific values in 
# dem_harmony. 

# Check how many values:
length(which(creative_harmony$ad_format == "TV"))
# 6806 values.
length(which(creative_harmony$ad_format == "Mobile"))
# 2776 values.
length(which(creative_harmony$ad_format == "Tablet"))
# 1912 values.
length(which(creative_harmony$ad_format == "Desktop"))
# 383 values.

# Note there are no similar looking values that might be misspelt.
# Check how many values:
length(which(ggeneral_harmony$ad_format == "Nmn_partner_insight_1"))
# Only three repeated values.
length(which(ggeneral_harmony$ad_format == "Nmn_partner_insight_2"))
# Only 5 repeated values.
length(which(ggeneral_harmony$ad_format == "Nmn"))
# 24 values.

# Check how many values:
length(which(dem_harmony$ad_format == "No lock campaign"))
# 1424 values.
length(which(dem_harmony$ad_format == "Interactive"))
# 6 values.
length(which(dem_harmony$ad_format == "Dsc"))
# 582 values.
length(which(dem_harmony$ad_format == "Banner"))
# 819 values.
length(which(dem_harmony$ad_format == "Remarketing"))
# 4094 values.  

# Consensus lead harmonisation decision to remove single and double digit 
# values, to better focus on important data.  Programming and visualisation can 
# have better visualisations and workarounds.
ggeneral_harmony$ad_format <- ifelse(ggeneral_harmony$ad_format == 
                                       "Nmn_partner_insight_1", NA, 
                                     ggeneral_harmony$ad_format)
length(which(ggeneral_harmony$ad_format == "Nmn_partner_insight_1"))
# confirmed 0 matching values.

ggeneral_harmony$ad_format <- ifelse(ggeneral_harmony$ad_format == 
                                       "Nmn_partner_insight_2", NA, 
                                     ggeneral_harmony$ad_format)
length(which(ggeneral_harmony$ad_format == "Nmn_partner_insight_2"))
# confirmed 0 matching values.

ggeneral_harmony$ad_format <- ifelse(ggeneral_harmony$ad_format == 
                                       "Nmn", NA, 
                                     ggeneral_harmony$ad_format)
length(which(ggeneral_harmony$ad_format == "Nmn"))
# confirmed 0 matching values.

dem_harmony$ad_format <- ifelse(dem_harmony$ad_format == 
                                       "Interactive", NA, 
                                     dem_harmony$ad_format)
length(which(dem_harmony$ad_format == "Interactive"))
# confirmed 0 matching values.

########################################### Harmonising changes(creative_family)
# These are values being checked that only appear in one datatset, to be 
# checked to confirm should be removed.

# Check how many values:
length(which(ggoals_harmony$creative_family == "SnapdocsLive"))
# 3 values.
length(which(ggoals_harmony$creative_family == "ComingSoon"))
# 1 value.
length(which(ggoals_harmony$creative_family == "219526440"))
# 1 value.
length(which(ggoals_harmony$creative_family == "domain"))
# 16 values.
length(which(ggoals_harmony$creative_family == "All3"))
# 1 value.
length(which(ggoals_harmony$creative_family == "08-24-2022"))
# 2 values.
length(which(ggoals_harmony$creative_family == "08-25-2022"))
# 1 value.
length(which(ggoals_harmony$creative_family == "08-29-2022"))
# 4 values.
length(which(ggoals_harmony$creative_family == "August"))
# 16 values.
length(which(ggoals_harmony$creative_family == "crm"))
# 3 values.
length(which(ggoals_harmony$creative_family == "09-06-2022"))
# 1 value.
length(which(ggoals_harmony$creative_family == "09-08-2022"))
# 1 value.


# The values below only appear in one dataset. The values are too small to 
# mean anything, but add several unnecessary categories that complicate 
# visualisations and possibly models.  Consensus agreement to convert these 
# values into NAs.
# Note there are no similar looking values that might be misspelt.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                  "SnapdocsLive", NA, 
                                ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "SnapdocsLive"))
# Confirmed 0 matching values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "ComingSoon", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "ComingSoon"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "219526440", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "219526440"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "domain", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "domain"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "All3", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "All3"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "08-24-2022", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "08-24-2022"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "08-25-2022", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "08-25-2022"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "08-29-2022", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "08-29-2022"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "August", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "August"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "crm", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "crm"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "09-06-2022", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "09-06-2022"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "09-08-2022", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "09-08-2022"))
# Confirmed 0 such values.

ggoals_harmony$creative_family <- ifelse(ggoals_harmony$creative_family == 
                                           "(not set)", NA, 
                                         ggoals_harmony$creative_family)
length(which(ggoals_harmony$creative_family == "(not set)"))
# Confirmed 0 such values.

########################################## Harmonising changes(creative_version)
# These are various versions of 'one page' that are being made uniform across 
# all datasets.

creative_harmony$creative_version <- 
  ifelse(creative_harmony$creative_version == "1Page", "one_page", 
         creative_harmony$creative_version)

ggeneral_harmony$creative_version <- 
  ifelse(ggeneral_harmony$creative_version == "1page", "one_page", 
         ggeneral_harmony$creative_version)
ggeneral_harmony$creative_version <- 
  ifelse(ggeneral_harmony$creative_version == "OnePage", "one_page", 
         ggeneral_harmony$creative_version)
ggeneral_harmony$creative_version <- 
  ifelse(ggeneral_harmony$creative_version == "One Page", "one_page", 
         ggeneral_harmony$creative_version)

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "1page", "one_page", 
         ggoals_harmony$creative_version)
ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "OnePage", "one_page", 
         ggoals_harmony$creative_version)

ganalytics_harmony$creative_version <- 
  ifelse(ganalytics_harmony$creative_version == "1page", "one_page", 
         ganalytics_harmony$creative_version)
ganalytics_harmony$creative_version <- 
  ifelse(ganalytics_harmony$creative_version == "OnePage", "one_page", 
         ganalytics_harmony$creative_version)

dem_harmony$creative_version <- 
  ifelse(dem_harmony$creative_version == "1page", "one_page", 
         dem_harmony$creative_version)
dem_harmony$creative_version <- 
  ifelse(dem_harmony$creative_version == "OnePage", "one_page", 
         dem_harmony$creative_version)



########################################## Harmonising changes(creative_version)
# Change '(not set)' to NA as they don't add value, confuse correlation 
# information and we can work around NA values.

ggeneral_harmony$creative_version <- 
  ifelse(ggeneral_harmony$creative_version == "(not set)", NA, 
         ggeneral_harmony$creative_version)

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "(not set)", NA, 
         ggoals_harmony$creative_version)

dem_harmony$creative_version <- 
  ifelse(dem_harmony$creative_version == "(not set)", NA, 
         dem_harmony$creative_version)

########################################## Harmonising changes(creative_version)
# Change 'Faster All' to the majority version across datasets, 'FasterAll'.

ggeneral_harmony$creative_version <- 
  ifelse(ggeneral_harmony$creative_version == "Faster All", "FasterAll", 
         ggeneral_harmony$creative_version)


########################################## Harmonising changes(creative_version)
# Count values that may be an aberration and only appear in one dataset, to 
# decide what to do.  These are the dates that also appear in creative_family, 
# same dataset.

length(which(ggoals_harmony$creative_version == "08-24-2022"))
# 2 values.
length(which(ggoals_harmony$creative_version == "08-25-2022"))
# 1 value.
length(which(ggoals_harmony$creative_version == "08-29-2022"))
# 4 values.
length(which(ggoals_harmony$creative_version == "09-06-2022"))
# 1 value.
length(which(ggoals_harmony$creative_version == "09-08-2022"))
# 1 value.
length(which(ggoals_harmony$creative_version == "219526440"))
# 1 value.

# Consensus to convert above set of syntax to NAs. 
# Note there are no similar looking values that might be misspelt.
# They add unnecessary issues to modelling and visualisation:

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "08-24-2022", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "08-24-2022"))
# Confirmed values removed.

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "08-25-2022", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "08-25-2022"))
# Confirmed values removed.

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "08-29-2022", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "08-29-2022"))
# Confirmed values removed.

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "09-06-2022", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "09-06-2022"))
# Confirmed values removed.

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "09-08-2022", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "09-08-2022"))
# Confirmed values removed.

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "219526440", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "219526440"))
# Confirmed values removed.


# The values below in dataset dem_harmony do not appear in any other dataset.

length(which(dem_harmony$creative_version == "273"))
# 2669 values.
length(which(dem_harmony$creative_version == "323"))
# 865 values.
length(which(dem_harmony$creative_version == "324"))
# 2463 values.
length(which(dem_harmony$creative_version == "311"))
# 1647 values.
length(which(dem_harmony$creative_version == "416"))
# 1352 values.
length(which(dem_harmony$creative_version == "337"))
# 1942 values.
length(which(dem_harmony$creative_version == "347"))
# 331 values.

# Likely to leave above as there are so many and may be significant,

# The values below in dataset creative_harmony do not appear in any other 
# dataset.

length(which(creative_harmony$creative_version == "Statement"))
# 123 values.
length(which(creative_harmony$creative_version == "v1"))
# 89 values.
length(which(creative_harmony$creative_version == "Digital"))
# 195 values.
length(which(creative_harmony$creative_version == "Static"))
# 5781 values.

# The values below in dataset ggeneral_harmony do not appear in any other 
# dataset.

length(which(ggeneral_harmony$creative_version == "ROS"))
# 1 value.  Consensus to convert to NA as adds no value and complicates 
# visualisations and modelling.
# Note there are no similar looking values that might be misspelt.

ggeneral_harmony$creative_version <- 
  ifelse(ggeneral_harmony$creative_version == "ROS", NA, 
         ggeneral_harmony$creative_version)
length(which(ggeneral_harmony$creative_version == "ROS"))
# Confirmed values removed.


# The values below in dataset ggoals_harmony do not appear in any other 
# dataset.

length(which(ggoals_harmony$creative_version == "One-Off"))
# 7 values.
length(which(ggoals_harmony$creative_version == "SnapdocsLive"))
# 3 values.
length(which(ggoals_harmony$creative_version == "ComingSoon"))
# 1 value.
length(which(ggoals_harmony$creative_version == "ad1"))
# 19 values.
length(which(ggoals_harmony$creative_version == "All3"))
# 1 value.

# Consensus to convert the above set of values to NA as they add no value and 
# complicate visualisation and modelling unnecessarily.  
# Note there are no similar looking values that might be misspelt.

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "One-Off", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "One-Off"))
# Confirmed values removed.

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "SnapdocsLive", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "SnapdocsLive"))
# Confirmed values removed.

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "ComingSoon", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "ComingSoon"))
# Confirmed values removed.

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "ad1", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "ad1"))
# Confirmed values removed.

ggoals_harmony$creative_version <- 
  ifelse(ggoals_harmony$creative_version == "All3", NA, 
         ggoals_harmony$creative_version)
length(which(ggoals_harmony$creative_version == "All3"))
# Confirmed values removed.


# The values below in dataset dem_harmony do not appear in any other 
# dataset.

length(which(dem_harmony$creative_version == "Animated - CloseMore"))
# 1366 values.

###################################################### Final checks.

# Check variable names across datasets:

names(creative_harmony)  
names(ggeneral_harmony)  
names(ggoals_harmony)
names(ganalytics_harmony)
names(dem_harmony)
# Confirm variable names match across datasets.

# Campaign variable has been left as last to agree consensus, because A3 advised
# they are unlikely to use this.  However, to confirm the difference in 
# unique values, please see below.  Also note that all data sets except ggeneral 
# include this variable. 

unique(creative_harmony$campaign)
unique(ggoals_harmony$campaign)
unique(ganalytics_harmony$campaign)
unique(dem_harmony$campaign)

# Last check.
glimpse(creative_harmony)
glimpse(ggeneral_harmony)
glimpse(ggoals_harmony)
glimpse(ganalytics_harmony)
glimpse(dem_harmony)

####################################################### Export data sets.

write_csv(creative_harmony, "creative_final.csv")
write_csv(ggoals_harmony, "ggoals_final.csv")
write_csv(ganalytics_harmony, "ganalytics_final.csv")
write_csv(ggeneral_harmony, "ggeneral_final.csv")
write_csv(dem_harmony, "dem_final.csv")


