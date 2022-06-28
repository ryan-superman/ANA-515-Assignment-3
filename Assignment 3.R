library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

##1
getwd()
list.files()
setwd("/Users/rsuparma/Documents/OneDrive/Documents/2021 Doc/McDaniel College/School/ANA 515 - Summer 2022/Week 6")
storm_data <- read_csv("stormeventscsv.csv")


##2 
head(storm_data, 6)

#2.2 Limit the data frame to the following columns.
columnfilter <- c("BEGIN_YEARMONTH", "BEGIN_DAY", "BEGIN_TIME", "END_YEARMONTH", "END_DAY", "END_TIME", "BEGIN_DATE_TIME", "END_DATE_TIME",
                  "EPISODE_ID", "EVENT_ID", "STATE", "STATE_FIPS", "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE", "SOURCE", "BEGIN_LAT",
                  "BEGIN_LON", "END_LAT", "END_LON")


storm_data_filtered <- storm_data[columnfilter]
head(storm_data_filtered)

#2.3 Arrange the data by the state name (STATE) 
storm_data_filtered <- arrange(storm_data_filtered, STATE)

#2.4 Change state and county names to title case (e.g., “New Jersey” instead of “NEW JERSEY”) 
storm_data_filtered$STATE <- str_to_title(storm_data_filtered$STATE)

#2.5	Limit to the events listed by county FIPS (CZ_TYPE of “C”) and then remove the CZ_TYPE column 
storm_data_filtered <- filter(storm_data_filtered, CZ_TYPE == "C")
storm_data_filtered <- select(storm_data_filtered, -CZ_TYPE)

#2.6	Pad the state and county FIPS with a “0” at the beginning (hint: there’s a function in stringr to do this) 
# and then unite the two columns to make one fips column with the 5 or 6-digit county FIPS code 
storm_data_filtered$STATE_FIPS <- str_pad(storm_data_filtered$STATE_FIPS, width = 3, side = "left", pad = "0")
storm_data_filtered$CZ_FIPS <- str_pad(storm_data_filtered$CZ_FIPS, width = 3, side = "left", pad = "0")
head(storm_data_filtered)

storm_data_filtered <- unite(storm_data_filtered, "fips", c("STATE_FIPS", "CZ_FIPS"))
storm_data_filtered$fips

#2.7	Change all the column names to lower case (you may want to try the rename_all function for this) 
storm_data_filtered <- rename_all(storm_data_filtered, tolower)

#2.8	There is data that comes with base R on U.S. states (data("state")). Use that to create a dataframe with these three columns: state name, area, and region 
data("state")
us_state_info <- data.frame(state = state.name, region = state.region, area = state.area)

#2.9 Create a dataframe with the number of events per state in the year of your birth. 
#Merge in the state information dataframe you just created in step 8. 
#Remove any states that are not in the state information dataframe
newset <- data.frame(table(storm_data_filtered$state))
head(newset)

newset1 <- rename(newset, c("state" = "Var1"))
new_merged <- merge(x = newset1, y = us_state_info, by.x = "state", by.y = "state")
head(new_merged)
view(new_merged)

#2.10	Create the following plot 
storm_plot <- ggplot(new_merged, aes(x = area, y = Freq)) + geom_point(aes(color = region)) + labs (x = "Land Area (Square Miles)", y = "# of storm events in 1996")
storm_plot
