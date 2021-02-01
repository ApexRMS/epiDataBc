# data-bc-cdc.R
# Download the case data from the BC CDC website

# Inputs:
#   Download URL
# Outputs:
#   regional_summary_data.csv - CSV file of the regional summary data in raw format
#   DataSummaryOutput - cases added to DataSummaryOutput datasheet

library(rsyncrosim)
library(tidyverse)
library(lubridate)
library(data.table)

# TODO: get the current scenario
# myScenario = scenario()  # Get the SyncroSim scenario that is currently running

# Download the raw data
# TODO: get this from the scenario's inputs
downloadUrl <- "http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv"


regionalSummaryData <- read.csv(downloadUrl)

# TODO: Use rsyncrosim to save the raw data to an output datasheet for this scenario
write.csv(regionalSummaryData, "regional_summary_data.csv")

# Import the data into DataSummaryInput datasheet
# Filtering the downloaded case data. We'll take the combined data from all health authorities, with all
# 	cases occurring after Day0, and renaming the Date and Case_Reported columns as "date" and "value"
# 	respectively.
cases <- data.table(regionalSummaryData) %>%
  separate(col=Date, into=c("date"), sep=' ') %>%
  mutate_at(vars(date), as.IDate) %>%
  subset(HA=="All" & Province=="BC") %>%
  select(date, Cases_Reported) %>%
  rename(value=Cases_Reported) %>%
  dplyr::as_tibble()

# TODO: Write the cases to the DataSummaryOutput datasheet
# Variable = "Cases"
# Jurisdiction = "Canada - British Columbia"






