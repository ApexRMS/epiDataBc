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

env = ssimEnvironment()
myScenario = scenario()
runControl = datasheet(myScenario, "RunControl")
inputData = datasheet(myScenario, "dataBcCdc_Input")
downloadUrl <- inputData$RegionalSummaryDataURL
regionalSummaryData <- read.csv(downloadUrl)

#Save raw data
csvFileName = paste(env$TransferDirectory, "RegionalSummaryData.csv", sep = "/")
write.csv(regionalSummaryData, csvFileName)
outputSheet = datasheet(myScenario, "dataBcCdc_Output", empty = T)
outputSheet = addRow(outputSheet, csvFileName)
saveDatasheet(myScenario, outputSheet, "dataBcCdc_Output")

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

cases <- subset(cases, cases$date >= runControl$StartDate & cases$date <= runControl$EndDate)

summarySheet = datasheet(myScenario, "DataSummaryOutput", empty = T)
summarySheet[nrow(cases),] <- NA

summarySheet$Iteration = 1
summarySheet$Date = cases$date
summarySheet$Timestep = floor(difftime(summarySheet$Date, runControl$StartDate, units='days')) + 1
summarySheet$Variable = "Data-Cases"
summarySheet$Jurisdiction = "Canada - British Columbia"
summarySheet$AgeMin = NULL
summarySheet$AgeMax = NULL
summarySheet$Sex = NULL
summarySheet$Value = cases$value

saveDatasheet(myScenario, summarySheet, "DataSummaryOutput")