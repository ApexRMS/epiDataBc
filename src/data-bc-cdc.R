# data-bc-cdc.R
# Downloads data from the BC CDC website for use in the SyncroSim epi Package

# Input Datasheets:
#   dataBcCdc_Input - optional user-specified download URL

# Output Datasheets:
#   dataBcCdc_Input - actual download URL
#   dataBcCdc_Output - copy of downloaded CSV filename and raw data
#   epi_DataSummary - case data in summary format
#   epi_RunControl - start and end dates of raw downloaded data

library(rsyncrosim)
library(tidyverse)
library(lubridate)
library(data.table)

# Get the scenario that is currently being run
env <- ssimEnvironment()
myScenario <- scenario()

# Retrieve the download URL (use default if not specified)
inputSheet <- datasheet(myScenario, "dataBcCdc_Input")
downloadUrl <- inputSheet$RegionalSummaryDataURL
if (length(downloadUrl) == 0) downloadUrl <- "http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv"

# Download the raw data
regionalSummaryData <- read.csv(downloadUrl)

# Save Url actually used
inputSheet[1,] <-NA
inputSheet$RegionalSummaryDataURL <- downloadUrl
saveDatasheet(myScenario, inputSheet, "dataBcCdc_Input")

# Save a copy of the raw data to CSV
csvFileName <- paste(env$TransferDirectory, "RegionalSummaryData.csv", sep = "/")
write.csv(regionalSummaryData, csvFileName)
outputSheet <- datasheet(myScenario, "dataBcCdc_Output", empty = T)
outputSheet <- transform(outputSheet, DownloadDateTime = as.character(DownloadDateTime))

# Save the filename of the raw data CSV
outputSheet <- addRow(outputSheet, c(csvFileName, as.character(Sys.time())))
saveDatasheet(myScenario, outputSheet, "dataBcCdc_Output")

# Import the data into the DataSummary datasheet

# Filtering the downloaded case data. We'll take the combined data from all health authorities, with all
# cases occurring after Day0, and renaming the Date and Case_Reported columns as "date" and "value"
# respectively.
cases <- data.table(regionalSummaryData) %>%
  separate(col=Date, into=c("date"), sep=' ') %>%
  mutate_at(vars(date), as.IDate) %>%
  subset(HA=="All" & Province=="BC") %>%
  select(date, Cases_Reported) %>%
  rename(value=Cases_Reported) %>%
  dplyr::as_tibble()

summarySheet <- datasheet(myScenario, "epi_DataSummary", empty = T)
summarySheet[nrow(cases),] <- NA

summarySheet$Iteration <- 1
summarySheet$Timestep <- cases$date
summarySheet$Variable <- "Cases"
summarySheet$Jurisdiction <- "Canada - British Columbia"
summarySheet$AgeMin <- NULL
summarySheet$AgeMax <- NULL
summarySheet$Sex <- NULL
summarySheet$Value <- cases$value

saveDatasheet(myScenario, summarySheet, "epi_DataSummary")

# Save the start and end date of the raw data to Run Control datasheet
runControl <- datasheet(myScenario, "epi_RunControl", empty = T)
runControl[1,] <- NA
runControl$MinimumTimestep <- min(summarySheet$Timestep)
runControl$MaximumTimestep <- max(summarySheet$Timestep)
runControl$MinimumIteration <- 1
runControl$MaximumIteration <- 1

saveDatasheet(myScenario, runControl, name = "epi_RunControl")
