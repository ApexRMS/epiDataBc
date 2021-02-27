# data-bc-cdc.R
# Downloads data from the BC CDC website for use in the SyncroSim epi Package

# Datasheets:
#   dataBcCdc_Input - optional user-specified download URL
#                   - copy of downloaded CSV filename and raw data
#                   - date and time of the download
#   epi_DataSummary - case data in summary format
#   epi_RunControl  - start and end dates of raw downloaded data

library(rsyncrosim)
library(tidyverse)
library(lubridate)
library(data.table)

# Get the environment 
env <- ssimEnvironment()
transferDir = env$TransferDirectory

# Get the scenario that is currently being run
myScenario <- scenario()

# Get the required datasheets
inputSheet <- datasheet(myScenario, "dataBcCdc_Input")
inputSheet <- transform(inputSheet, DownloadDateTime = as.character(DownloadDateTime))
summarySheet <- datasheet(myScenario, "epi_DataSummary", empty = T)
runControl <- datasheet(myScenario, "epi_RunControl", empty = T)

# Download the raw data and process
downloadUrl <- inputSheet$RegionalSummaryDataURL
if (length(downloadUrl) == 0) stop("URL Missing.")
regionalSummaryData <- read.csv(downloadUrl)
csvFileName <- paste(transferDir, "RegionalSummaryData.csv", sep = "/")
write.csv(regionalSummaryData, csvFileName)

# Save input parameters used
inputSheet[1,] <-NA
inputSheet$RegionalSummaryDataURL <- downloadUrl
inputSheet$RegionalSummaryDataFile <- csvFileName
inputSheet$DownloadDateTime = as.character(Sys.time())
saveDatasheet(myScenario, inputSheet, "dataBcCdc_Input")

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

summarySheet[nrow(cases),] <- NA
summarySheet$Timestep <- cases$date
summarySheet$Variable <- "Cases"
summarySheet$Jurisdiction <- "Canada - British Columbia"
summarySheet$AgeMin <- NULL
summarySheet$AgeMax <- NULL
summarySheet$Sex <- NULL
summarySheet$Value <- cases$value

saveDatasheet(myScenario, summarySheet, "epi_DataSummary")

# Save the start and end date of the raw data to Run Control datasheet

runControl[1,] <- NA
runControl$MinimumTimestep <- min(summarySheet$Timestep)
runControl$MaximumTimestep <- max(summarySheet$Timestep)

saveDatasheet(myScenario, runControl, name = "epi_RunControl")

