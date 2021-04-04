# data-bc-cdc.R
# Downloads data from the BC CDC website for use in the SyncroSim epi Package

# TODO:
# -Use Run Control to filter the download dates (if provided)

# Datasheets:
#   dataBcCdc_Inputs  - optional user-specified download URL
#                     - copy of downloaded CSV filename and raw data
#                     - date and time of the download
#   dataBcCdc_Outputs - raw data and download date/time
#   epi_Variable      - required variables for epi_DataSummary
#   epi_DataSummary   - case data in summary format

library(rsyncrosim)
library(tidyverse)
library(lubridate)
library(data.table)

# Set the epi package project-scoped strings for jurisdiction and variables
jurisdictionBC <- "Canada - British Columbia"
casesDailyVar <- "Cases - Daily"
casesCumVar <- "Cases - Cumulative"

# Get the environment 
env <- ssimEnvironment()
transferDir = env$TransferDirectory

# Get the scenario that is currently being run
myScenario <- scenario()

# Add the required jurisdictions and variables to the SyncroSim project (will ignore if they exist already)
saveDatasheet(myScenario, data.frame(Name = jurisdictionBC), "epi_Jurisdiction")
saveDatasheet(myScenario, data.frame(Name = c(casesDailyVar, casesCumVar)), "epi_Variable")

# Get the required scenario-scoped datasheets
inputSheet <- datasheet(myScenario, "dataBcCdc_Inputs")
outputSheet <- datasheet(myScenario, "dataBcCdc_Outputs")
outputSheet <- transform(outputSheet, DownloadDateTime = as.character(DownloadDateTime))
summarySheet <- datasheet(myScenario, "epi_DataSummary", empty = T)

# Download the raw data and process
downloadUrl <- inputSheet$RegionalSummaryDataURL
if (length(downloadUrl) == 0) stop("URL Missing.")
regionalSummaryData <- read.csv(downloadUrl)
csvFileName <- paste(transferDir, "Regional_Summary_Data.csv", sep = "/")
write.csv(regionalSummaryData, csvFileName)

# Save input parameters used
outputSheet[1,] <-NA
# inputSheet$RegionalSummaryDataURL <- downloadUrl
outputSheet$RegionalSummaryDataFile <- csvFileName
outputSheet$DownloadDateTime = as.character(Sys.time())
saveDatasheet(myScenario, outputSheet, "dataBcCdc_Outputs")

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
summarySheet$TransformerID="BC CDC: Download data"
summarySheet$Timestep <- cases$date
summarySheet$Variable <- casesDailyVar
summarySheet$Jurisdiction <- jurisdictionBC
summarySheet$AgeMin <- NULL
summarySheet$AgeMax <- NULL
summarySheet$Sex <- NULL
summarySheet$Value <- cases$value

# Add cumulative cases
summarySheet <- mutate(summarySheet, Value = cumsum(Value)) %>%
  mutate(Variable = casesCumVar) %>%
  bind_rows(summarySheet)

saveDatasheet(myScenario, summarySheet, "epi_DataSummary")
