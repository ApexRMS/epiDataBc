# data-bc-cdc.R
# Downloads data from the BC CDC website for use in the SyncroSim epi Package

library(rsyncrosim)
library(tidyverse)
library(lubridate)
library(data.table)

# Set the epi package project-scoped strings for jurisdiction and variables
jurisdictionBC <- "Canada - British Columbia"
casesDailyVar <- "Cases - Daily"
casesCumVar <- "Cases - Cumulative"
transformerName <- "Download BC CDC Data"

# Get the environment 
env <- ssimEnvironment()
transferDir = env$TransferDirectory

# Get the scenario that is currently being run
myScenario <- scenario()

# Add the required jurisdictions and variables to the SyncroSim project (will ignore if they exist already)
saveDatasheet(myScenario, data.frame(Name = jurisdictionBC), "epi_Jurisdiction")
saveDatasheet(myScenario, data.frame(Name = c(casesDailyVar, casesCumVar)), "epi_Variable")

# Get the required scenario-scoped input datasheet
inputSheet <- datasheet(myScenario, "epiDataBc_Settings")

# Download the raw data and process
downloadUrl <- inputSheet$RegionalSummaryDataURL
if (length(downloadUrl) == 0) stop("URL Missing.")
regionalSummaryData <- read.csv(downloadUrl)
csvFileName <- paste(transferDir, "Regional_Summary_Data.csv", sep = "/")
write.csv(regionalSummaryData, csvFileName)

outputSheet = data.frame(
  RegionalSummaryDataFile = csvFileName,
  DownloadDateTime = as.character(Sys.time()),
  MinimumTimestep = as.character(min(regionalSummaryData$Date)),
  MaximumTimestep = as.character(max(regionalSummaryData$Date))
)
saveDatasheet(myScenario, outputSheet, "epiDataBc_ResultsRaw")

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

summarySheet <- datasheet(myScenario, "epi_DataSummary", empty = T, optional=T)
summarySheet[nrow(cases),] <- NA
summarySheet$TransformerID <- transformerName
summarySheet$Timestep <- cases$date
summarySheet$Variable <- casesDailyVar
summarySheet$Jurisdiction <- jurisdictionBC
# summarySheet$AgeMin <- NA
# summarySheet$AgeMax <- NULL
# summarySheet$Sex <- NULL
summarySheet$Value <- cases$value

# Add cumulative cases
summarySheet <- mutate(summarySheet, Value = cumsum(Value)) %>%
  mutate(Variable = casesCumVar) %>%
  bind_rows(summarySheet)

saveDatasheet(myScenario, summarySheet, "epi_DataSummary")
