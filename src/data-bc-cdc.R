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
transformerName <- "BC COVID-19 Data: Download Cases from BC CDC"

# Get the environment 
env <- ssimEnvironment()
transferDir = env$TransferDirectory

# Get the scenario that is currently being run
myScenario <- scenario()

# Add the required jurisdictions and variables to the SyncroSim project (will ignore if they exist already)
saveDatasheet(myScenario, data.frame(Name = c(casesDailyVar, casesCumVar)), "epi_Variable")

# Get the required scenario-scoped input datasheet
inputSheet <- datasheet(myScenario, "epiDataBc_Settings")

# Download the raw data and process
downloadUrl <- "http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv"
if (length(downloadUrl) == 0) stop("URL Missing.")
regionalSummaryData <- read.csv(downloadUrl)
csvFileName <- paste(transferDir, "BC_CDC_Data.csv", sep = "/")
write.csv(regionalSummaryData, csvFileName)

outputSheet = data.frame(
  DataFile = csvFileName,
  DownloadDateTime = as.character(Sys.time()),
  MinimumTimestep = as.character(min(as.character(regionalSummaryData$Date))),
  MaximumTimestep = as.character(max(as.character(regionalSummaryData$Date)))
)
saveDatasheet(myScenario, outputSheet, "epiDataBc_ResultsRaw")

# Import the data into the DataSummary datasheet

# Choose which health regions to keep
if(inputSheet$Regions) {
  regionsToKeep <- regionalSummaryData$HA %>%
    unique() %>%
    str_subset("All", negate = T) %>%
    str_subset("Out of Canada", negate = T)
  saveDatasheet(myScenario, data.frame(Name = str_c(jurisdictionBC, " - ", regionsToKeep)), "epi_Jurisdiction")
} else {
  regionsToKeep <- "All"
  saveDatasheet(myScenario, data.frame(Name = jurisdictionBC), "epi_Jurisdiction")
}

# Filtering the downloaded case data. We'll take the combined data from all health authorities, with all
# cases occurring after Day0, and renaming the Date and Case_Reported columns as "date" and "value"
# respectively.
cases <- data.table(regionalSummaryData) %>%
  separate(col=Date, into=c("date"), sep=' ') %>%
  mutate_at(vars(date), as.IDate) %>%
  subset(HA %in% regionsToKeep & Province=="BC") %>%
  filter(HSDA == "All") %>%
  select(date, value = Cases_Reported, Jurisdiction = HA) %>%
  mutate(Jurisdiction = str_c(jurisdictionBC, " - ", Jurisdiction)) %>%
  dplyr::as_tibble()

summarySheet <- datasheet(myScenario, "epi_DataSummary", empty = T, optional=T)
summarySheet[nrow(cases),] <- NA
summarySheet$TransformerID <- transformerName
summarySheet$Timestep <- cases$date
summarySheet$Variable <- casesDailyVar
if(!inputSheet$Regions) {
  summarySheet$Jurisdiction <- jurisdictionBC
} else
  summarySheet$Jurisdiction <- cases$Jurisdiction
summarySheet$Value <- cases$value

# Add cumulative cases
summarySheetCumulative <- 
  summarySheet %>%
  group_by(Jurisdiction) %>%
  group_split %>%
  map(~ .x %>% mutate(Value = cumsum(Value))) %>%
  bind_rows %>%
  mutate(Variable = casesCumVar)

summarySheet <- bind_rows(summarySheet, summarySheetCumulative)
    
saveDatasheet(myScenario, summarySheet, "epi_DataSummary")
