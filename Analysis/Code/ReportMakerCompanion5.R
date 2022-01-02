# NOTE: You will always want to run this code using the "Source" button.
# This is the easiest way for R to prompt the user in event of error (see below).

# The very first thing you want to do is download:
# 1) today's vaccination data file from: https://covid.cdc.gov/covid-data-tracker/#vaccinations
# 2) today's case/death/testing file from: https://covid.cdc.gov/covid-data-tracker/#cases_casesinlast7days
# 3) today's global ranking data from: https://globalepidemics.org/key-metrics-for-covid-suppression/
# 4) today's vaccination data from Colorado: https://drive.google.com/drive/folders/1r095ofG8YvNj_dMWEq4XKkfhDaF8-I0n
# 5) today's vaccination data from Louisiana: https://ladhh.maps.arcgis.com/apps/webappviewer/index.html?id=3b9b6f22d92f4d688f1c21e9d154cae2
# 6) today's Google data from: https://console.cloud.google.com/bigquery?project=covid-forecasting-272503&ws=!1m5!1m4!4m3!1scovid-forecasting-272503!2scovid_analyze!3sforecast_STATE_28_20210714!1m5!1m4!1m3!1scovid-forecasting-272503!2sbquxjob_246fb33a_17ab1d8a928!3sUS&j=bq:US:bquxjob_246fb33a_17ab1d8a928&page=queryresults
# 7) today's vaccination data from South Dakota (manual pull): https://doh.sd.gov/COVID/Dashboard.aspx

library(RCurl)
library(stringr)
library(openxlsx)

# Inputting today's date as a string for manipulation.
# This needs to be updated everyday.
# !!!Always use 2 digit days and months!!!
todaysDate <- "01/01/2022"

# Utility vectors of month abbreviations and days in each month
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Here, we assign days to the months above. Note this assumes no leap year. 
# If we are still dealing with the pandemic in 2024 we have bigger problems...
monthDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)

# Read in the current historical data tables (Update with your filepaths if not Ben)
histHosp <- read.csv("/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/hospDataHist.csv", stringsAsFactors = F)
histICU <- read.csv("/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/icuDataHist.csv", stringsAsFactors = F)
histPos <- read.csv("/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/posDataHist.csv", stringsAsFactors = F)
histDed <- read.csv("/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/dedDataHist.csv", stringsAsFactors = F)
histTot <- read.csv("/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/totDataHist.csv", stringsAsFactors = F)
histvacI <- read.csv("/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/vacInDataHist.csv", stringsAsFactors = F)
histvacC <- read.csv("/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/vacComDataHist.csv", stringsAsFactors = F)
histvacN <- read.csv("/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/vacNatHist.csv", stringsAsFactors = F)

# The historical data files above are EXTREMELY valuable. We want to make sure
# we do not overwrite these files, because if so, we will lost all of our data.

# Here, we create copies of data frames so if a bug results in an incorrect 
# overwrite of the saved historical files, the user can still write the original 
# DFs to csv as a way of restoring historical data. In a worst case scenario,
# the HHS hosp scraper in conjunction with python scraping code can be used to 
# rebuild dataset from scratch, but hat situation should be avoided. 
# Be sure to write these save files to csv if the daily report
# has issues and you find the original files were incorrectly changed.
histHospSave <- histHosp
histICUSave <- histICU
histPosSave <- histPos
histDedSave <- histDed
histTotSave <- histTot
histvacISave <- histvacI
histvacCSave <- histvacC
histvacNSave <- histvacN

# You will find the unaltered files (the ones with "Save" attached) in your environment.

# Extract the month, day, and year components from user provided date above.
# To do this, we select the starting and ending character to split.
month = as.integer(substr(todaysDate,1,2))
day = as.integer(substr(todaysDate,4,5))
year = as.integer(substr(todaysDate,7,10))

# For more on string manipulation, see here: https://www.youtube.com/watch?v=FyWG954OytA.

# Reformat today's date using column header format.
# Here, we are creating a specific date header, with month, date, and year.
# First, we paste the correct value of "month" from the "months" list above.
# Then, we paste the day. To paste the correct year, we divide by 1000 with no
# remainder. So, 2020/1000 = 20; 2021/1000 = 21, etc. We use a dash as a separator.
dateHeader <- paste(months[month], day, year%%1000, sep = "_")

# This code is structured to add one column to the dataset for each day it is run.
# So, on 5/07/2021 - a new column titled "5/07/2021" will be generated. On 5/08,
# this pattern repeats. However, if we run this code multiple times in the same
# day, the code's mechanism does not work (i.e. we run 5/07 when "5/07/2021"
# already exists). Here, we have defined a function which will warn the user
# if their date column already exists, and ask if they want to proceed.
# In almost all cases, you should select "No". This likely means you have not
# properly updated todaysDate at top, and should do so before rerunning the code.

# If today's date is already in the dataset, prompt user if they really want to proceed
if(colnames(histPos)[ncol(histPos)] == dateHeader) {
  cat("Warning, the listed date is already in the historical data set!")
  q = readline(prompt = "Are you sure you want to proceed?: ")
  if(!((q == "Y") | (q == "Yes") | (q == "yes"))){
    stop("Program stopped. Be careful not to add duplicate columns by running multiple times in a day")
  }
}

# Here, we read in the hospital data by pulling from the government URL.
hosp <- read.csv(text = getURL("https://healthdata.gov/resource/6xf2-c3ie.csv"), stringsAsFactors = F)

# You can learn more about scraping data from URLs here: https://www.youtube.com/watch?v=_36bjAoAN-o
# Note, we combine the two steps from the video into a single line of code (84).

  # Subset and rename columns of interest
  hosp <- hosp[,c("state","total_adult_patients_hospitalized_confirmed_and_suspected_covid",
                   "adult_icu_bed_utilization","reporting_cutoff_start")]
  
  colnames(hosp) <- c("state", "covHosp", "icuOcc", "date")
   
  # Convert the reported cutoff date to a column header format
  dateName <- paste(months[as.integer(substr(hosp$date[1],6,7))], 
                     as.integer(substr(hosp$date[1],9,10)),
                     as.integer(substr(hosp$date[1],1,4)) %% 1000, sep = "_")
  
  # If hospital data date is already in the dataset, we prompt the user if they
  # really want to proceed. Here, the user can choose to use the previous day's
  # data; quit the program; or add a column full of NAs. You probably just 
  # want to answer yes, and replicate yesterday's data.
  
  # First, resetting our question variable from before.
  q <- "temp"
  # Now, setting up the appropriate conditional prompt.
  if(dateName %in% colnames(histHosp)) {
     cat("Warning, the listed date is already in the historical hospital data set!")
     cat("\nType 'Y' to proceed and copy yesterday's data (preferred), 'Q' to quit, or 'NA' to add a day of NAs")
     q = readline(prompt = "Are you sure you want to proceed?: ")
     if(!(q == "Y" | q == "NA")){
       stop("Program stopped. Be careful not to add duplicate columns by running multiple times in a day")
     }

     if(q == "NA"){
      
       # If you believe there are errors with the hospital data on a given day,
       # or it is not available. You can enter "NA" into the prompt above, and 
       # a column with missing values will be generated.
       
       # Add new NA columns to dataframes
       histHosp$new <- NA
       histICU$new <- NA
       
       # Obtain previous day's date from column headers (note this is not the same as user entered date
       # because hospital data reported by HHS always has a cutoff date of a few days before reporting)
       yesterDate <- colnames(histHosp)[ncol(histHosp)-1]
       
       # Break date of last day of data into components, accounting for fact that days may be one or two characters
       dateLength <- nchar(yesterDate)
       yesterYear <- as.integer(substr(yesterDate,dateLength-1,dateLength))
       yesterDay <- as.integer(str_remove(substr(yesterDate,5,6), "_"))
       yesterMonth <- substr(yesterDate,1,3)
       
       # Move forward one day
       toDay <- yesterDay + 1
       toMonth <- yesterMonth
       monthInd = which(months == toMonth)
       toYear <- yesterYear
       
       # Making adjustments to month and year if necessary.
       
       # If "today" is greater than the number of days in a month (28-31), 
       # give it a value of 1. So, the "32nd" day of a month is just the 1st day
       # of the following month.
       if(toDay > monthDays[monthInd]){
         toDay = 1
         # If the month was previously 12 in the above, case we are now in a 
         # new calendar year, so we add 1 to our year vector, and convert the 
         # month to 1 (January).
         if(monthInd == 12){
           toMonth = months[1]
           toYear = toYear + 1
           # If not, we simply add one to our month vector.
         } else {
           toMonth = months[monthInd + 1]
         }
       }
       
       # Combine date components to create a singular date value.
       dateName <- paste(toMonth, toDay, toYear, sep = "_")
       
       # Rename NA columns with the appropriate date
       colnames(histHosp)[ncol(histHosp)] <- dateName
       colnames(histICU)[ncol(histICU)] <- dateName
     }
       
     # If the user decides to use the previous day's hospital data, we change
     # the date header to reflect this.
     if(q == "Y") {
       dateName <- paste(months[as.integer(substr(hosp$date[1],6,7))], 
                         as.integer(substr(hosp$date[1],9,10)) + 1,
                         as.integer(substr(hosp$date[1],1,4)) %% 1000, sep = "_")
     }
   }     
    
   # Merge new columns into main dataframes
   if(q == "temp"){
   histHosp <- merge(histHosp, hosp[,c("state", "covHosp")], by = "state", all = F)
   histICU <- merge(histICU, hosp[,c("state", "icuOcc")], by = "state", all = F)
   
   # Rename new columns with date headers
   colnames(histHosp)[ncol(histHosp)] <- dateName
   colnames(histICU)[ncol(histICU)] <- dateName
 }

  # Read in today's testing and case data files from downloads (change file path).
  # Must have downloaded today's testing data file from: https://covid.cdc.gov/covid-data-tracker/#testing_tests7day
  # and case/death file from: https://covid.cdc.gov/covid-data-tracker/#cases_casesinlast7days
  # Expand data tables below maps to access the download buttons.
  
  #### NOTE: after running this report each day you should delete the daily files 
  #### from downloads so it downloads tomorrow with the base name 
  #### rather than automatically appending "(1)" or something that would cause 
  #### this not to read in the most recent data.
  
  # If you forgot to delete your downloaded files from the previous day, you are
  # prompted to do so. (In this example, you will need to delete 
  # "covid19_tests_performed_by_state (1).csv" and "covid19_tests_performed_by_state.csv",
  # before redownloading the most recent data).

# Check for forgetting to delete documents
if("united_states_covid19_cases_deaths_and_testing_by_state (1).csv" %in% list.files("Downloads/")) {
  cat("You've downloaded multiple CDC data files with the same name.
      Be sure to delete yesterday's files before downloading today's.")
  stop()
}

  if("Risk Levels Downloadable Data (1).xlsx" %in% list.files("Downloads/")) {
    cat("You've downloaded multiple Global Epidemics files with the same name.
      Be sure to delete yesterday's files before downloading today's.")
    stop()
  }
  
  # Reading in the case and vaccination data. Change file paths as necessary.
  # We use "skip = 2" to remove the second row of the raw data before inputting.
  newCaseDeathTest <- read.csv("/Users/sameer_nair-desai/Downloads/united_states_covid19_cases_deaths_and_testing_by_state.csv", skip = 2, stringsAsFactors = F)
  newVaccines <- read.csv("/Users/sameer_nair-desai/Downloads/covid19_vaccinations_in_the_united_states.csv", skip = 3, stringsAsFactors = F)
  pop <- read.csv("/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/statePopulation.csv", stringsAsFactors = F)
  
  # Renaming problematic state.
  newCaseDeathTest$`State.Territory`[newCaseDeathTest$`State.Territory` == "New York*"] <- "New York"
  
  # Reading in the OWID data.
  OWID <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
  
  # Reading in the Global Epidemics data.
  risk_levels <- read.xlsx("/Users/sameer_nair-desai/Downloads/Risk Levels Downloadable Data.xlsx", sheet = "Country Risk Level")
  
  newData <- merge(newCaseDeathTest, newVaccines, by.x = "State.Territory", by.y = "State.Territory.Federal.Entity", all = T)
  
  # Convert State names to abbreviations
  colnames(newData)[1] <- "State"
  newData$State[newData$State == "District of Columbia"] <- "DC"
  
  # Merging population data by state.
  newData <- merge(newData, pop, by = "State", all = F)
  
  # Subsetting the data.
  newData <- newData[,c("abbr","Total.Cases", "Total.Deaths", "Total...Tests", 
                        "Percent.of.Total.Pop.with.at.least.One.Dose.by.State.of.Residence",
                        "Percent.of.Total.Pop.Fully.Vaccinated.by.State.of.Residence", 
                        "People.with.at.least.One.Dose.by.State.of.Residence",
                        "People.Fully.Vaccinated.by.State.of.Residence")]
  
  # Converting relevant variables to proper percentage formats.
  newData$Percent.of.Total.Pop.with.at.least.One.Dose.by.State.of.Residence <- as.numeric(newData$Percent.of.Total.Pop.with.at.least.One.Dose.by.State.of.Residence)/100
  newData$Percent.of.Total.Pop.Fully.Vaccinated.by.State.of.Residence <- as.numeric(newData$Percent.of.Total.Pop.Fully.Vaccinated.by.State.of.Residence)/100
  
  #Add columns to historical data
  histPos <- merge(histPos, newData[,c("abbr", "Total.Cases")], by.x = "state", by.y = "abbr")
  histDed <- merge(histDed, newData[,c("abbr", "Total.Deaths")], by = "state", by.y = "abbr")
  histTot <- merge(histTot, newData[,c("abbr", "Total...Tests")], by = "state", by.y = "abbr")
  histvacI <- merge(histvacI, newData[,c("abbr", "Percent.of.Total.Pop.with.at.least.One.Dose.by.State.of.Residence")], 
                    by = "state", by.y = "abbr")
  histvacC <- merge(histvacC, newData[,c("abbr", "Percent.of.Total.Pop.Fully.Vaccinated.by.State.of.Residence")], 
                    by = "state", by.y = "abbr")
  histvacN <- cbind(histvacN, c(NA, NA))
  histvacN[1,ncol(histvacN)] <- sum(newData$People.with.at.least.One.Dose.by.State.of.Residence, na.rm = T) / 328200000
  histvacN[2,ncol(histvacN)] <- sum(newData$People.Fully.Vaccinated.by.State.of.Residence, na.rm = T) / 328200000
  
  #Rename new columns
  colnames(histPos)[ncol(histPos)] <- dateHeader
  colnames(histDed)[ncol(histDed)] <- dateHeader
  colnames(histTot)[ncol(histTot)] <- dateHeader
  colnames(histvacI)[ncol(histvacI)] <- dateHeader
  colnames(histvacC)[ncol(histvacC)] <- dateHeader
  colnames(histvacN)[ncol(histvacN)] <- dateHeader
  
  #Remove non-numeric values
  histPos[, dateHeader] <- as.integer(histPos[, dateHeader])
  histDed[, dateHeader] <- as.integer(histDed[, dateHeader])
  histTot[, dateHeader] <- as.integer(histTot[, dateHeader])
  histvacI[, dateHeader] <- as.numeric(histvacI[, dateHeader])
  histvacC[, dateHeader] <- as.numeric(histvacC[, dateHeader])
  histvacN[, dateHeader] <- as.numeric(histvacN[, dateHeader])
  
  # Overwrite historical data tables with new column added. BE VERY CAREFUL here.
  # First, check the data frames to make sure there are no major errors.

  # Update each day.
  today <- Sys.Date()
  print(today)
  
  # Specifying today's date as a suffix for each file.
  file.paths <- paste("/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Temp/", today)
  file.paths <- gsub(" ", "", file.paths, fixed = TRUE)
  print(file.paths)
  
  # Writing our CSVs.
  write.csv(histHosp, paste(file.paths, "_hospDataHist.csv"), row.names = F)
  write.csv(histICU, paste(file.paths, "_icuDataHist.csv"), row.names = F)
  write.csv(histPos, paste(file.paths, "_posDataHist.csv"), row.names = F)
  write.csv(histDed, paste(file.paths, "_dedDataHist.csv"), row.names = F)
  write.csv(histTot, paste(file.paths, "_totDataHist.csv"), row.names = F)
  write.csv(histvacI, paste(file.paths, "_vacInDataHist.csv"), row.names = F)
  write.csv(histvacC, paste(file.paths, "_vacComDataHist.csv"), row.names = F)
  write.csv(histvacN, paste(file.paths, "_vacNatHist.csv"), row.names = F)
  
  # Deleting last week's file. This means we have the seven previous historical 
  # files in case of errors.
  lastWeek <- (as.Date(today) - 7)
  file.name <- paste(lastWeek, "_dedDataHist.csv")
  file.name <- gsub(" ", "", file.name, fixed = TRUE)
  print(file.name)
  
  # First, checking to make sure the file exists.
  file.exists(file.name)
  
  # Delete file path if it exists.
  file.remove(file.name)
  
  # Rewriting the files for reading in to the reportMakers.
  write.csv(histHosp, "/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/hospDataHist.csv", row.names = F)
  write.csv(histICU, "/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/icuDataHist.csv", row.names = F)
  write.csv(histPos, "/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/posDataHist.csv", row.names = F)
  write.csv(histDed, "/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/dedDataHist.csv", row.names = F)
  write.csv(histTot, "/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/totDataHist.csv", row.names = F)
  write.csv(histvacI, "/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/vacInDataHist.csv", row.names = F)
  write.csv(histvacC, "/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/vacComDataHist.csv", row.names = F)
  write.csv(histvacN, "/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Input/vacNatHist.csv", row.names = F)
  write.csv(OWID, "/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Global/Input/OWID.csv", row.names = F)
  write.csv(risk_levels, "/Users/sameer_nair-desai/Desktop/KEY/Work/Brown_RA/LT/Daily_Updates/Global/Input/risk_levels.csv", row.names = F)

