# Week 4 Quiz

# Question 1
URL <-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
dldest <- "/Users/non-work/datasciencecoursera/GettingAndCleaningData/quiz4question1.csv"
data <- download.file(URL, dldest)

data <- read.csv(dldest)

dataColNames <- names(data)

wgtpNames <-strsplit(names(data),"[wgtp]")

wgtpNames[123]


# Question 2
library(plyr)
library(dplyr)
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
dldest <- "/Users/non-work/datasciencecoursera/GettingAndCleaningData/quiz4question2.csv"
data <- download.file(URL, dldest)

GDPdata <- tbl_df(read.csv(dldest, header = TRUE, skip = 4, na.strings = "..", 
                           sep = ",", nrows = 190, stringsAsFactors = FALSE))
GDPdata <- select(GDPdata, X,X.1,X.3,X.4)
GDPdata <- rename(GDPdata, CountryCode = X, rank = X.1, countryNames = X.3, US_Dollars = X.4)

dols <- trimws(GDPdata$US_Dollars)
dols <- gsub(",","",dols)

avgDOL <- mean(as.numeric(dols))



# Question 3
countryNames <- GDPdata$countryNames
Encoding(countryNames) <-"latin1"
numUnitedContries <- length(countryNames[grepl("^United",countryNames)])


# Question 4
# uses GDP data from Q2
# Find Fiscal year end in "special notes"

Encoding(GDPdata$countryNames) <- "latin1"

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
dldest <- "/Users/non-work/datasciencecoursera/GettingAndCleaningData/quiz4question4.csv"

data <- read.csv(URL)

matchedData <- merge(GDPdata, data, by = "CountryCode")

fiscalInfo <- as.character(matchedData$Special.Notes)
info <- fiscalInfo[fiscalInfo != '']

rexp <- "Fiscal year end(.*)[jJ]+une"
x <- grep(rexp,info, value = TRUE)
length(x)


# Question 5
#install.packages("quantmod")
library(quantmod)
library(lubridate)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

yr2012 <- sampleTimes[year(sampleTimes) == 2012]
length(yr2012)

mon2012 <- yr2012[wday(yr2012, label = TRUE) == "Mon"]
length(mon2012)


