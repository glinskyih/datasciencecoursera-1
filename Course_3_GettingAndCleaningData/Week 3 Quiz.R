library(dplyr)
library(jpeg)

# Quiz Question 1

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

data <- tbl_df(read.csv(fileURL))

agricultureLogical <- data$ACR ==3 & data$AGS==6

x <- which(agricultureLogical)

head(x, n=3)


# Quiz Question 2

jpegURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"

imageDestination = "~/datasciencecoursera/GettingAndCleaningData/jeff.jpg"

img <- download.file(jpegURL, destfile = imageDestination, mode = "wb")

Im <- readJPEG(imageDestination, native = TRUE)

res <- quantile(Im, c(0.3,0.8))


# Quiz Question 3
gdpURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
GDPdata <- tbl_df(read.csv(gdpURL, header = TRUE, skip = 4, na.strings = "..", 
                           sep = ",", nrows = 190, stringsAsFactors = FALSE))
GDPdata <- select(GDPdata, X,X.1,X.3,X.4)
GDPdata <- rename(GDPdata, CountryCode = X, rank = X.1, Economy = X.3, US_Dollars = X.4)

dols <- trimws(GDPdata$US_Dollars)
dols <- gsub(",","",dols)

GDPdata <- mutate(GDPdata, US_Dollars = as.numeric(dols))


eduURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
EDUdata <- tbl_df(read.csv(eduURL))


matches <- GDPdata$CountryCode %in% EDUdata$CountryCode
sum(matches)

combined <- merge(GDPdata, EDUdata, by = "CountryCode")

sorted_GDP <- arrange(combined, (US_Dollars))
sorted_GDP[13,"Economy"]


# Quiz Question 4
# High Income: OECD && High Income: nonOECD
incomegrpA <- filter(combined, Income.Group == "High income: OECD")
summarize(incomegrpA, mean(rank))

incomegrpB <- filter(combined, Income.Group == "High income: nonOECD")
summarize(incomegrpB, mean(rank))


# Quiz Question 5
quant <- quantile(combined$rank, probs = seq(0,1,0.2), na.rm = TRUE)
q <- table(cut(combined$rank, quant),combined$Income.Group)
q[1,"Lower middle income"]

