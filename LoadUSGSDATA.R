#LoadUSGS Peak FLow Data
#November 13, 2018
library(dataRetrieval)


SitesChar <- "62856" # character vector of STAID to be analyzed

#pcode <- "62856"

data <- df.Fail.NBI.Gage[]

rowsforanalysis <- c(1,2,3,4,5,6)
BeginDate <- sapply(rowsforanalysis, function(i) max(as.Date(as.numeric(df.Fail.NBI.Gage[i,"DATE_P_BEGIN_ALT_USGS"]),"1970-01-01"),df.Fail.NBI.Gage[i,"YR_BLT_EST"],na.rm = TRUE))

BeginDate <- BeginDate[BeginDate != "-Inf"]

BeginDate <- as.Date(BeginDate,"%Y-%m-%d",origin="1970-01-01")
BeginDate <- sapply(BeginDate, function(y) substr(y,1,4))
BeginDate <- paste(BeginDate,"01","01",sep="-")

BeginDate
head(data$DATE_P_BEGIN_ALT_USGS)
head(data$YR_BLT_EST)
