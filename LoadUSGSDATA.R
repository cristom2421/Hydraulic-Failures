#LoadUSGS Peak FLow Data
#November 13, 2018

library(dataRetrieval)


SitesChar <- c("13015000", "03196800", "03155000", "03117500", "01611500", "01613900") 
# character vector of STAID to be analyzed

#pcode <- "62856"



data <- df.Fail.NBI.Gage[]
head(data$YR_FAIL_EST)

#PeakFlow 00060
rows <- c(1,2,3,4,5,6)
BeginDate <- sapply(rows, function(i) max(as.Date(as.numeric(df.Fail.NBI.Gage[i,"DATE_P_BEGIN_ALT_USGS"]),"1970-01-01"),df.Fail.NBI.Gage[i,"YR_BLT_EST"],na.rm = TRUE))

#BeginDate <- BeginDate[BeginDate != "-Inf"]

BeginDate <- as.Date(BeginDate,"%Y-%m-%d",origin="1970-01-01")
BeginDate <- sapply(BeginDate, function(y) substr(y,1,4))
BeginDate <- paste(BeginDate,"01","01",sep="-")

EndDate   <- sapply(rows, function(i) min(as.Date(as.numeric(df.Fail.NBI.Gage[i,"DATE_P_END_ALT_USGS"]),origin="1970-01-01"),df.Fail.NBI.Gage[i,"YR_FAIL"],na.rm=TRUE))

#EndDate <- EndDate[EndDate != "-Inf"]

EndDate   <- as.Date(as.numeric(EndDate),origin="1970-01-01")
EndDate   <- sapply(EndDate, function(y) substr(y,1,4))
EndDate   <- paste(EndDate,"12","31",sep="-")

ls.Discharge <- vector("list",length=length(SitesChar))

for (i in 1:length(SitesChar)){
  datpeak <- readNWISdata(siteNumbers = SitesChar[i], parameterCd = "00060",
                      startDate = BeginDate[i], endDate = EndDate[i],
                      service = "uv")
  
  ls.Discharge[[i]]$staid <- datpeak$staid
  ls.Discharge[[i]]$val   <- datpeak$val
  ls.Discharge[[i]]$dates <- datpeak$dates
}

#2 Instantaneous flow on day of failure, 00061

HasFailDate <- !is.na(df.Fail.NBI.Gage[rows,"YR_FAIL"])
NoFailDate  <- !HasFailDate
FailDate    <- character(length(rows))
FailDate[HasFailDate] <- df.Fail.NBI.Gage[rows[HasFailDate],"YR_FAIL"]

NoFailIndex  <- c(1:length(rows))[NoFailDate]
Dates <- lapply(ls.Discharge,"[[",3)
BeginFailYearIndex <- sapply(NoFailIndex, function(i) which.min(abs(Dates[[SitesChar[i]]]-df.Fail.NBI.Gage[rows[i],"YR_FAIL"])))
EndFailYearIndex <- sapply(NoFailIndex, function(i) which.min(abs(Dates[[SitesChar[i]]]-as.Date(paste(substr(df.Fail.NBI.Gage[rows[i],"YR_FAIL"],1,4),"12","31",sep="-")))))
FailDate[NoFailDate] <- sapply(1:length(BeginFailYearIndex), function(i) ls.Discharge[[NoFailIndex[i]]][which.max(ls.Discharge[[NoFailIndex[i]]]$val[BeginFailYearIndex[i]:EndFailYearIndex[i]])+BeginFailYearIndex[i],"dates"])
FailDate <- as.Date(as.numeric(FailDate),"1970-01-01")

for (i in 1:length(SitesChar)){
  dat <- readNWISdata(siteNumbers = SitesChar[i], parameterCd = "00060",
                          startDate = FailDate[i], endDate = FailDate[i],
                          service = "dv")
}

#head(data$DATE_P_BEGIN_ALT_USGS)
#head(data$YR_BLT_EST)
