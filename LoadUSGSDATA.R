#LoadUSGS Peak FLow Data
#November 13, 2018

library(dataRetrieval)


SitesChar <- c("13015000", "03196800", "03155000", "03117500", "01611500", "01613900") 
# character vector of STAID to be analyzed

#pcode <- "62856"

data <- df.Fail.NBI.Gage[]
head(data$STAID)

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
  dat <- importDVs(SitesChar[i],code="00060",stat="00003",sdate=BeginDate[i],edate=EndDate[i])

  ls.Discharge[[i]]$staid <- dat$staid
  ls.Discharge[[i]]$val   <- dat$val
  ls.Discharge[[i]]$dates <- dat$dates
}

BeginDate
EndDate


#head(data$DATE_P_BEGIN_ALT_USGS)
#head(data$YR_BLT_EST)
