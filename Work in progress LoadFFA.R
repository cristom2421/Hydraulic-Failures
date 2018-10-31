# Function to load flood frequency analysis results from output file
# Written by Madeleine Flint, 2018-06-27
# Supports HEC-SSP v2? Bulletin 17B analysis, PeakFQ v4.2 B17B, and PeakFQ v7.2 B17C
LoadFFA <- function(filename, folder.out, TYPE = "PeakFQ_17C",
                    colStandard = TRUE){ # standardize column output names
  
  ls.colNames <- list("PEAKFQSA" = c("Ann. Exc. Prob.", "EMA Est.", "V[log(EMA)]","At-Site Est.","CI-Low","CI-High"),
                      "PKFQ72_17C" = c("ANN_EXC_PROB",  "EMA_REG",	"EMA_STAT",	"LOG_VAR_REG",	"5PCT_CONF_REG",	"95PCT_CONF_REG"),
                      "HEC_17B"    = c("Ordinate",  "FREQ",	"FLOW Computed Curve",	"FLOW Expected Prob Curve",	"FLOW 5Pct Conf",	"FLOW 95Pct Conf",	
                                       "FLOW Observed (Median)",	"FLOW Historic Data",	"FLOW High Outlier",	"FLOW Low Outlier"),
                      "PKFQ42_17B" = c("ANN_EXC_PROB",  "BULL17B_EST",	"SYSTEM_RECORD",	"EXPECTED_PROB",	"5PCT_CONF",	"95PCT_CONF"),
                      "PARTDUR"    = c("Val","Exc"))
  colNames <- ls.colNames[TYPE]
  
  colsSwitch <- list(FlowCols   = c("EMA_REG", "FLOW Computed Curve", "BULL17B_EST","Val","EMA Est."),
                     FreqCols   = c("Ann. Exc. Prob.","ANN_EXC_PROB","FREQ","Exc"),
                     Flow05Cols = c("5PCT_CONF_REG", "5PCT_CONF","CI-Low"),
                     Flow95Cols = c("95PCT_CONF_REG", "95PCT_CONF","CI-High"))
  
  colsOut <- c(FlowCols   = "Flow",
               FreqCols   = "Freq",
               Flow05Cols = c("Flow05"),
               Flow95Cols = c("Flow95"))
  
fileinput<-file.path(folder.out,filename)
fileinput<c(fileinput)

require(data.table)
  if(grepl("PEAKFQSA",TYPE)){ # PeakfqSA Bulleting 17C analysis
    text.list<-lapply(fileinput,readLines)
    skip.rows<-sapply(text.list, grep, pattern = '^Ann. Exc. Prob.\\s+EMA Est.\\s+EMA Est.') - 1
    
    PFA<-lapply(seq_along(text.list),function(i) fread(fileinput[i],skip=skip.rows[i]))
  }   
}
    # PFA <- read.table(file.path(folder.out,filename),
    #                   header=FALSE,na.strings="NA",sep = "\t",
    #                   skip=skip, col.names = colNames,
    #                   stringsAsFactors = FALSE)}
    
# Now setup column names
# colnames(PFA) <- colNames
# colClasses    <- names(ColsSwitch)
# if(colStandard){
#   for(col in colClasses){
#     colsIn <- unlist(colsSwitch[[col]])
#     colnames(PFA)[sapply(colNames, function(c) any(grepl(c, colsIn)))] <- colsOut[col]
#   }
#   PFA <- PFA[,colnames(PFA) %in% colsOut]
# }
# return(PFA)
# }