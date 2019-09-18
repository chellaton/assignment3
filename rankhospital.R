rankhospital <- function(state, outcome, rank="best")
{
  ## read outcome data
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  validStates <- data$State
  validStates <- unique(validStates)
  ## Check that state and outcome are valid
  ## print(validStates)
  if(!state %in% validStates) {
    stop("invalid state")
  }
  if(!tolower(outcome) %in% c('heart attack','heart failure','pneumonia')) {
    stop("invalid outcome")
  }
  
  ## return hospital name in state with given rank for 30-day mortality rate
  ##
  ## construct col name from input outcome string
  tmp <- unlist(strsplit(outcome,' '))
  tmp1 <- ""
  for (i in 1:length(tmp)) {
    tmp1 <- paste(tmp1, toupper(substr(tmp[i],1,1)), substr(tmp[i],2,nchar(tmp[i])),sep="") 
    if(i<length(tmp)) tmp1 <- paste(tmp1,".",sep="")
  }
  colname <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",tmp1,sep="")
  ## convert string to numeric and suppress na warning
  data[,colname] <- suppressWarnings (as.numeric(data[,colname]))
  ## create subset for state
  d1 <- subset(data, data$State==state)
  ## sort by outcome column
  cHospitalName <- "Hospital.Name"
  ## print(paste(cHospitalName, colname))
  d1sorted <- d1[order(d1[,colname], d1[,cHospitalName]),]
  ## print(tail(na.exclude(d1sorted[,colname])))
  nRow <- nrow(na.exclude(d1sorted))
  index <- 1
  ##	print(paste("number of rows", nRow, "rank", as.numeric(rank)))
  if(tolower(rank)=="best") index <- 1
  if(tolower(rank)=="worst") index <- nRow
  if(is.numeric(rank)) {
    index <- as.numeric(rank)
  if (index > nRow) stop ("NA")
  }
  ## print(paste("index:", index))
  datarow <- d1sorted[index, "Hospital.Name"]
datarow
}