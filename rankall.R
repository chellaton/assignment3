rankall <- function(outcome, num="best"){
  ## read csv file
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")

  ## check if input outcome value is valid
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
    if(!tolower(outcome) %in% c('heart attack','heart failure','pneumonia')) {
    stop("invalid outcome")
    }
  ## get list of states
  listStates <- unique(data$State)
  
  ## build column name for outcome
  tmp <- unlist(strsplit(outcome,' '))
  tmp1 <- ""
  for (i in 1:length(tmp)) {
    tmp1 <- paste(tmp1, toupper(substr(tmp[i],1,1)), substr(tmp[i],2,nchar(tmp[i])),sep="") 
    if(i<length(tmp)) tmp1 <- paste(tmp1,".",sep="")
  }
  colname <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",tmp1,sep="")
  data[,colname] <- suppressWarnings(as.numeric(data[,colname]))
  mylist <- data.frame(hospital="", state="")
  
  for (aState in listStates[order(listStates)]) {
    if(suppressWarnings(tolower(as.character(num))) == "best") index <- 1
    else if(tolower(as.character(num)) == "worst") index <- -1
    else if(is.numeric(num)) index <- as.numeric(num)
    stateData <- subset(data, data$State==aState)
    sortedData <- stateData[order(stateData[,colname], stateData[,"Hospital.Name"]),]
    rowMax <- length(na.exclude (sortedData[,colname]))
    if(index == -1) index <- rowMax
    if(index <= rowMax) {
      nameHospital <- sortedData[index,"Hospital.Name"]
    }
    else nameHospital <- "<NA>"
    mylist <- rbind(mylist, data.frame(hospital=nameHospital, state=aState))
  }
  
  mylist
  
}