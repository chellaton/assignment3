best <- function(state, outcome)
{
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
## Return hospital name in that state with lowest 30-day death rate 
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
    
## find hospital with lowest mortality rate & remove NAs
    d2 <- d1$Hospital.Name[d1[,colname]==min(d1[,colname],na.rm=TRUE)]
    d2 <- d2[!is.na(d2)]
    d2
}
