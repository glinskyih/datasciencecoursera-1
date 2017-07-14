rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available")
  
  
  ## Check that state and outcome are valid
  if (is.element(as.factor(state) , data$State)==FALSE) {
    stop("invalid state")
  }
  
  validOutcomes <- c("heart failure", "heart attack", "pneumonia")
  if (sum(outcome == validOutcomes)==0) {
    stop("invalid outcome")
  }
  
  
  ## Isolate input state data
  stateRows <- data$State == state
  tmp <- data[stateRows,]
  dataForState <- tmp[order(tmp[,2]),]
  
  
  ## Get specific outcome data
  if (outcome == "heart failure") {
    mortality <- dataForState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  } else if (outcome == "heart attack"){
    mortality <- dataForState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  } else if (outcome == "pneumonia"){
    mortality <- dataForState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  
  ## simplify data frame, sort by increasing mortality, and add rank
  dataForState <- data.frame(dataForState$Hospital.Name,mortality)
  
  sortedDataForState <- dataForState[ order(dataForState[,2], na.last = NA, decreasing = FALSE), ]
  
  numHospsInState <- nrow(sortedDataForState)
  
  sortedDataForState$rank <- c(1:numHospsInState)
  
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if (is.character(num) && num == "best"){
    loc <- which.min(sortedDataForState$mortality)
    hosp <- as.character(sortedDataForState[loc,1])
    
  } else if (is.character(num) && num == "worst") {
    loc <- which.max(sortedDataForState$mortality)
    hosp <- as.character(sortedDataForState[loc,1])
    
  } else if (is.numeric(num) && num > numHospsInState) {
    hosp <- NA
    
  } else if (is.numeric(num) && num <= numHospsInState){
    loc <- which(sortedDataForState$rank == num)
    hosp <- as.character(sortedDataForState[loc,1])
  } 
    
  hosp
  
}