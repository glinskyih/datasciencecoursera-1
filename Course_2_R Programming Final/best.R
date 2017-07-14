best <- function(state,outcome){
  
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
  
  
  ## Isolate input State data
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
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  loc <- which.min(mortality)
  
  hosp <- as.character(dataForState$Hospital.Name[loc])
  
  hosp
  
}