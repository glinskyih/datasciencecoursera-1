rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",na.strings = "Not Available")
  
  ## Check that outcome are valid
  validOutcomes <- c("heart failure", "heart attack", "pneumonia")
  if (sum(outcome == validOutcomes)==0) {
    stop("invalid outcome")
  }
  
  ## Get specific outcome data
  if (outcome == "heart failure") {
    mortality <- data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  } else if (outcome == "heart attack"){
    mortality <- data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  } else if (outcome == "pneumonia"){
    mortality <- data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  
  
  ## Simplify data frame
  simpleData <- data.frame(data$State,data$Hospital.Name,mortality)
  colnames(simpleData) <- c("State",'Hospital.Name','Mortality')
  simpleData <- simpleData[order(simpleData[,1],simpleData[,2]),]
  
  
  ## For each state, find the hospital of the given rank
  stateNames = levels(simpleData$State)
  numStates = length(stateNames)
  
  hosp <- character(length(numStates))
  
  
  for (i in 1:numStates) {
    stateRows <- simpleData$State == stateNames[i]
    tmp <- simpleData[stateRows,]
    tmp <- tmp[order(tmp[,3],tmp[,2]),]
    tmp$rank <- c(1:nrow(tmp))
    
    if (is.character(num) && num == "best"){
      loc <- which.min(tmp$Mortality)
      hosp[i] <- as.character(tmp[loc,2])
      
    } else if (is.character(num) && num == "worst") {
      loc <- which.max(tmp$Mortality)
      hosp[i] <- as.character(tmp[loc,2])
      
    } else if (is.numeric(num) && num > nrow(tmp)) {
      hosp[i] <- NA
      
    } else if (is.numeric(num) && num <= nrow(tmp)){
      loc <- which(tmp$rank == num)
      hosp[i] <- as.character(tmp[loc,2])
    } 
    
  }
  
  
  ## Create output data frame
  rankedHospitals = data.frame(hospital = hosp, state = stateNames)
  
}