best <- function(state, outcome) {
  
  ## Read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv", header = TRUE)
  
  ## Check that state and outcome are valid
  
  if(!is.element(state, data[,7])) {
    stop("invalid state")
  }
  
  if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  data <- data[data$State == state,]
  
  if (outcome == "heart attack") {
    data <- data[,c(2,11)]
  }
  
  if (outcome == "heart failure" ) {
    data <- data[,c(2, 17)]
  }
  
  if (outcome == "pneumonia") {
    data <- data[,c(2,23)]
  }
  
  data <- data[data[,2] != "Not Available",]

  data[,2] <- as.numeric(as.character(data[,2]))
  
  data <- data[data[,2] == min(data[,2]),]
  
  data <- data[order(data[,1]),]
  
  as.character(data[1,1])
    
}