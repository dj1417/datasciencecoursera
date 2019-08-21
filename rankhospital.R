rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv", header = TRUE)
  
  ## Check that state and outcome are valid
  
  if(!is.element(state, data[,7])) {
    stop("invalid state")
  }
  
  if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
  
  if(num == "best") {
    return(best(state, outcome))
  }
  
  if(num == "worst") {
    return(worst(state, outcome))
  }
  
  data <- data[data$State == state,]
  
  if(outcome == "heart attack") {
    data <- data[,c(2,11)]
  }
  
  if(outcome == "heart failure") {
    data <- data[,c(2,17)]
  }
  
  if(outcome == "pneumonia") {
    data <- data[,c(2,23)]
  }
  
  data <- data[data[,2] != "Not Available",]
  
  data[,2] <- as.numeric(as.character(data[,2]))
  
  data <- data[order(data[,2], data[,1]),]
  
  data$rank <- 1:nrow(data)
  
  data <- data[data$rank == num,]
  
  as.character(data[1,1])
  
}