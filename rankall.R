rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv", header = TRUE)
  
  ## Check that outcome is valid
  
  if(!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  ## Return a data frame with the hospital names and the (abbreviated state name)
  
  states <- as.character(unique(data[,7]))
  
  output <- data.frame(hospital=character(), state=character(), stringsAsFactors = FALSE)
  
  for (i in states) {
    hospital <- rankhospital(i, outcome, num)
    
    output[nrow(output) + 1,] = c(hospital, i)
    
  }
  
  output <- output[order(output[,2]),]
  
  rownames(output) <- output[,2]
  
  return(output)
  
}