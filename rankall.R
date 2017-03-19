##rankall
##(outcome, num = "best)
##return data frame containing the hospital in each state that has the ranking specified in num
##1) read outcome data
##2) check that state and outcome are valid
##3) for each state, find the hospital of the given rank
##4) return a data frame with the hospital names and the state name
rankall <- function (outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ##best= 1st
    ##worst= last
    ##2) check state and outcome are valid
    StateList <- unique(data$State)
    OutcomeList <- c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, OutcomeList)) {
        stop("invalid outcome")
    } 
    Column <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    ##11, 17, 23
    ##3) for each state, find the hospital of the given rank
    data[, Column] <- suppressWarnings(as.numeric(data[, Column]))
    data <- na.omit(data)
    
    rank <- sapply(StateList, function(state) {
        Statedata <- data[data$State==state, ]
        Statedata <- Statedata[order(Statedata[,Column], Statedata[,2]), ]
        num <- ifelse(num == "best", 1, ifelse(num == "worst", nrow(Statedata), as.numeric(num)))
            return (c(state, Statedata[num,2]))
    })
    result <- data.frame(rank[2,], rank[1,]) # Insert values into DF
    colnames(result) <- c("Hospital", "State")
    rownames(result) <- result$State
    result <- result[order(result$State),]
    return(result)
}
