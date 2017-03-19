##rankhospital
##(state, outcome, num: ranking of a hospital in that state for that outcome)
##1) read outcome data
##2) check that state and outcome are valid
##3) return hospital name in that state with the given rank
##30-day death rate
rankhospital <- function (state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ##best= 1st
    ##worst= last
    ##2) check state and outcome are valid
    StateList <- unique(data$State)
    OutcomeList <- c("heart attack", "heart failure", "pneumonia")
    if (!is.element(state, StateList)) {
        stop("invalid state")
    }
    if (!is.element(outcome, OutcomeList)) {
        stop("invalid outcome")
    } 
    Column <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    ##11, 17, 23
    ##3) return hospital name in that state with the given rank
    ##30-day death rate
    data[, Column] <- suppressWarnings(as.numeric(data[, Column]))
    data <- na.omit(data)
    Statedata <- data[data$State==state, ]
    Statedata <- Statedata[order(Statedata[, Column], Statedata[,2]), 2]
    num <- ifelse(num == "best", 1, ifelse(num == "worst", length(Statedata), as.numeric(num)))
    Statedata[num]
}
