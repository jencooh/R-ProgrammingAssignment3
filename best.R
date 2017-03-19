##best: 2 arguments
##1) read outcome data
##2) check state and outcome are valid
##3) Returns a character vector with the name of the hospital that has the best(lowest) 30-day mortality for the specified outcome in that state.
##4) hospital name = name in Hospital.Name variable
best <- function(state, outcome) {
    ##1) read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
    data[, Column] <- suppressWarnings(as.numeric(data[, Column]))
    data <- na.omit(data)
    Statedata <- data[data$State==state, ]
    ##3) Returns a character vector with the name of the hospital with the lowest outcome
    min <- min(Statedata[, Column])
    lowest <- which(Statedata[, Column] == min)
    hospital <- Statedata[lowest, 2]
    ##if tie, then first on alphabetical order chosen
    if (is.list(hospital)){ hospital <- sort(hospital)
        hospital1 <- hospital[1]}
        else {hospital1 <- hospital}
    return(hospital1)
}

