rankall <- function(outcome, num = "best") {
    mn <- NA
    RetData <- c()
    Hospital <- c()
    State <- c()
    
    'in case of best/worst.. only 1 hospital is returned'
    if(num == "best")
        num = 1
    else if(num == "worst")
        num = -1
    
    'get parent outcome in data local variable'
    data <<- get("outcome", parent.frame())
    
    'split hospitals by state'
    stateHopitals = split(data, data$State)
    
    'loop over..'
    for (i in 1:length(stateHopitals)) {
        if(outcome == "heart attack") {
            dataX <- stateHopitals[i]
            dataX <- dataX[[1]]     # this is data frame
            dataX[,11] <- as.numeric(dataX[,11])
            dataX <- dataX[, c(2, 7, 11)] #HospitalName, State, HeartAttack
            dataX <- na.omit(dataX)
            if(num == 1) {
                #best case
                dataX <- dataX[order((dataX[,3]), dataX[,1]),]
                cnt = num
            } else if(num == -1) {
                #worst case
                dataX <- dataX[order((dataX[,3]), dataX[,1], decreasing = TRUE),]
                cnt = 1
            } else {
                #all other cases
                dataX <- dataX[order((dataX[,3]), dataX[,1]),]
                cnt = num
            }
            if(dim(dataX)[1] < num)
                Hospital <- c(Hospital, NA)
            else
                Hospital <- c(Hospital, dataX[cnt,1])
            State <- c(State, dataX[1,2])
        } else if(outcome == "heart failure") {
            dataX <- stateHopitals[i]
            dataX <- dataX[[1]]     # this is data frame
            dataX[,11] <- as.numeric(dataX[,11])
            dataX <- dataX[, c(2, 7, 17)] #HospitalName, State, HeartAttack
            dataX <- na.omit(dataX)
            if(num == 1) {
                #best case
                dataX <- dataX[order((dataX[,3]), dataX[,1]),]
                cnt = num
            } else if(num == -1) {
                #worst case
                dataX <- dataX[order((dataX[,3]), dataX[,1], decreasing = TRUE),]
                cnt = 1
            } else {
                #all other cases
                dataX <- dataX[order((dataX[,3]), dataX[,1]),]
                cnt = num
            }
            if(dim(dataX)[1] < num)
                Hospital <- c(Hospital, NA)
            else
                Hospital <- c(Hospital, dataX[cnt,1])
            State <- c(State, dataX[1,2])
        } else if(outcome == "pneumonia") {
            dataX <- stateHopitals[i]
            dataX <- dataX[[1]]     # this is data frame
            dataX[,11] <- as.numeric(dataX[,23])
            dataX <- dataX[, c(2, 7, 11)] #HospitalName, State, HeartAttack
            dataX <- na.omit(dataX)
            if(num == 1) {
                #best case
                dataX <- dataX[order((dataX[,3]), dataX[,1]),]
                cnt = num
            } else if(num == -1) {
                #worst case
                dataX <- dataX[order((dataX[,3]), dataX[,1], decreasing = TRUE),]
                cnt = 1
            } else {
                #all other cases
                dataX <- dataX[order((dataX[,3]), dataX[,1]),]
                cnt = num
            }
            if(dim(dataX)[1] < num)
                Hospital <- c(Hospital, NA)
            else
                Hospital <- c(Hospital, dataX[cnt,1])
            State <- c(State, dataX[1,2])
        } else {
            stop("invalid outcome")
        }
    }
    RetData <- data.frame(hospital = Hospital, state = State)
    RetData
}