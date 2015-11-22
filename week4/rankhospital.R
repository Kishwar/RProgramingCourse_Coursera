rankhospital <- function(state, outcome, num = "best") {
    mn <- NA
    Hospitals <- NA
    
    'in case of best/worst.. only 1 hospital is returned'
    if(num == "best")
        num = 1
    else if(num == "worst")
        num = -1
    
    'get parent outcome in data local variable'
    data <<- get("outcome", parent.frame())
    
    'filter hospitals of this particular state only'
    stateHopitals = data[data["State"] == state,]
    
    'check if state is not invalid'
    if(dim(stateHopitals)[1] == 0)
        stop("invalid state")
    
    if(outcome == "heart attack") {
        if (num == -1) {
            mn <- max(suppressWarnings(as.numeric(stateHopitals[,11])), na.rm = TRUE)
            Hospitals <- stateHopitals[stateHopitals["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] == mn,]
            Hospitals <- Hospitals[!is.na(Hospitals["Hospital.Name"]),]
            Hospitals <- c(Hospitals$Hospital.Name)
            Hospitals <- Hospitals[1]
        } else if(num == 1) {
            mn <- min(suppressWarnings(as.numeric(stateHopitals[,11])), na.rm = TRUE)
            Hospitals <- stateHopitals[stateHopitals["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] == mn,]
            Hospitals <- Hospitals[!is.na(Hospitals["Hospital.Name"]),]
            Hospitals <- c(Hospitals$Hospital.Name)
            Hospitals <- Hospitals[1]
        } else {
            data <- stateHopitals[order(suppressWarnings(as.numeric(stateHopitals[,11]))),]
            Hospitals <- c(data$Hospital.Name)
            Hospitals <- Hospitals[num]
        }
    } else if(outcome == "heart failure") {
        if (num == -1) {
            mn <- max(suppressWarnings(as.numeric(stateHopitals[,17])), na.rm = TRUE)
            Hospitals <- stateHopitals[stateHopitals["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] == mn,]
            Hospitals <- Hospitals[!is.na(Hospitals["Hospital.Name"]),]
            Hospitals <- c(Hospitals$Hospital.Name)
            Hospitals <- Hospitals[1]
        } else if(num == 1) {
            mn <- min(suppressWarnings(as.numeric(stateHopitals[,17])), na.rm = TRUE)
            Hospitals <- stateHopitals[stateHopitals["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] == mn,]
            Hospitals <- Hospitals[!is.na(Hospitals["Hospital.Name"]),]
            Hospitals <- c(Hospitals$Hospital.Name)
            Hospitals <- Hospitals
        } else {
            data <- stateHopitals[order(suppressWarnings(as.numeric(stateHopitals[,17]))),]
            Hospitals <- c(data$Hospital.Name)
            Hospitals <- Hospitals[num]
        }
    } else {
        stop("invalid outcome")
    }
    Hospitals
}