best <- function(state, outcome) {
    mn <- NA
    Hospitals <- NA
    
    'get parent outcome in data local variable'
    data <<- get("outcome", parent.frame())
    
    'filter hospitals of this particular state only'
    stateHopitals = data[data["State"] == state,]
    
    'check if state is not invalid'
    if(dim(stateHopitals)[1] == 0)
        stop("invalid state")
    
    if(outcome == "heart attack") {
        mn <- min(as.numeric(suppressWarnings(stateHopitals[,11])), na.rm = TRUE)
        Hospitals <- stateHopitals[stateHopitals["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] == mn,]
        Hospitals <- Hospitals[!is.na(Hospitals["Hospital.Name"]),]
        Hospitals <- c(Hospitals$Hospital.Name)
        Hospitals <- Hospitals[1]
    } else if(outcome == "heart failure") {
        mn <- min(suppressWarnings(as.numeric(stateHopitals[,17])), na.rm = TRUE)
        Hospitals <- stateHopitals[stateHopitals["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] == mn,]
        Hospitals <- Hospitals[!is.na(Hospitals["Hospital.Name"]),]
        Hospitals <- c(Hospitals$Hospital.Name)
        Hospitals <- Hospitals[1]
    } else if(outcome == "pneumonia") {
        mn <- min(suppressWarnings(as.numeric(stateHopitals[,23])), na.rm = TRUE)
        Hospitals <- stateHopitals[stateHopitals["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] == mn,]
        Hospitals <- Hospitals[!is.na(Hospitals["Hospital.Name"]),]
        Hospitals <- c(Hospitals$Hospital.Name)
        Hospitals <- Hospitals[1]
    } else {
        stop("invalid outcome")
    }
    Hospitals
}