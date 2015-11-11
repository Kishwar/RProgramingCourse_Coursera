complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    library(stringr)
    RetDF <- 0
    nIndex <- 1
    nobVector <- vector(mode="numeric", length(id))
    DataDim <- 0
    for(i in id) {
        path <- paste(directory, '/', str_pad(i, 3, pad=0), '.csv', sep="")
        data <- read.csv(path)
        DataDim <- dim(data[!is.na(data$nitrate) & !is.na(data$sulfate),])
        nobVector[nIndex] <- DataDim[1]
        nIndex <- nIndex + 1
    }
    RetDF <- data.frame(id=id, nobs=nobVector)
    RetDF
}