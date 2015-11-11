makeVector <- function(x = numeric()) {
    m <- NULL  #set mean as NULL object
    set <- function(y) {
        x <<- y     #assign value
        m <<- NULL  #mean
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set=set, get=get, setmean=setmean, getmean=getmean)
}