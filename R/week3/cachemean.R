#source first makeVector.R
cachemean <- function(x, ...) {
    #x here is x <- makeVector(VectorValues), list object of functions
    m <- x$getmean() #lets see any mean was caluclated before
    if(!is.null(m)) {
        message("getting cache data...")
        return(m)
    }
    #no cache found.. lets calculate mean and set it to cache
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(x)
    return(m)
}