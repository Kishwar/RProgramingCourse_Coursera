corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    library(stringr)
    curr_dir_backup <- getwd()
    
    #run complete to get ids of completed cases
    cmp <- complete(directory)
    ids <- cmp[cmp$nobs > threshold,]$id
    
    CorrVector <- numeric() #empty vector
    
    for(i in ids) {
        path <- paste(directory, '/', str_pad(i, 3, pad=0), '.csv', sep="")
        data <- read.csv(path)
        DataComplete <- data[!is.na(data$nitrate) & !is.na(data$sulfate),]
        Correlation <- cor(DataComplete$nitrate, DataComplete$sulfate)
        if(!is.na(Correlation))
            CorrVector <- c(CorrVector, Correlation)
    }
    
    #set back working directory
    setwd(curr_dir_backup)
    
    CorrVector
}