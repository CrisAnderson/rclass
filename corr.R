corr <- function(directory, threshold = 0) {
    filelist <- list.files(directory)

    shortlist <- c()

    counter <- 0
    oz <- '0'
    tz <- '00'
    results <- c()

    for (a in filelist) {
        counter <- 0

        #if (nchar(a)==1) filen <- paste(tz,a,sep="")
        #if (nchar(a)==2) filen <- paste(oz,a,sep="")
        #if (nchar(a)==3) filen <- a

        #filelocation <- paste(directory,"/",filen,".csv",sep="")
        filelocation <- paste(directory,"/",a,sep="")

        df <- read.csv(filelocation)

        leng <- nrow(df)

        for (i in 1:leng) { 
            if (is.na(df[i,'sulfate']) | is.na(df[i,'nitrate']))
                counter <- counter + 1
        }

        result <- (leng - counter)
        if (result >= threshold) {
            results <- append(results, result)
            shortlist <- append(shortlist, a)
        }
    }

output <- data.frame(id = shortlist, nobs = results)

return(output)

}


