complete <-function(directory, id = 1:332) {
    counter <- 0
    oz <- '0'
    tz <- '00'
    results <- c()

    for (a in id) {
        counter <- 0

        if (nchar(a)==1) filen <- paste(tz,a,sep="")
        if (nchar(a)==2) filen <- paste(oz,a,sep="")
        if (nchar(a)==3) filen <- a

        filelocation <- paste(directory,"/",filen,".csv",sep="")

        df <- read.csv(filelocation)

        leng <- nrow(df)

        for (i in 1:leng) { 
            if (is.na(df[i,'sulfate']) | is.na(df[i,'nitrate']))
                counter <- counter + 1
        }

        result <- (leng - counter)
        results <- append(results, result)
    }

output <- data.frame(id = id, nobs = results)

return(output)

}
