corr <- function(directory, threshold = 0) {
    filelist <- list.files(directory)

    oz <- '0'
    tz <- '00'
    corvalues <- c()

    for (a in filelist) {

        filelocation <- paste(directory,"/",a,sep="")

        df <- read.csv(filelocation)

        result <- nrow(na.omit(df)) - 1


        if (result >= threshold) {
            temp <- na.omit(df)
            corvalues <- append(corvalues,cor(temp$sulfate, temp$nitrate))
        }
    }

if (length(corvalues) == 0) {
    corvalues <- numeric(length = 0)
}

return(corvalues)

}


