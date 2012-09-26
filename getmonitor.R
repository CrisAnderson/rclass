getmonitor <- function(id, directory, summarize = FALSE) {
# id vector-length of 1 for monitor ID number
tl <- "00"
ol <- "0"
file <- ""
if (nchar(id) == 1) 
  file <- paste(tl,id,sep="")
if (nchar(id) == 2)
  file <- paste(ol,id,sep="")
if (nchar(id) == 3)
  file <- id
#directory is character vector with length of 1 to indicate location of csv files

datacsv <- read.csv(paste(directory,'/',file,'.csv',sep=''), header = TRUE)

#summarize decides whether a summary of the data should be printed to the console

if (summarize == TRUE) 
  print(summary(datacsv))

return(datacsv)

}
