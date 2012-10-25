rankall <- function(outcome, num = 'best') {
    #read in the data, then change the columns we are sorting on to numeric
    oc <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    oc[,11] <- suppressWarnings(as.numeric(oc[,11]))
    oc[,17] <- suppressWarnings(as.numeric(oc[,17]))
    oc[,23] <- suppressWarnings(as.numeric(oc[,23]))
    
    #create a list of states for verification
    states <- sort(unique(oc$State))
    
    #make a list of outcomes to check for
    conditions <- c('heart attack', 'heart failure', 'pneumonia')
    #if (!state %in% states) { stop('invalid state') }
    if (!outcome %in% conditions) { stop('invalid outcome') }
  
    #pick a column to select based on the outcome
    if (outcome == 'heart attack' ) { selector <- 11 }
    if (outcome == 'heart failure' ) { selector <- 17 }
    if (outcome == 'pneumonia' ) { selector <- 23 }
    
    #create a list for the data frame outcomes
    hospitals <- c()
    
    #loop through all states
    for (i in states) {
        #make a subset, just for that state
        soc <- oc[grep(i, oc$State, ignore.case=T),]
        
        #get a dataframe, sorted on the outcome, then drop the NAs out
        sorted <- soc[order(soc[,selector],soc[,2]),c(1,2,selector)]
        sorted <- na.omit(sorted)

        #figure out indexes for 'best' and 'worst'
        if (num == 'best') { num <- 1 }
        if (num == 'worst') { num <- nrow(sorted) }

        #return the name of the hospital at that rating
        hospitals <- append(hospitals, sorted[num,2])
     }
     
    #create output dataframe
    data.frame(hospital=hospitals, state=states)
}