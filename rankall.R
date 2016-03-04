IsInteger <- function(x){
        test <- all.equal(x, as.integer(x), check.attributes = FALSE)
        if(test == TRUE)(TRUE) else (FALSE)}

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        options(warn=-1) 
        HospitalData <- read.csv("RProgramming-Week3/outcome-of-care-measures.csv", colClasses = "character")
        hospitallist <- numeric() 
        ## Check that state and outcome are valid
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop ("invalid outcome")
        if ((!IsInteger(num)) && (!num %in% c("best","worst"))) stop("invalid number")
        
        ## For each state, find the hospital of the given rank
        stateList <- (unique(HospitalData[,7]))
        stateList <- stateList[order(stateList)]
        df <- data.frame()
        for (state in stateList) {
                hospital <- rankhospital2(HospitalData, state, outcome, num)
                ## print(paste("state: ",state , " hospital: ", hospital))
                hospitallist <- c(hospitallist,hospital)
                } 
        df <- cbind(hospitallist, stateList)
        rownames(df) <- stateList
        colnames(df) <- c("hospital","state")
        df
        ## Return a data frame withthe hospital names and the
        ## (abbreviated) state name
}

rankhospital2 <- function(HospitalData, state, outcome, num = "best") {
        
        ## Convert rates to numeric values
        options(warn=-1) # turn of warning messages when converting char to NA
        suppressWarnings (HospitalData[, 11] <- as.numeric(HospitalData[, 11])) # heart attack
        suppressWarnings (HospitalData[, 17] <- as.numeric(HospitalData[, 17])) # heart failure
        suppressWarnings (HospitalData[, 23] <- as.numeric(HospitalData[, 23])) # pneumonia
        stateList <- unique(HospitalData[,7])
        ## options(warn=0) # turn warnings back on

        ## Check that state and outcome are valid
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop ("invalid outcome")
        if (!state %in% stateList) stop("invalid state")
        if ((!IsInteger(num)) && (!num %in% c("best","worst"))) stop("invalid number")

        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        ## Select data that matches state
        HospitalData <- HospitalData[ which(HospitalData$State == state),]
        
        ## order data so that lowest value is at top and that the Hospital name is also ordered
        outcomeColumn <- 0 
        if (outcome == "heart attack")  outcomeColumn <- 11 
        if (outcome == "heart failure")  outcomeColumn <- 17 
        if (outcome == "pneumonia")  outcomeColumn <- 23
        
        if (num=="worst") HospitalDataOrdered <- HospitalData[ order(HospitalData[,outcomeColumn], HospitalData[,2], decreasing = TRUE), ]
        else HospitalDataOrdered <- HospitalData[ order(HospitalData[,outcomeColumn], HospitalData[,2], decreasing = FALSE), ]
        
        if (num %in% c("best","worst")) RankingsTotal <- 1 else RankingsTotal <- num ## if num is "worst" or "best" then take the top value
        
        Result <- HospitalDataOrdered[,c(2,outcomeColumn)] ## create a matrix of name and outcome
        Result <- na.omit(Result) ## omit NA data
        
        LengthOfResult <- length(Result[,2]) ## check if ranking > length of result
        if (RankingsTotal > LengthOfResult) return(NA) ## in that case return NA
        
        Result[RankingsTotal,1] ## only keep the data that has been requested
}

## tested with
## head(rankall("heart attack", 20), 10)
## tail(rankall("pneumonia", "worst"), 3)
## tail(rankall("heart failure"), 10)