IsInteger <- function(x){
        test <- all.equal(x, as.integer(x), check.attributes = FALSE)
        if(test == TRUE)(TRUE) else (FALSE)     }

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        HospitalData <- read.csv("RProgramming-Week3/outcome-of-care-measures.csv", colClasses = "character")
        
        ## Convert rates to numeric values
        options(warn=-1) # turn of warning messages when converting from characters to NA
        HospitalData[, 11] <- as.numeric(HospitalData[, 11]) # heart attack
        HospitalData[, 17] <- as.numeric(HospitalData[, 17]) # heart failure
        HospitalData[, 23] <- as.numeric(HospitalData[, 23]) # pneumonia
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
        
        ## order data correctly
        ## outcomeColumn <- 0 
        if (outcome == "heart attack")  outcomeColumn <- 11 
        if (outcome == "heart failure")  outcomeColumn <- 17 
        if (outcome == "pneumonia")  outcomeColumn <- 23
        
        ## normal ordering except when using num="worst" 
        if (num=="worst") HospitalDataOrdered <- HospitalData[ order(HospitalData[,outcomeColumn], HospitalData[,2], decreasing = TRUE), ]
        else HospitalDataOrdered <- HospitalData[ order(HospitalData[,outcomeColumn], HospitalData[,2], decreasing = FALSE), ]
        
        if (num %in% c("best","worst")) RankingsTotal <- 1 else RankingsTotal <- num ## if num is "worst" or "best" then take the top value
        
        Result <- HospitalDataOrdered[,c(2,outcomeColumn)] ## create a matrix of name and outcome
        Result <- na.omit(Result) ## omit NA data
         
        LengthOfResult <- length(Result[,2]) ## check that requested ranking < available result
        if (RankingsTotal > LengthOfResult) return(NA) ## if ranking > length of matrix then return NA
        
        Result[RankingsTotal, 1] ## only keep the data that has been requested
        
}
## Tested with
## rankhospital("TX", "heart failure", 4)
## [1] "DETAR HOSPITAL NAVARRO"
## rankhospital("MD", "heart attack", "worst")
## [1] "HARFORD MEMORIAL HOSPITAL"
## rankhospital("MN", "heart attack", 5000)
## [1] NA

