best <- function(state = "TX", outcome = "heart attack") {
        
        ## Read outcome data
        HospitalData <- read.csv("RProgramming-Week3/outcome-of-care-measures.csv", colClasses = "character")
        
        ## Convert rates to numeric values
        options(warn=-1) # turn of warning messages when converting char to NA
        HospitalData[, 11] <- as.numeric(HospitalData[, 11]) # heart attack
        HospitalData[, 17] <- as.numeric(HospitalData[, 17]) # heart failure
        HospitalData[, 23] <- as.numeric(HospitalData[, 23]) # pneumonia
        stateList <- unique(HospitalData[,7])
        options(warn=0) # turn warnings back on

        ## Check that state and outcome are valid
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop ("invalid outcome")
        if (!state %in% stateList) stop("invalid state")
        
        
        ## Return hospital name in that state with lowest 30-day death
        
        ## Select data that matches state
        HospitalData <- HospitalData[ which(HospitalData$State == state),]
        
        ## order data so that lowest value is at top and that the Hospital name is also ordered
        outcomeColumn <- 0 
        if (outcome == "heart attack")  outcomeColumn <- 11 
        if (outcome == "heart failure")  outcomeColumn <- 17 
        if (outcome == "pneumonia")  outcomeColumn <- 23
        HospitalDataOrdered <- HospitalData[ order(HospitalData[,outcomeColumn], HospitalData[,2]), ]
        
        ## best hospital is at the top
        BestHospital <- HospitalDataOrdered[1,2] 
        BestHospital    
}
