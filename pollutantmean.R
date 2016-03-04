pollutantmean <- function (directory, pollutant, id=1.332){
  
        ## list.files will read all the csv files into temp
        temp <- list.files(directory, full.names=TRUE, pattern="*.csv")
        
        ## create empty data fram and add all data from each file to this data frame
        mydata <- data.frame()
        for (i in id) mydata <- rbind(mydata, read.csv(temp[i]))

        
        
        mydata <- na.omit(mydata)
        ## remove na data and limit output to 3 digits 
        mean(mydata[[pollutant]], na.rm=TRUE)
        } 