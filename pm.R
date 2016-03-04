## CAPTAIN SLOG
## vim: set expandtab tabstop=4 shiftwidth=4 autoindent smartindent:
## File         : pollutantmean.R
## System       : Assignment 1
## Date         : 08/08/2014
## Author       : Mark Addinall
## Synopsis     : This file is part of the course work
##                assignments for the Johns Hopkins
##                series of Data Science units.
##                This unit is R Programming
## Expected output
## source("pollutantmean.R")
##	pollutantmean("specdata", "sulfate", 1:10)
## 	[1] 4.064
##	pollutantmean("specdata", "nitrate", 70:72)
## 	[1] 1.706
##	pollutantmean("specdata", "nitrate", 23)
## 	[1] 1.281

## --------------------------------------
clean_table <- function(table, columns) {
        
        ## this funtion iterates through a data table and
        ## removes whole rows based on the validity of
        ## one or more identified COLUMNS.  So if we have
        ## a NaN in foo in a record, but we only care about
        ## the value in bar, maintain the record in the data set.
        ## If bar is NaN, remove the entire row.
        ##
        ## input:  table
        ## output: table MINUS bad records
        
        clean_rows <- complete.cases(table[, columns])
        return(table[clean_rows, ])
}



## -----------------------------------------------------------------
pm <- function(directory = ".", pollutant, id = 1:332) {
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        
        ## first build a file list
        ## what we are doing in this line is using the paste
        ## function to iterate over the id vector, applying
        ## the function of sprintf to each iteration to 
        ## pad that portion of the string as "000".."00n".
        ##
        ## we also use this iteration to build the fully qualified
        ## file name "specdata/005.csv" etc., and return them
        ## in the vector files.
        
        files <- paste( directory,"/", 
                        sprintf("%03d", as.numeric(id)), 
                        ".csv", sep="")
        
        ## I tried for a whole day, like about 16 hours, to R'ify
        ## this code using lapply, sapply, apply etc to read all of 
        ## the required CSV files into one data structure.  I managed
        ## to get the lot into memory, but not in a format that
        ## was readily usable.  lapply gave me a list of lists
        ## with no names, and did not like mean() AT ALL, unlist()
        ## made it worse.  sapply did much the same deal.  I tried
        ## the coerce functions as.data.bleh to no avail....
        
        ## Initialise variables
        
        N           <- 0
        this_sum    <- 0
        total       <- 0
        mean        <- 0
        
        ## SUCK in the data set
        # and calculate means
        
        for (file in files) {
                data        <- read.csv(file)
                data        <- clean_table(data,pollutant)
                N           <- N + nrow(data)
                this_sum    <- sum(data[,pollutant])
                total       <- total + this_sum
        }
        mean <- round(total/N, 3)
        print(mean)
}