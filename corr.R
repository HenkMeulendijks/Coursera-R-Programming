corr <- function(directory = ".", threshold = 0){
 
  ## list.files will read all the csv files into temp
  temp <- list.files(directory, full.names=TRUE, pattern="*.csv")
  
  ##  store relevant data to the result
  result <- numeric(0)  

    for (file in 1:length(temp)) {
            data <- read.csv(temp[file])
            data <- na.omit(data)
            if (threshold < sum(complete.cases(data))) result <- c(result, cor(data$sulfate, data$nitrate))
            }
    result
  }