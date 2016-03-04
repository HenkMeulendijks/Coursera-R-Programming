complete <- function (directory, id=1.332){
  
  ## list.files will read all the csv files into temp
  temp <- list.files(directory, full.names=TRUE, pattern="*.csv")
  
  ##  read data from file, omit NA values and store relevant data to the result
  file_c <- numeric(0) ## initate vectors
  nobs <- numeric(0)
    for (file in id) {
      data <- read.csv(temp[file])
      file_c <- c(file_c, file)
      nobs <- c(nobs, sum(complete.cases(data)))
      }
  data.frame(id=file_c, nobs=nobs)
}