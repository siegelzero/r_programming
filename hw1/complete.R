# Write a function that reads a directory full of files and reports the number
# of completely observed cases in each data file. The function should return a
# data frame where the first column is the name of the file and the second
# column is the number of complete cases.

complete <- function(directory, id=1:332) {
    nobs <- numeric(0)

    for (i in id) {
        filename <- paste(directory, sprintf("%03d.csv", i), sep='/')
        frame <- read.csv(filename)
        complete <- complete.cases(frame)
        total <- sum(complete)
        nobs <- c(nobs, total)
    }
    data.frame(id, nobs)
}
