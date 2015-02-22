# Write a function named 'pollutantmean' that calculates the mean of a pollutant
# (sulfate or nitrate) across a specified list of monitors. The function
# 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
# Given a vector monitor ID numbers, 'pollutantmean' reads that monitors'
# particulate matter data from the directory specified in the 'directory'
# argument and returns the mean of the pollutant across all of the monitors,
# ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id=1:332) {
    allValues <- numeric(0)

    for (i in id) {
        filename <- paste(directory, sprintf("%03d.csv", i), sep='/')
        frame <- read.csv(filename)
        values <- frame[pollutant][,]
        allValues <- c(allValues, values)
    }
    mean(allValues, na.rm=T)
}
