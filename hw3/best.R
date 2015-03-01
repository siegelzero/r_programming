best <- function(state, outcome) {
    # Read data.
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses="character")

    # Verify validity of outcomes.
    if (!(state %in% outcomeData[,"State"])){
        stop("invalid state")
    }

    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }

    if (outcome == "heart attack") {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }
    else if (outcome == "heart failure") {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }
    else {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }

    stateHospitals <- outcomeData[outcomeData[,"State"] == state,]
    stateHospitals[,colName] <- as.numeric(stateHospitals[,colName])
    minVal <- min(stateHospitals[,colName], na.rm=T)
    bestHospitals <- stateHospitals["Hospital.Name"][stateHospitals[,colName] == minVal,]

    choices <- sort(bestHospitals, na.last=T)
    choices[1]
}
