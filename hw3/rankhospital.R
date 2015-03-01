rankhospital <- function(state, outcome, num="best") {
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

    #if (num != "best") {
    #    if (num > length(unlist(stateHospitals))) {
    #        return(NA)
    #    }
    #}

    stateHospitals <- outcomeData[outcomeData[,"State"] == state,]
    stateHospitals[,colName] <- as.numeric(stateHospitals[,colName])

    if (num == "worst") {
        maxVal <- max(stateHospitals[,colName], na.rm=T)
        worstHospitals <- stateHospitals["Hospital.Name"][stateHospitals[,colName] == maxVal,]
        choices <- sort(worstHospitals, na.last=T)
        return(choices[1])
    }

    ordering <- order(stateHospitals[colName], stateHospitals["Hospital.Name"])
    ordered <- stateHospitals[ordering,]
    orderedNames <- ordered[,"Hospital.Name"]

    if (num == "best") {
        return(orderedNames(1))
    }
    else if (num == "worst") {
        return(tail(orderedNames, n=1))
    }
    else {
        return(orderedNames[num])
    }
}

