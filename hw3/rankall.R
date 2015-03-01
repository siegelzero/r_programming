rankall <- function(outcome, num="best") {
    # Read data.
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses="character")

    # Verify validity of outcomes.
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

    state <- sort(unique(outcomeData[,"State"]))
    outcomeData[,colName] <- as.numeric(outcomeData[,colName])
    hospital <- character()

    for (name in state) {
        stateHospitals <- outcomeData[outcomeData[,"State"] == name,]

        if (num == "worst") {
            maxVal <- max(stateHospitals[,colName], na.rm=T)
            worstHospitals <- stateHospitals["Hospital.Name"][stateHospitals[,colName] == maxVal,]
            choices <- sort(worstHospitals, na.last=T)
            hospital <- c(hospital, choices[1])
        }
        else {
            ordering <- order(stateHospitals[colName], stateHospitals["Hospital.Name"])
            ordered <- stateHospitals[ordering,]
            orderedNames <- ordered[,"Hospital.Name"]

            if (num == "best") {
                num <- 1
            }
            hospital <- c(hospital, orderedNames[num])
        }
    }
    #return(hospital)
    return(data.frame(hospital, state))
}
