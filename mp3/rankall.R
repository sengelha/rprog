rankall <- function(outcome, num = "best") {
    # Read outcome data from the file outcome-of-care-measures.csv.
    # In the file, NAs are encoded as "Not Available".
    tbl <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")

    # Determine the column we will need to process.  If the
    # provided outcome name is invalid, exit with a failure.
    if (outcome == "heart attack") {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
        colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("invalid outcome")
    }
    # Mark the mortality column as a numeric so it will sort properly.
    tbl[, colName] <- as.numeric(tbl[, colName])

    # Order the table by state, then by mortality rate, then by name
    orderedData <- tbl[order(tbl[,"State"], tbl[,colName], tbl[,"Hospital.Name"], na.last = NA),]

    # Pull out the nth result from each state into a data frame
    selectNth <- function(data) {
        if (num == "best") {
            data[1]
        } else if (num == "worst") {
            data[length(data)]
        } else {
            data[num]
        }
    }
    result <- aggregate(orderedData$Hospital.Name, by=list(orderedData$State), FUN=selectNth)

    # Remove NAs and reformat for output
    colnames(result) <- c("state", "hospital")
    result <- result[!is.na(result$hospital),]
    result
}