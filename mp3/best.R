best <- function(state, outcome) {
    # Read outcome data from the file outcome-of-care-measures.csv.
    # In the file, NAs are encoded as "Not Available".
    tbl <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")

    # Load the rows we care about into stateRows.  If we do
    # not find any rows, exit with a failure.
    stateRows <- tbl[tbl$State == state,]
    if (nrow(stateRows) == 0) {
        stop("invalid state")
    }

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
    stateRows[, colName] <- as.numeric(stateRows[, colName])
    # Order the table by the mortality metric first and by name second
    orderedData <- stateRows[order(stateRows[,colName], stateRows[,"Hospital.Name"]),]
    # Return the name of the first hospital
    orderedData[1, "Hospital.Name"]
}