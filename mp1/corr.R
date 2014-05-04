# corr: Calculates the correlation between sulfate and nitrate for monitor
# locations where the number of completely observed cases (on all variables)
# is greater than the threshold.
corr <- function(directory, threshold = 0) {
    # Use the complete function to determine which monitors meet the
	# threshold requirements.  Save the result in the vector id.
	data <- complete(directory)
	id <- data[data$nobs > threshold,"id"]
	
	# Define a function calccor which, given the monitor id i, calculates
	# the correlation between sulfate and nitrate for that monitor
	# (ignorning NAs)
	calccor <- function(i) {
	    filename <- paste(formatC(i, width=3, flag="0"), ".csv", sep="")
		filepath <- file.path(directory, filename)
		table <- read.csv(filepath)
		cor(table$sulfate, table$nitrate, use="complete.obs")
	}

	# Create a vector of all the results
	vapply(id, calccor, numeric(1))
}