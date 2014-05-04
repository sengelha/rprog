# pollutantmean: Prints the mean of a pollutant (sulfate or nitrate)
# across as specified list of monitors.
pollutantmean <- function(directory, pollutant, id = 1:332) {
    # We will combine all pollutant data into the data vector
	data <- vector()
	for (i in id) {
	    filename <- paste(formatC(i, width=3, flag="0"), ".csv", sep="")
		filepath <- file.path(directory, filename)
		table <- read.csv(filepath)
		data <- c(data, table[,pollutant])
	}
	mean(data, na.rm = TRUE)
}