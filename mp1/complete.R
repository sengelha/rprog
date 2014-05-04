# complete: Reads a directory full of files and reports the number of
# completely observed cases in each data file.
complete <- function(directory, id = 1:332) {
	# Build a data frame with two columns.  The first column is the id
	# of the record, the second column has the number of observations.
	# Fill the second column with NAs and then replace them in the
	# inner loop.
	df = data.frame(id, nobs=rep(NA, length(id)))
	for (i in 1:length(id)) {
	    filename <- paste(formatC(id[i], width=3, flag="0"), ".csv", sep="")
		filepath <- file.path(directory, filename)
		table <- read.csv(filepath)
		df[i,"nobs"] <- sum(!is.na(table$sulfate) & !is.na(table$nitrate))
	}
	df
}