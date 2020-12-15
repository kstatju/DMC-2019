##################################################################
## DMC 2019: Standardization of data set 
##
## Programmer: Oscar Aguilar
## Date Created: 05/12/2019
## Date Modified: 05/12/2019 <-------------- (5:30 pm)
## Purpose: Combining training and testing to standardization
##################################################################

std.data <- function(data.train, data.test, var.to.std){
	
	## First we store fraud
	fraud <- data.train$fraud
	
	## First we remove fraud from training data
	data.train <- data.train[, !(names(data.train) %in% c('fraud'))]
	
	## Here we combine the two data sets
	data <- rbind(data.train, data.test)

	## Here we standardize only numerical variables (given their names as argument)
	data.trans <- data.frame(apply(data[, names(data) %in% var.to.std], 2, function(x) (x - mean(x)) / sd(x)))
	
	## Here we put everything together
	data.tot <- data.frame(data[, !(names(data) %in% var.to.std)], data.trans)
	
	## Here we split the data into training and testing 
	data.train <- data.frame(data.tot[1:nrow(data.train),], fraud = fraud)
	data.test <- data.tot[-c(1:nrow(data.train)),]
	
	## Here we create the list to be output
	data.out <- list(data.train, data.test)
	
	return(data.out)
}


# test <- std.data(data.train, data.test, c('grandTotal', 'valuePerSecond'))



