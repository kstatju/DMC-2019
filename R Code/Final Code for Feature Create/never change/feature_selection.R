##################################
## Feature Selection Algorithms ##
##################################

lasso_selection <- function(data){
	
	## Checking required package
	if(!requireNamespace('glmnet'))
		stop('package ', sQuote('glmnet'), ' is not available')
	
	## Here we find the number of variables
	n = dim(data)[2]
	n = n - 1
	
	## Here we fit the lasso model for feature selection
	md = glmnet(as.matrix(data[,!(names(data) %in% c('fraud'))]), data$fraud, intercept = F)

	## Here we extract the estimated coefficients
	w = md$beta; m = dim(w)[2]
	w = w[, m]
	
	## Here we select coefficients greater than or equal to 0.001 in aboslute value
	w = w[which(abs(w) > 0.001)]

	## Here we extract variables with coefficients greater than zero
	var.to.select = names(w)
	
	return(var.to.select)
}


boruta_selection <- function(data){
	
	## Checking required package
	if(!requireNamespace('Boruta'))
		stop('package ', sQuote('Boruta'), ' is not available')
	
	## Here we proceed to conduct the boruta algorithm
	boruta.md = Boruta(fraud ~ ., data = data)
	
	## Here we extract the most important variables
	var.to.select = getSelectedAttributes(boruta.md, withTentative = F)
	
	return(var.to.select)
}


loess_selection <- function(data){
	
	## Checking required package
	if(!requireNamespace('caret'))
		stop('package ', sQuote('caret'), ' is not available')

	## Here we compute the importance scores based on loess 
	loess_scores <- filterVarImp(x = data[,!(names(data) %in% c('fraud'))], 
	                             y = data$fraud, nonpara = T)
	                             
	## Here we return each of the variables with it scores
	return(loess_scores)
	
}


randomForest_selection <- function(data, ntree = 2000){
	
	## Checking required package
	if(!requireNamespace('randomForest'))
		stop('package ', sQuote('randomForest'), ' is not available')
		
	## Here we fit the random forest model to compute importance scores	
	rf.md <- randomForest(as.factor(fraud) ~ ., data = data, ntree = ntree, importance = T)
	
	## Here we return the variables with its scores
	return(varImp(rf.md))
}
