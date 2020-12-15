################################################
## DMC 2019: Exploratory Analysis 
##
## Programmer: Oscar Aguilar
## Date Created: 04/11/2019
## Date Modified: 04/15/2019  <---------------- (6:39 pm)
## Purpose: Principal Component Outlier Score 
################################################


####################################################################################
## This function computes the outlier-score based on principal component analysis ##
####################################################################################

outlier_princ_comp_score = function(data){

  ## Here we create a temporal data set
  data.temp = data[, !(names(data) %in% c('fraud'))]		
  
  ## First we standardize the data (each column has mean 0 and variance equal to 1)
  data.temp = apply(data.temp, 2, function(x) (x - mean(x)) / sd(x))
  
  ##################################################################
  ## Here we computs the outlier score for each row in the matrix ##
  ##################################################################
  
  ## First we compute the covariance matrix sure 
  sigma = cov(data.temp)
  row.names(sigma) = NULL
  colnames(sigma) = NULL
  
  ## Here we find P (matrix of eigenvectors)
  P = eigen(sigma)$vectors
  
  ## Here we find D'
  D.prime = data.temp %*% P
  
  ## Here we standardize the columns of D'
  D.prime = apply(D.prime, 2, function(x) (x - mean(x)) / sd(x))
  
  ## Here we find the centroid of the rows of D.prime
  D.prime.centroid = apply(D.prime, 2, mean)
  
  ## Here we find the euclidean distance of each row with respect to the centroid
  Euclid.dists = apply(D.prime, 1, function(x) sum((x - D.prime.centroid)^2))
  
  ## Here we standardize the squared Euclidean distances
  Euclid.dists = (Euclid.dists - mean(Euclid.dists)) / sd(Euclid.dists)
  
  ## Here we add the principal component outlier score
  data.out = data.frame(Prin_Comp_Out_Score = Euclid.dists)

  return(data.out)
}
