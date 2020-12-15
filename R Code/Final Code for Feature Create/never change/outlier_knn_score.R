#####################################
## DMC 2019: Exploratory Analysis 
##
## Programmer: Oscar Aguilar
## Date Created: 04/14/2019
## Date Modified: 04/15/2019  <--------------- (6:37 pm)
## Purpose: knn outlier scores 
#####################################

outlier_knn_score = function(data){

  ## Here we create a temporal data set
  data.temp = data[, !(names(data) %in% c('fraud'))]	
  
  ## First we standardize the data (each column has mean 0 and variance equal to 1)
  data.temp = apply(data.temp, 2, function(x) (x - mean(x)) / sd(x))
  
  ## Here we find the distance matrix using Euclidean distance as measure of similarity
  dist.data = as.matrix(dist(data.temp, method = 'euclidean'))
  
  ## Here we declare the number of k-neighbors
  k = 1:10
  
  ## Here we find the outlier-scores using k-nearest neighbors
  knn.scores = knn_scores(dist.data, k)		

  ## Here we average the outlier-scores
  scores = apply(knn.scores, 1, mean)
  
  ## Here we put together the data to returned
  data.out = data.frame(knn_Out_Score = scores)

  return(data.out)
}

###################################################################################
## This function computes the outlier-scores for a given set of set of neighbors ##
###################################################################################

knn_scores = function(data, k){
  
  ## sanity check (data should be similarity matrix)
  if(!is.matrix(data)) stop('The argument should be a numerical matrix!')
  
  ## k: vector of neighbors
  if(!is.vector(k) | any(k < 0)) stop('The vector of neigbors should be at least 1!')
  
  ## Here we declare the matrix to save the outlier score for each 
  ## considered neighbor
  scores = matrix(0, nrow = dim(data)[1], ncol = 3*length(k))
  
  ## Here we find the outlier score for each neighbor
  for(j in 1:length(k)){
    
    low = 3*j - 2
    up = 3*j
    scores[,(low:up)] = k_nearest_neighbor(data, k[j])
      
  }
  return(scores)
}


############################################################################
## This function computes the outlier-scores based on k-nearest neighbors ##
############################################################################

k_nearest_neighbor = function(data, k){
  
  ## sanity check (data should be similarity matrix)
  if(!is.matrix(data)) stop('The argument should be a numerical matrix!')
  
  ## k: number of neighbors
  if(k <= 0) stop('The number of neigbors should be at least 1!')
  
  ## Finding the number of observations
  n = dim(data)[1]
  
  ## Here we declare the matrix to save the distances of the k-nearest neighbors
  k.mat = matrix(0, nrow = n, ncol = k)
  
  ## Here we loop through the similarity matrix
  for(i in 1:n){
    
    k.mat[i,] = sort(data[i,])[2:(k+1)] 
    
  }
  
  ####################################################
  ## Here we find the distance-based outlier scores ##
  ####################################################
  
  ## nearest score
  near.score = apply(k.mat, 1, min) 
  
  ## average score
  avg.score = apply(k.mat, 1, mean)
  
  ## harmonic mean score
  harmonic.score = apply(k.mat, 1, function(x) length(x) / sum(1/x)) 
  
  ## Here we put together all the scores
  outlier.scores = as.matrix(cbind(near.score, avg.score, harmonic.score))
  
  ## Here we standardize the outlier scores
  outlier.scores = apply(outlier.scores, 2, function(x) (x - mean(x)) / sd(x))
                         
  return(outlier.scores)
}



