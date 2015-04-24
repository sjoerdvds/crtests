#-------Helper methods for creating training samples------------------------------------------------------
#'Make multiple samples of data
#'@name multisample
NULL

#'
#'\code{cross_fold}: Make 'folds' samples of the data, so \code{all(rbind(folds)==row.names(data))=TRUE}
#'@param  	data					Data to sample
#'@param		folds					Number of folds to create
#'@param		dependent				The dependent variable in the data. Used only if \code{preserve_distribution=TRUE}
#'@param		preserve_distribution	Logical, only applicable if the dependent variable is a factor
#'@return   A list of numeric vectors of length 'folds'
#'@rdname multisample
multisample.cross_fold <- function(data, folds=10, dependent, preserve_distribution=FALSE){
  data_folds <- list()
  # Folds should just be random
  if(!preserve_distribution){
    # Make a shuffled factor with values 1:folds of length (nrow(data))
    split_unshuffled <- rep(1:folds, ceiling(nrow(data)/folds))
    split_unshuffled <- split_unshuffled[1:nrow(data)]
    # Shuffle the unshuffled vector to make a factor for splitting the data
    split_factor <- sample(split_unshuffled)
    
    # Actually split the data using the factor. Every fold is a character, so
    # it needs to be converted to numeric
    data_folds <- split(row.names(data), split_factor)
    data_folds <- lapply(data_folds, as.numeric)
    
  } else {
    # The distribution of each fold should be as similar as possible
    data_folds <- caret::createFolds(data[[dependent]], folds)
    
  }
  # For each fold, make a training set of the other folds
  lapply(1:length(data_folds), 
         function(fold){
           unlist(data_folds[-fold], use.names=FALSE)
         })
}


#' Make random samples of the data
#' 
#'\code{random}:  Makes \code{iterations} random samples of size \code{holdout * nrow(data)}
# Arguments:
#'@param		iterations				    Number of iterations to make
#'@param		holdout					      The fraction of data to be used as holdout set
#'@rdname multisample
multisample.random <- function(data, holdout=0.2, iterations=10, dependent, preserve_distribution=FALSE){
  lapply(1:iterations, function(x, 
                                holdout, 
                                dependent, 
                                preserve_distribution
                                ) {
                                  train_index <- c()
                                  if (preserve_distribution){
                                    #The class distribution in train and test should be as similar as possible. Only makes sense for classification problems: regression has no classes.
                                    train_index <- caret::createDataPartition(data[[dependent]], p=1-holdout, list=FALSE, times=1)
                                  } else {
                                    #The train and test sets should be constructed randomly by taking a sample. The size of the sample is rounded down, as the sample size should be an integer. 
                                    train_index <- sample(1:nrow(data), round((1-holdout) * nrow(data)))
                                  }
                                  train_index
                                },
                                # Set arguments to anonymous inner function
                                holdout=holdout, 
                                dependent=dependent, 
                                preserve_distribution=preserve_distribution
                                )
}
