# -------Run multiple instances of a test----------------------------------------------------
#' Create and run multiple instances of a test
#' 
#' Wrapper for creating multiple copies of a test and running them. This function supports cross validation and regular sampling. Cross validation splits the data into 'iterations' number of folds, and uses one fold as holdout, using every other fold as training set. This is repeated 'iteration's times, using every fold as holdout exactly once. Non-cross validation takes a random sample of size holdout * nrow(data) and uses it as holdout, the rest is used for training. This is repeated 'iteration's times. Test creation and execution is handled by \code{\link{create_and_run_test}}
# Arguments passed to createtest:
#'@inheritParams createtest
#'@param 		iterations		The number of times the test is to be performed. If cross-validation is used, this is the number of folds
#'@param		holdout				Sample testing only. The fraction of data to be used as holdout set
#'@param		cross_validation		  Logical. Should cross validation be used?
#'@param		preserve_distribution	Logical, classification problems only. Should the distribution of factors in the dependent variable be as similar as possible between holdout and training sets?
# Returns:
#'@return		A list of class \code{'multitest_results_' + problem}, containing the test results of each iteration
multitest <- function(data, dependent, problem=c("classification", "regression"), method=c("randomForest", "rpart"), name, description = "", data_transform=identity, iterations=10, holdout=0.2, cross_validation=FALSE, preserve_distribution=FALSE){
  #Split the data into train and test sets. The rows that are in trainIndex will be in the train set, the rows that are not will be in the test set
  samples <- list()
  if(!cross_validation){   
    samples <- multisample.cross_fold(data, 
                                      folds = iterations, 
                                      dependent = dependent, 
                                      preserve_distribution = preserve_distribution)
    
  } else {
    samples <- multisample.random(data,
                                  holdout = holdout, 
                                  iterations = iterations, 
                                  dependent = dependent, 
                                  preserve_distribution = preserve_distribution)
  }
  
  results<- lapply(samples,
                   create_and_run_test, 
                   data = data, 
                   dependent = dependent, 
                   problem = problem, 
                   method = method, 
                   name = name, 
                   description = description, 
                   data_transform = data_transform)
  structure(results, class=paste0("multitest_results_", problem))
}


#' Create test and run it
#' 
#' A convenience function calling \code{\link{createtest}} first, then runs the test using \code{\link{runtest}}. 
#' @inheritParams createtest
create_and_run_test <- function(train_index, data, dependent, problem=c("classification", "regression"), method=c("randomForest", "rpart"), name, description, data_transform=identity){
  test <- createtest(original_data = data,
                     problem = problem, 
                     dependent = dependent, 
                     data_transform = data_transform, 
                     train_index = train_index, 
                     method = method, 
                     name = name, 
                     description = description)
  runtest(test)
}
