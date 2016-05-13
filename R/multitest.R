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
#'@export
multitest <- function(data, dependent, problem=c("classification", "regression"), method, name, description = "", data_transform=identity, iterations=10, holdout=0.2, cross_validation=FALSE, preserve_distribution=FALSE){
  #Split the data into train and test sets. The rows that are in trainIndex will be in the train set, the rows that are not will be in the test set
  data_transform_name = ""
  
  data_transform_name <- as.character(substitute(data_transform))
  
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
                   data_transform = data_transform,
                   data_transform_name = data_transform_name)
  multitest_evaluation(evaluations = results, 
                       iterations = iterations,
                       cross_validation = cross_validation,
                       preserve_distribution = preserve_distribution,
                       name = name,
                       method = method,
                       problem = problem
                       )
}


#' Create test and run it
#' 
#' A convenience function calling \code{\link{createtest}} first, then runs the test using \code{\link{runtest}}. 
#' @inheritParams multitest
#' @param train_index A vector containing the rows from \code{data} to be used as the training 
#' @param data_transform_name The name of the data transformation function
#' @return An object of class 'evaluation', containing the evaluated test
create_and_run_test <- function(train_index, data, dependent, problem=c("classification", "regression"), method=c("randomForest", "rpart"), name, description, data_transform=identity, data_transform_name = "identity"){
  test <- createtest(data = data,
                     problem = problem, 
                     dependent = dependent, 
                     data_transform = data_transform, 
                     train_index = train_index, 
                     method = method, 
                     name = name, 
                     description = description)
  
  evaluation <- runtest(test)
  # Make the data_transform attribute more meaningful: otherwise, it would simply be 'data_transform'
  evaluation$test_attributes$`Data transformation` <- data_transform_name
  evaluation
}
