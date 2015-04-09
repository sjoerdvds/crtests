# -------Run multiple instances of a test----------------------------------------------------
# Wrapper for creating multiple copies of a test and running them. This function supports cross validation and regular sampling.
# Cross validation splits the data into 'iterations' number of folds, and uses one fold as holdout, using every other
# fold as training set. This is repeated 'iteration's times, using every fold as holdout exactly once.
# Non-cross validation takes a random sample of size holdout * nrow(data) and uses it as holdout, the rest is used for training. This is 
# repeated 'iteration's times.
# Arguments passed to createtest:
#       data             		A data frame
#       problem                 Either classification or regression. This influences how the algorithms are trained 
#                               and what method is used to determine performance
#       data_transform          A function that transforms the data. 
#                               It should maintain it in data frame form and maintain the dependent variable.
#       dependent               The dependent variable: the name of the column containing the prediction goal
#       method                  The regression or classification method
#       name                    The name of the test. Printed in the test results
#       description             Optional. A more elaborate description of the test
# 
# multitest specific arguments:
# 		iterations				The number of times the test is to be performed. If cross-validation is used, this is the number of folds
#		holdout					Sample testing only. The fraction of data to be used as holdout set
#		cross_validation		Logical. Should cross validation be used?
#		preserve_distribution	Logical, classification problems only. Should the distribution of factors in the dependent variable
#								be as similar as possible between holdout and training sets?
# Returns:
#		A list of class 'multitest_results_' + problem, containing the test results of each iteration
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


# Create a test using a specified train_index and run it
# Arguments passed to createtest:
#       data           			A data frame
#       problem                 Either classification or regression. This influences how the algorithms are trained 
#                               and what method is used to determine performance
#       data_transform          A function that transforms the data. 
#                               It should maintain it in data frame form and maintain the dependent variable.
#       dependent               The dependent variable: the name of the column containing the prediction goal
#       method                  The regression or classification method
#       name                    The name of the test. Printed in the test results
#       description             Optional. A more elaborate description of the test
# multitest.integer specific arguments:
#		train_index				A vector of rows to use as the training sample		
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
