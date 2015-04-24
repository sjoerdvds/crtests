# ----------1. Data Preparation---------------------------------------------------------------------------------------
#'Prepare the data for the specified test. 
#'
#'This allows for different implementations for regression or classification
# 
#'@param test	The test for which data is prepared
#'@param ... Extra arguments to prepare
#'@return data	A list containing prepared train (\code{data$train}) and holdout (\code{data$holdout}) data frames. Extra method specific preparation is executed through a call to method_prepare
prepare <- function(test,...) UseMethod("prepare")

#'@describeIn prepare The default method relevels the holdout set, so the holdout and train set are completely independent, and to prevent problems with certain algorithms that can't deal with different factor levels across train and holdout set
prepare.default <- function(test, ...){
  train   <- test$data$train
  holdout <- test$data$holdout
  
  # Some algorithms cannot deal with different levels in the same columns between train and holdout set. 
  # As releveling should not cause problems for other algorithms, this is done by default.
  holdout_prepared <- prepare_data(holdout, train)
  train_prepared <- prepare_data(train, relevel=FALSE)
  # Replace the test data by the partially prepared data 
  test$data <- list(train=train_prepared, 
                    holdout=holdout_prepared)
  method_prepare(method = test$method, 
                 test = test)
  
}
