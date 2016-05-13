# ----------1a. Method specific data preparation----------------------------------------------------------------------
#'Method-specific data preparation
#'
#'Generic function for method-specific data preparation, if any is necessary
# Arguments:
#'@param  	method	The regression or classification method that needs specific data preparation.
#'@param		test	The test being executed, whose \code{data} attribute is a list of the train and holdout data sets, that has already been prepared by \code{\link{prepare_data}}
#'@param    ...   Extra arguments to pass on to class methods 
#'@return A prepared data.frame
method_prepare <- function(method, test, ...) UseMethod("method_prepare")

#'@describeIn method_prepare Default function for method-specific data preparation. There is no default method-specific preparation, returns \code{\link[base]{identity}}.
#'@return \code{\link[base]{identity}(data)}
method_prepare.default <- function(method, test,  ...){
  identity(test)
}

#'@describeIn method_prepare Random Forest specific data preparation. Calls \code{\link{group_levels}} on \code{data}, then relevels the holdout set so it has no levels not found in the training set (using \code{\link{prepare_data}})
method_prepare.randomForest <- function(method, test,  ...){
  data <- group_levels(data = test$data, 
                       maximum_levels = 32)
  
  reference <- rbind(data$train, data$holdout)
  train_prepared <- prepare_data(data$train,
                             reference,
                             dependent = test$dependent,
                             drop.nas = "all")
  #Levels in the data may have changed, so relevel the holdout set
  holdout_prepared <- prepare_data(data$holdout, 
                               reference,
                               dependent = test$dependent,
                               drop.nas = "all")
  
  # Replace the partially prepared data in the test object by the fully prepared data
  test$data <- list(train = train_prepared,
                    holdout = holdout_prepared)
  
  test
}

#' #'@describeIn method_prepare RPart specific data preparation. Identical to \code{\link{method_prepare.randomForest}}
# method_prepare.rpart <- function(method, test, ...){ 
#   data <- test$data
#   #Levels in the data may have changed, so relevel the holdout set
#   data$holdout <- prepare_data(data$holdout, 
#                                data$train)
#   
#   # Replace the partially prepared data in the test object by the fully prepared data
#   test$data <- data
#   
#   test
#  }