# ----------1a. Method specific data preparation----------------------------------------------------------------------
#'Method-specific data preparation
#'
#'Generic function for method-specific data preparation, if any is necessary
# Arguments:
#'@param  	method	The regression or classification method that needs specific data preparation.
#'@param		test	The test being executed
#'@param		data	A list of the train and holdout data sets, that has already been prepared by \code{\link{prepare_data}}
#'@param    ...   Extra arguments to pass on to class methods 
#'@return A prepared data.frame
method_prepare <- function(method, test, data, ...) UseMethod("method_prepare")

#'@describeIn method_prepare Default function for method-specific data preparation. There is no default method-specific preparation, returns \code{\link[base]{identity}}.
#'@return \code{\link[base]{identity}(data)}
method_prepare.default <- function(method, test, data, ...){
  identity(data)
}

#'@describeIn method_prepare Random Forest specific data preparation. Calls \code{\link{group_levels}} on \code{data}, then relevels the holdout set so it has no levels not found in the training set (using \code{\link{prepare_data}})
method_prepare.randomForest <- function(method, test, data, ...){
  data <- group_levels(data = data, 
                       maximum_levels = 32)
  
  #Levels in the data may have changed, so relevel the holdout set
  data$holdout <- prepare_data(data$holdout, 
                               data$train)
  
  data
}
