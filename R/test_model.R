# -----------3. Model testing-----------------------------------------------------------------------------------------
#' Make predictions using a model
#'  
#' Generic function for testing a model by making predictions
# Arguments:
#'@param  	model	A classification or regression model
#'@param		data	The list of train and holdout data sets
#'@param		test	The test being conducted
test_model <- function(model, data, test, ...) UseMethod("test_model")

#'@describeIn test_model This function is a simple wrapper to \code{\link[base]{predict}}, which it with the trained model and holdout data. Model classes that require extra arguments to predict can do so through a separate implementations or, less desirably, through the extra arguments.
test_model.default <- function(model, data, test, ...){
  predict(model, newdata=data$holdout, ...)
}

#'@describeIn test_model Calls predict.rpart with appropriate type: "class" for classifcation problems and "vector" for regression problems. Other problem types are not supported, providing a test with another class throws an error.
#'@seealso predict.rpart
test_model.rpart <- function(model, data, test, ...){
  type <- ""
  if(class(test)=="classification"){
    type <- "class"
  }
  else if (class(test)=="regression"){
    type <- "vector"
  }
  else {
    stop(paste("Tests of type", class(test), "are not supported by test_model.rpart"))
  }
  predict(model, newdata=data$holdout, type=type,...)
}
