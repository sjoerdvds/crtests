#' Run a classification or regression test
#' 
# Arguments:
#'@param       test    An object of class 'classification' or 'regression'
#'@param       ...     Extra arguments to runtest
runtest <- function(test, ...) UseMethod("runtest")

#'@describeIn runtest The default test run subsequently calls \code{\link{prepare}}, \code{\link{train_model}}, \code{\link{make_predictions}},  \code{\link{evaluate}}
runtest.default <- function(test, ...){
  data 		<- prepare(test, ...)
  model 		<- train_model(test, data, ...)
  prediction 	<- make_predictions(model, data, test, ...)
  evaluate(prediction, data, test, ...)
}