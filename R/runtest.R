#' Run a classification or regression test
#' 
# Arguments:
#'@param       test    An object of class 'classification' or 'regression'
#'@param       ...     Extra arguments to runtest
#'@export
runtest <- function(test, ...) UseMethod("runtest")

#'@describeIn runtest The default test run subsequently calls \code{\link{prepare}}, \code{\link{train_model}}, \code{\link{make_predictions}},  \code{\link{evaluate}}
#'@export
runtest.default <- function(test, ...){
  test 		<- prepare(test, 
                     ...)
  model 	<- train_model(test, 
                         test$data, 
                         ...)
  prediction 	<- make_predictions(model, 
                                  test$data, 
                                  test, 
                                  ...)
  evaluate(prediction, 
           test$data, 
           test, 
           ...)
}