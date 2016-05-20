#' Run a classification or regression test
#' 
# Arguments:
#'@param       test    An object of class 'classification' or 'regression'
#'@param       ...     Extra arguments to runtest
#'@export
#'@examples
#'data(iris)
#'# A classification test
#'test <- createtest(data = iris, 
#'                   dependent = "Species",
#'                   problem = "classification",
#'                   method = "randomForest",
#'                   name = "An example classification test",
#'                   train_index = sample(150, 100)
#')
#'\dontrun{
#'# Run the test
#'runtest(test)
#'}
runtest <- function(test, ...) UseMethod("runtest")

#'@describeIn runtest The default test run subsequently calls \code{\link{prepare}}, \code{\link{train_model}}, \code{\link{make_predictions}},  \code{\link{evaluate}}
#'@export
runtest.default <- function(test, ...){
  test 		<- prepare(test, 
                     ...)
  model 	<- train_model(test, 
                         test$data, 
                         ...)
  test$model <- model
  prediction 	<- make_predictions(model, 
                                  test$data, 
                                  test, 
                                  ...)
  evaluate(prediction, 
           test$data, 
           test, 
           ...)
}