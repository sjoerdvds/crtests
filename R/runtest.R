# Run the specified test
# Arguments:
#       test    An object of class 'classification' or 'regression'
#       ...     Extra arguments to runtest
runtest <- function(test, ...) UseMethod("runtest")

# The default test runner. The default test consists of these steps:
#   prepare			Prepare the data for testing, like removing NAs
#	train_model		Create a model with the training data
#	test_model		Make predictions on the holdout data using the created model
#	evaluate		Evaluate the differences between observations and predictions
#					from the holdout set
runtest.default <- function(test, ...){
  data 		<- prepare(test, ...)
  model 		<- train_model(test, data, ...)
  prediction 	<- test_model(model, data, test, ...)
  evaluate(prediction, data, test, ...)
}