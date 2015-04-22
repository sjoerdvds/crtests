# -----------4. Evaluation--------------------------------------------------------------------------------------------
#'Evaluate the performance of a prediction. 
#' 
#'Wraps the problem-specific evaluation functions by calling \code{\link{evaluate_problem}}. This wrapper is desirable, as it can perform the extraction of the holdout set (observations)
# Arguments:
#'@param  	prediction	A vector of predictions for each row in the holdout set
#'@param		data		The data list containing train and holdout data sets
#'@param		test		The test object being evaluated
#'@return   A vector of predictions

evaluate <- function(prediction, data, test, ...){
  holdout <- data$holdout
  observations <- holdout[[test$dependent]]
  evaluate_problem(test, prediction, observations)
}

#'Generic function for evaluation of test results
#'
# Arguments:
#'@param		test			    The test that was run
#'@param		predictions		A vector of predictions for each row in the holdout set
#'@param		observations	The true observations for the dependent value in the holdout set
evaluate_problem <- function(test, prediction, observations) UseMethod("evaluate_problem")

#'@describeIn evaluate_problem Evaluate a classification test's results. Calls \code{\link[caret]{confusionMatrix}}
evaluate_problem.classification <- function(test, prediction, observations){
  caret::confusionMatrix(prediction, observations)
}

#'@describeIn evaluate_problem Evaluate a regression test's results
evaluate_problem.regression <- function(test, prediction, observations){
  difference <- (prediction-observations)
  # Calculate the mean error
  me <- mean(difference)
  # Calculate the mean absolute error
  mae <- mean(abs(difference))
  # Calculate the mean squared error
  mse <- mean(difference^2)
  # Calculate the mean absolute percentage error
  mape <- mean(abs(difference)/observations)
  # Calculate the root mean absolute error
  rmse <- sqrt(mse)
  
  summary(prediction - observations)
}

