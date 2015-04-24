# -----------4. Evaluation--------------------------------------------------------------------------------------------
#'Evaluate the performance of a prediction. 
#' 
#'Wraps the problem-specific evaluation functions by calling \code{\link{evaluate_problem}}. This wrapper is desirable, as it can perform the extraction of the holdout set (observations)
# Arguments:
#'@param  	prediction	A vector of predictions for each row in the holdout set
#'@param		data		The data list containing train and holdout data sets
#'@param		test		The test object being evaluated
#'@param    ...     Extra arguments to evaluate
#'@return   An object of class 'evaluation', which contains a list of performance measures and a test object.

evaluate <- function(prediction, data, test, ...){
  holdout <- data$holdout
  observations <- holdout[[test$dependent]]
  evaluate_problem(test, prediction, observations)
}

#'Generic function for evaluation of test results
#'
# Arguments:
#'@param		test			    The test that was run
#'@param		prediction		A vector of predictions for each row in the holdout set
#'@param		observations	The true observations for the dependent value in the holdout set
#'@return An object of class 'evaluation', which contains a list of performance measures and a test object.
evaluate_problem <- function(test, prediction, observations) UseMethod("evaluate_problem")

#'@describeIn evaluate_problem Evaluate a classification test's results. Uses \code{\link[caret]{confusionMatrix}} to determine accuracy and other performance measures
evaluate_problem.classification <- function(test, prediction, observations){
  evaluation_matrix <- caret::confusionMatrix(prediction, observations)
  # Make a list of the overall statistics, so its attributes can be extracted with $
  overall <- as.list(evaluation_matrix$overall)
  evaluation( measures = list("Accuracy" = overall$Accuracy,
                              "95% CI"   = c(overall$AccuracyLower, 
                                             overall$AccuracyUpper),
                              "No information rate" = overall$AccuracyNull,
                              "P-value (accuracy > NIR)" = overall$AccuracyPValue,
                              "McNemar's test P-value" = overall$McnemarPValue
                              )
              ,
              test = test
            )
  
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
  # Put it all together in an object of class "regression_evaluation", so it can be printed by the appropriate function
  evaluation(
              measures = list("Mean error" = me,
                              "Mean absolute error" = mae,
                              "Mean square error" = mse,
                              "Mean absolute percentage error" = mape,
                              "Root mean square error" = rmse),
              test = test
            )
  
}

