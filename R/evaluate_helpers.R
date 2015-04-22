#' Evaluate the difference between predictions and observations
#' 
#' Calculate the mean error, mean absolute error, mean squared error, mean absolute percentage error and root mean absolute error of predictions versus observations
#' @param predictions A numerical vector of predictions
#' @param observations A numerical vector of observations, of equal length to \code{predictions}
#' @return An object of type "regression_evaluation", which has the performance measures as attributes
regression_evaluation <- function(predictions, observations, name){
  difference <- (predictions-observations)
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
  structure(class="regression_evaluation",
            list(me = me,
                 mae = mae,
                 mse = mse,
                 mape = mape,
                 rmse = rmse)
            )
}

#' Print a 'regression_evaluation' object
#' 
#' Pretty prints an object of class 'regression_evaluation'
#' @param x Object to print
#' @param ... Further arguments to print.regression_evaluation
print.regression_evaluation <- function(x, ...){
  
}
