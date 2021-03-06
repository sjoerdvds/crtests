# -----------2.b) Model training: regression--------------------------------------------------------------------------
#' Fit a regression model
#'  
#' Generic function for fitting a regression model
# Arguments:
#'@param  	method	The regression method to use
#'@param		formula	An object of class 'formula', used to fit a model to the data
#'@param		training_data	Train data used to fit the model
#'@param    ... Further arguments
# Returns:
#'@return		model	The fitted model
regression_model <- function(method, formula, training_data, ...) UseMethod("regression_model")

#'@describeIn regression_model Default function for fitting a regression model
#' 
#' This \code{\link[base]{get}}s the method and calls it using formula and data
regression_model.default <- function(method, formula, training_data, ...){
  f <- get(method)
  f(formula=formula, data=training_data, ...)
}
