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
#' @describeIn regression_model Fit a regression model using Freund & Schapire's adaboost.M1
regression_model.adaboost.M1 <- function(method, formula, training_data, ...){
  adaboost.M1(formula = formula, data = training_data, coeflearn = "Freund", control = rpart.control())
}
