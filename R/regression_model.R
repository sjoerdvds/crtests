# -----------2.b) Model training: regression--------------------------------------------------------------------------
# Generic function for fitting a regression model
# Arguments:
#  	method	The regression method to use
#		formula	An object of class 'formula', used to fit a model to the data
#		data	Train data used to fit the model
# Returns:
#		model	The fitted model
regression_model <- function(method, formula, data, ...) UseMethod("regression_model")

# Default function for fitting a regression model
# This _get_s the method and calls it using formula and data
regression_model.default <- function(method, formula, train_data, ...){
  f <- get(method)
  f(formula=formula, data=train_data, ...)
}
