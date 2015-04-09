# -----------2.a) Model training: classification----------------------------------------------------------------------
# Generic function for creating a classification model
# Arguments:
#  	method	The method for classification.
#		y		The dependent (class) variable. Should be a factor for most algorithms
#		x		The independent variables.
#		data	The complete data set for training
#		...		Extra arguments to pass to the classification algorithm
# Returns: 
#		model	The produced model
classification_model <- function(method, test, x, y, training_data, ...) UseMethod("classification_model")

# Default function for creating a classification model
# This _get_s the method and calls it using x, y, and data
classification_model.default <- function(method, test, x, y, training_data, ...){
  f <- get(method)
  f(x=x, y=y, data=training_data, ...)
}

# Rpart specific function for creating a classification model, as RPart requires a formula
classification_model.rpart <- function(method, test, x, y, training_data, ...){
  f <- as.formula(paste(test$dependent, "~."))
  rpart(formula=f, data=training_data, method="class")
}
