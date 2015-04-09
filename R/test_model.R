# -----------3. Model testing-----------------------------------------------------------------------------------------
# Generic function for testing a model by making predictions
# Arguments:
#  	model	A classification or regression model
#		data	The list of train and holdout data sets
#		test	THe test being conducted
test_model <- function(model, data, test, ...) UseMethod("test_model")

# Default function for testing a model. This function is a simple wrapper to _predict_, which it with the 
# trained model and holdout data. Model classes that require extra arguments to predict can do so through
# a separate implementations or, less desirably, through the extra arguments.
test_model.default <- function(model, data, test, ...){
  predict(model, newdata=data$holdout, ...)
}

# Wrapper to predict.rpart. Calls predict.rpart with appropriate type: "class" for classifcation problems
# and "vector" for regression problems. Other problem types are not supported, providing a test with another class
# throws an error.
# See the rpart documentation (?rpart)
test_model.rpart <- function(model, data, test, ...){
  type <- ""
  if(class(test)=="classification"){
    type <- "class"
  }
  else if (class(test)=="regression"){
    type <- "vector"
  }
  else {
    stop(paste("Tests of type", class(test), "are not supported by test_model.rpart"))
  }
  predict(model, newdata=data$holdout, type=type,...)
}
