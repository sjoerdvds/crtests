# ----------1a. Method specific data preparation----------------------------------------------------------------------
# Generic function for method-specific data preparation, if any is necessary
# Arguments:
#  	method	The regression or classification method that needs specific data preparation.
#		test	The test being executed
#		data	A list of the train and holdout data sets, that has already been prepare
method_prepare <- function(method, test, data, ...) UseMethod("method_prepare")

# Default function for method-specific data preparation. There is no default method-specific preparation, so 
# returns the identity.
method_prepare.default <- function(method, test, data, ...){
  identity(data)
}

# Random Forest specific data preparation. This method checks all factor columns for number of levels. If that number is greater 
# than 32 (maximum for Random Forest), it groups the infrequent levels into "Other".
method_prepare.randomForest <- function(method, test, data, ...){
  #To each element of the data list (a data.frame) apply the group_levels function
  data <- lapply(	data, 
                  group_levels,
                  maximum_levels=32
  )
  
  #Levels in the data may have changed, so relevel the holdout set
  data$holdout <- prepare_data(data$holdout, data$train)
  
  data
}
