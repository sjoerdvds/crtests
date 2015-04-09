# ----------2. Model training: wrapper--------------------------------------------------------------------------------
# Generic function for training a model. 
# Arguments:
#  	test	The test object. This is passed so the method can be extracted.
#		data	An object of class "regression" or "classification" with at least x, y, train and holdout
#				properties
train_model <- function(test, data,...) UseMethod("train_model")

# Train a model for classification using a classifier algorithm. This function wraps the actual classifier
# Any extra arguments are passed to the classifier function
train_model.classification <- function(test, data,...){
  #Only training data is necessary
  training_data <- data$train
  #If the dependent variable is not a factor (categorical), convert it to categorical
  if(!is.factor(training_data[[test$dependent]])){
    # Make a factor of y
    training_data[[test$dependent]] <- factor(training_data[[test$dependent]])
  }
  #The dependent variable
  y <- training_data[[test$dependent]]
  #All other variables: the independents/predictors
  x <- training_data[, -which(names(training_data)==test$dependent)]
  
  
  #Call the generic classification_model function
  classification_model(method=test$method, test=test, x=x, y=y, training_data=training_data, ...)
}

# Train (fit) a regression model. This function wraps a regression algorithm.
# Any extra arguments are passed to the regression function
train_model.regression <- function(test, data, ...){
  #Only training data is necessary
  data <- data$train
  #Formula for model fitting
  f <- as.formula(paste(test$dependent, "~."))
  #Call the generic regression_model function
  regression_model(method, formula=f, data=data)
}
