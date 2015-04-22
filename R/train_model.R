# ----------2. Model training: wrapper--------------------------------------------------------------------------------
#' Train a classification or regression model
#' 
#' Generic function for training a model. 
# Arguments:
#'@param  	test	The test object. This is passed so the method can be extracted.
#'@param		data	An object of class "regression" or "classification" with at least x, y, train and holdout
#'@param		...   Extra arguments to pass to the classification or regression method
train_model <- function(test, data,...) UseMethod("train_model")

#'@describeIn train_model Train a model for classification using a classifier algorithm. This function wraps the actual classifier
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

#'@describeIn train_model Train (fit) a regression model. This function wraps a regression algorithm.
train_model.regression <- function(test, data, ...){
  #Only training data is necessary
  training_data <- data$train
  #Formula for model fitting
  f <- extract_formula(test)
  #Call the generic regression_model function
  regression_model(method = test$method, formula = f, training_data = training_data)
}
