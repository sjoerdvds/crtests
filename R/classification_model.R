# -----------2.a) Model training: classification----------------------------------------------------------------------
#' Generic function for creating a classification model
# Arguments:
#'@param method	The method for classification.
#'@param y The dependent (class) variable. Should be a factor for most algorithms
#'@param x The independent variables.
#'@param data	The complete data set for training
#'@param ... Extra arguments to pass to the classification algorithm
# Returns: 
#'@return	 The produced model
classification_model <- function(method, test, x, y, training_data, ...) UseMethod("classification_model")

#'@describeIn classification_model Default function for creating a classification model
#' 
#' \code{\link[base]{get}} s the method and calls it using \code{x}, \code{y}, and \code{data}
classification_model.default <- function(method, test, x, y, training_data, ...){
  f <- get(method)
  f(x=x, y=y, data=training_data, ...)
}

#'@describeIn classification_model Rpart specific function for creating a classification model
#'
#' RPart requires a formula for classification, which is not provided by the default function
classification_model.rpart <- function(method, test, x, y, training_data, ...){
  f <- as.formula(paste(test$dependent, "~."))
  rpart(formula=f, data=training_data, method="class")
}
