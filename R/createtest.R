# -----------Creating a test------------------------------------------------------------------------------------------
#' Create a classification or regression test case
#' 
#' Create a test, which can be run using any of the available runtest functions
#' 
# Arguments:
#'@param       data                    A data frame
#'@param       problem                 Either classification or regression. This influences how the algorithms are trained and what method is used to determine performance
#'@param       data_transform          A quoted function name that transforms the data. It should maintain it in data frame form and maintain the dependent variable.
#'@param       dependent               The dependent variable: the name of the column containing the prediction goal
#'@param       train_index             A vector of the rows to be used as training set. All other rows will form the holdout set
#'@param       method                  The regression or classification method
#'@param       name                    The name of the test. Printed in the test results
#'@param       description             Optional. A more elaborate description of the test
#'@param       ...                     Extra arguments used while running the test.
#'
#'@return An object of class 'classification' or 'regression', which holds the data, method, etc. for executing the test case.
#'@export
createtest <- function(data, problem = c("classification", "regression"), dependent, data_transform = identity, train_index, method, name, description="", ...){
  # The problem should not be missing and be either classification or regression
  if(missing(problem)){
    stop("problem is missing with no default")
  }
  problem <- match.arg(problem)
  # The method should not be missing 
  if(missing(method)){
    stop("method is missing with no default")
  }
  
  
  # If there is no data, nothing can happen
  if(missing(data)){
    stop("data is missing with no default")
  }
  # Without a dependent variable, no model could be trained or tested
  if(missing(dependent)){
    stop("dependent is missing with no default")
  }
  # Without an train index, data cannot be split 
  if(missing(train_index)){
    stop("train_index is missing with no default")
  }
  # The dependent variable should refer to a column by name
  if(!missing(dependent) & typeof(dependent)!="character"){
    stop(paste("Dependent variable should be a column name (character), not", typeof(dependent)))
  }
  # Tests should have a name 
  if(missing(name)){
    stop("Test name is missing with no default")
  }
  # Transform the data using the provided function. 
  # If no function is provided, i.e. the default is overwritten with NULL or a non-function,
  # stop with an error: the provided function cannot be applied to the data.
  if(!is.null(data_transform) & typeof(data_transform)=="closure"){
    #Transform the data
    transformed <- data_transform(data)
    
    # For classification, the dependent variable should be a factor
    if(problem=="classification" & !is.factor(data[[dependent]])){
      data[[dependent]] <- factor(data[[dependent]])
      warning("The dependent variable was converted to factor")
    }

    
    #Check if the train_index is smaller than the number of rows in the transformed. Otherwise, 
    #the holdout set would be empty
    if(length(train_index)>=nrow(data)){
      stop("length(train_index) must be smaller than number of rows in the data")
    }
    #There would be no training data if the train_index is of length zero
    if(length(train_index)==0){
      stop("train_index is of length 0")
    }
    #Combine the train and test sets in one list. This way, all data is compartmentalized in the final test object. 
    #Every unused factor is dropped in the train and test sets, so the sets are completely self-contained.
    #Object properties:
    #   train       Data for training the model
    #   holdout     Data held out for testing the model. Otherwise known as test data, but named holdout here to avoid confusion.
    data <- list(train = droplevels(transformed[train_index,]), holdout = droplevels(transformed[-train_index,]))
    
    #Create a structured object of the type specified by the class problem. This makes that the correct runtest method is selected automatically to run the test.
    #Object properties:
    #   dependent               The fully specified column name of the dependent variable
    #	problem					The type of problem for this test (classification or regression)
    #   data                    The list of train and test data
    #   name                    The name of this test
    #   description             A description of the test
    # 	method					The method that should be used for solving the problem: a single-item list of class method
    #   data_transform          Name of the data_transform function
    #   extra.args              Extra arguments that should be passed to the runtest method executing this test
    #   call                    The call to the createtest function
    #Object class:
    #   problem. If this matches one of the runtest methods, that method will be executed.
    test <-  structure(list(dependent = dependent, 
                            data = data, 
                            name = name, 
                            description = description, 
                            method = structure(method, class=method),
                            data_transform = deparse(data_transform),
                            extra.args = c(...),
                            call = (match.call())
    ), 
    class = problem
    )
    
    # Return the test object
    test
    
  } else {
    stop("data_transform is NULL or not a quoted function: cannot be applied to the data")
  }
  
  
}
