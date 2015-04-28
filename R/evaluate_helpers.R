#' Create an evaluation object
#' 
#' Creates an evaluation object from the test and measures. Reads out the attributes of the test
#' @param test Object of class 'regression' or 'classification'
#' @param measures List of test measures and their values
#' @return Object of class 'evaluation', with attributes: 'test_attributes', 'measures' and 'test'
evaluation <- function(test, measures){
  method <- class(test$method)
  dependent <- test$dependent
  held_out_rows <- nrow(test$data$holdout)
  total_rows <- held_out_rows + nrow(test$data$train)
  
  # Convert the data transformation function to string
  data_transformation <- test$data_transform
  # If the transform function was 'identity' (the default), convert it to "None"
  data_transformation <- ifelse(data_transformation=="identity", "None", data_transformation)
  
  test_attributes <- list("Method" = method, 
                       "Dependent variable" = dependent,
                       "Rows held out" = held_out_rows,
                       "Total rows in data" = total_rows,
                       "Data transformation" = data_transformation)
  structure(class="evaluation",
            list(measures = measures,
                 test_attributes = test_attributes,
                 test = test))
}

#' Print an 'evaluation' object
#' 
#' Pretty prints an object of class 'evaluation'
#' @param x Object to print
#' @param digits Numeric. Number of digits to print. Defaults to \code{max(3, getOption("digits")-4)}
#' @param ... Further arguments to print.evaluation
#' @details Prints the object to look like a table
#' @export
print.evaluation <- function(x, digits = max(3, getOption("digits")-4), ...){
  test <- x$test
  measures <- x$measures
  test_attributes <- x$test_attributes
  # Results should look a little something like:
  # Regression Test Evaluation: A regression test
  #
  # Test attributes:
  #
  #                         Method:  randomForest
  #             Dependent variable:  Sepal.Width
  #            Percentage held out:  20% (30 rows)
  #             Total rows in data:  150
  #            Data transformation:  None
  # 
  # Performance measures & statistics:
  #
  #                     Mean error:  0.08
  #            Mean absolute error:  0.23
  #              Mean square error:  0.09
  # Mean absolute percentage error:  0.08
  #         Root mean square error:  0.30
  
  # Format the test class & name prettily: 'Regression Test Results' or 'Classification Test Results' etc.
  test_class <- class(test)
  test_class_label <- paste0(capitalize_first(test_class),
                             " Test Evaluation: ",
                             test$name)
  # Top line
  cat(paste0(test_class_label, "\n\n"))
  # Lines after that
  cat("Test attributes:\n")
  
  # ----Make the test attributes pretty-------------------------
  # Convert held_out_rows to a percentage
  held_out_percentage <- paste0(format(test_attributes$"Rows held out" / test_attributes$"Total rows in data" * 100,
                                       digits),
                                "%")
  held_out <- paste(held_out_percentage, 
                    paste0("(", test_attributes$"Rows held out",
                           " rows)")) 
  # Insert the held out percentage string into the attributes
  test_attributes <- c(test_attributes[1:2], "Percentage held out" = held_out, test_attributes[4:5])
  test_attribute_names <- paste(c(names(test_attributes), 
                                names(measures)),
                                ":")

  test_attribute_names <-  format(test_attribute_names, justify="right")
  # Remove the 5 last test_attribute names: these where just included to make sure the width of format is correct
  test_attribute_names <- test_attribute_names[1:5]


  test_attribute_matrix <- cbind(test_attribute_names,
                                 format(test_attributes,
                                    digits))
  # Remove row and column names and print the test attributes
  print(remove_names(test_attribute_matrix), 
        quote=FALSE)
  
  # ----Make the test measures pretty---------------------------
  cat("\nPerformance measures & statistics:\n")
  measure_names <- paste(names(measures), 
                         ":")
  measure_names_labels <- format(measure_names, 
                                 justify="right")
  measure_matrix <- cbind(measure_names_labels,
                          format(measures, 
                                 digits, justify="right")
                    )
  print(remove_names(measure_matrix),
        quote = FALSE)
  
  invisible(x)
}

#' Summary of an evaluation
#' 
#' Produces a summary of an evaluation, consisting of the test attributes and the performance measures
#' @param object Evaluation object to make summary of
#' @param include_test_attributes Logical. Should all attributes of the test be included in the output? 
#' @param ... Extra arguments to \code{summary.evaluation}
#' @export
summary.evaluation <- function(object, include_test_attributes=TRUE, ...){
  # The output should be a transposed matrix (1 x n)
  out <- t(
          as.matrix(
            object$measures
            )
          )
  rownames(out) <- object$test$name
  if(include_test_attributes){
    test_attributes <- t(
                        as.matrix(
                          object$test_attributes
                          )
                        )
    out <- cbind(test_attributes, 
                 out)
  }
  out
}
