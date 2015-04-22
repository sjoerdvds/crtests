#' Print an 'evaluation' object
#' 
#' Pretty prints an object of class 'evaluation'
#' @param x Object to print
#' @param ... Further arguments to print.evaluation
#' @details Prints the object to look like a table
print.evaluation <- function(x, digits = max(3, getOption("digits")-3), ...){
  test <- x$test
  # Results should look a little something like:
  # Regression Test: A regression test
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
  test_class_label <- paste0(toupper(substring(test_class, 
                                       1,
                                       1)),
                             substring(test_class,
                                       2),
                             " Test: ",
                             test$name)
  # Top line
  cat(paste0(test_class_label, "\n\n"))
  # Lines after that
  cat("Test attributes:\n")
  # Make the test attributes pretty
  test_attribute_names <- paste(c("Method",
                                  "Dependent variable",
                                  "Percentage held out",
                                  "Total rows in data",
                                  "Data transformation"),
                                ":")
  test_attribute_names <-  format(test_attribute_names, justify="right")
  method <- class(test$method)
  dependent <- test$dependent
  held_out_rows <- nrow(test$data$holdout)
  total_rows <- held_out_rows + nrow(test$data$train)
  held_out_percentage <- paste((held_out_rows / total_rows) * 100,
                               "%")
  held_out <- paste(held_out_percentage, 
                    paste("(", held_out_rows,
                          "rows)")) 
  # Convert the data transformation function to string
  data_transformation <- test$data_transform
  # If the transform function was 'identity' (the default), convert it to "None"
  data_transformation <- ifelse(data_transform=="identity", "None", data_transformation)
  
  test_attribute_values <<- c(method, 
                             dependent,
                             held_out,
                             total_rows,
                             data_transformation)
  test_attributes <- cbind(test_attribute_names,
                           test_attribute_values)
  # Remove row and column names and print the test attributes
  print(remove_names(test_attributes), 
        quote=FALSE)
  
  invisible(x)
}
