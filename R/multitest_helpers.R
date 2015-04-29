#' Create an evaluation of multiple tests
#' 
#' Creates an object of class 'multitest_evaluation'
#' @param evaluations List of \code{evaluation} objects
#' @param iterations  Numeric. Number of times the test was conducted
#' @param cross_validation Logical. Was cross-validation used as a sampling strategy?
#' @param preserve_distribution Logical. Was preservation of class distribution between training and holdout set attempted?
#' @param name Name of the test
#' @param method Name of the method used in the test
#' @param problem Name of the machine learning problem
#' @return An object of type 'multitest_evaluation'. Attributes are:
#' \tabular{ll}{  
#'  {\code{evaluations}}\tab {List of evaluations}\cr
#'  {\code{iterations}}\tab {Number of times the test was conducted}\cr
#'  {\code{cross_validation}}\tab {Was cross-validation used as a sampling strategy?}\cr
#'  {\code{preserve_distribution}}\tab {Was preservation of class distribution between training and holdout set attempted?}\cr
#'  {\code{name}} \tab{Name of the method used in the test}\cr
#'  {\code{problem}} \tab {Name of the machine learning problem}
#' }
multitest_evaluation <- function(evaluations, iterations, cross_validation, preserve_distribution, name, method, problem){
  structure(class="multitest_evaluation",
            list(
              evaluations = evaluations,
              iterations = iterations,
              cross_validation = cross_validation,
              preserve_distribution = preserve_distribution,
              name = name,
              method = method,
              problem = problem
              ))
}


#' Make a summary of multiple test evaluations
#' 
#' Summary implementation for the results of a multitest
#' @inheritParams base::summary
#'@return		Object of class 'summary.multitest_evaluation'. Attributes are a list of \code{evaluation} objects, 
#'@export
summary.multitest_evaluation <- function(object,...){
  # Combine all the measures from the evaluations
  
  measures <- data.frame(
                t(sapply(object$evaluations,
                       function(x){
                         # Measures should not be a list, or they can't be summarized later
                         # It is safe to unlist: all measures are numeric, so no coercion would happen.
                         unlist(x$measures)
                       }))
              )
  # Combine all the test_attributes from the evaluations
  test_attributes <- data.frame(
                      t(sapply(object$evaluations,
                            function(x){
                              x$test_attributes
                            }))
                     )
  # Make summaries of each column of measures. Transpose it for consistency with the test_attributes
  overall_measures <- t(
                        sapply(measures,
                               function(x){
                                 summary(na.omit(x)) 
                               }
                               )
                      )
  # Make summaries of each column of test_attributes that is numeric
  numeric_attrs <- plyr::ldply(test_attributes,
                            function(x){
                              # x is a list, so try to unlist it. Iff this results in a numeric vector,
                              # a summary is meaningful
                              unlisted_x <- unlist(x)
                              if(is.numeric(unlisted_x)){
                                summary(unlisted_x)
                              } 
                            }
                       )
  # Cleanup: the first column should be the row.names.
  row.names(numeric_attrs) <- numeric_attrs[,1]
  numeric_attrs <- numeric_attrs[,-1]
  
  # Determine the unique values for each column of test_attributes that is not numeric
  other_attrs <- unlist(
                      sapply(test_attributes,
                            function(x){
                              x <- unlist(x)
                              if(is.character(x) || is.factor(x)){
                                unique(x)
                              }
                            },
                            simplify = FALSE)
                    )
  
  structure( 
    list(evaluations = object$evaluations,
         overall_test_attributes = list(numeric = numeric_attrs,
                                        other = other_attrs
                                        ),
         overall_measures = overall_measures,
         test_attributes = test_attributes,
         measures = measures,
         iterations = object$iterations,
         cross_validation = object$cross_validation,
         preserve_distribution = object$preserve_distribution,
         name = object$name,
         method = object$method,
         problem = object$problem
    ),
    class = "multitest_evaluation.summary"
  )
}

#' Print a multitest_evaluation.summary object
#' 
#' @param x Object to print
#' @param digits Numeric. Number of digits to print. Defaults to \code{max(3, getOption("digits")-4)}
#' @param ... Further arguments to \code{print.multitest_evaluation.summary}
#' @export
print.multitest_evaluation.summary <- function(x, digits = max(3, getOption("digits")-4), ...){
  # Regression Multiple Test Evaluation: Some Test
  #
  # Test attributes:
  #
  # General:
  # 
  # Method:              randomForest
  # Dependent variable:  Sepal.Width
  # Sampling method:     10 random samples without preservation of distribution
  # Data transformation: None
  #
  # Summary of attributes per test iteration:
  #
  #                     Min. 1st Qu. Median Mean 3rd Qu. Max.
  # Rows held out        50      50     50   50      50   50
  # Total rows in data  150     150    150  150     150  150
  # 
  # Performance measures:
  #
  #                                    Min.  1st Qu.   Median     Mean   3rd Qu.      Max.
  # Mean error                     -0.01167 -0.01145 -0.01103 -0.01041 -0.009643 -0.008273
  # Mean absolute.error             0.21120  0.21190  0.21200  0.21230  0.212300  0.214300
  # Mean square.error               0.07126  0.07136  0.07166  0.07170  0.071770  0.072460
  # Mean absolute percentage error  0.07212  0.07228  0.07236  0.07245  0.072450  0.073050
  # Root mean square error          0.26690  0.26710  0.26770  0.26780  0.267900  0.269200
  overall_measures <- replace_names(x$overall_measures,
                                    replace_colnames = FALSE)
  # If the problem was classification, do some cleanup
  if(x$problem == "classification"){
    row.names(overall_measures)<- c("Accuracy",
                                    "Lower bound of 95% CI",
                                    "Upper bound of 95% CI",
                                    "No information rate",
                                    "P-value (accuracy > NIR)",
                                    "McNemar's test P-value")
  }
  overall_attributes <- replace_names(x$overall_test_attributes$numeric, 
                                      replace_colnames = FALSE) 
  # Print out the general stuff
  test_problem <- capitalize_first(x$problem)
  test_name <- x$name
  cat(paste(test_problem,
            "Multiple Test Evaluation:",
            test_name,
            "\n\n"))
  
  cat("Test attributes:\n\n")
  cat("General:\n")
  
  # Print the general attributes
  # Pretty print the sampling method
  sample_method <- ""
  if(x$cross_validation){
    sample_method <- paste0(x$iterations,
                            "-fold cross validation")
  } else{
    sample_method <- paste(x$iterations,
                           "random samples")
  }
  if(x$problem == "classification"){
    if(x$preserve_distribution){
      sample_method <- paste(sample_method,
                             "with")
    } else {
      sample_method <- paste(sample_method,
                             "without")
    }
    sample_method <- paste(sample_method, 
                           "preservation of class distribution")
    sample_method <- strwrap(sample_method, width = getOption("width") * .4)
    names(sample_method) <- rep("", length(sample_method))
    names(sample_method)[1] <- "Sampling method"
  }
  if(length(sample_method)==1){
    gen_attrs <- c(x$overall_test_attributes$other,
                 "Sampling method" = sample_method)
  } else {
    gen_attrs <- c(x$overall_test_attributes$other,
                   sample_method)
  }
  # Before printing, 
  # determine the maximum length of any name or row.name, to make the output prettier
  
  allnames <- c(
    row.names(overall_attributes),
    names(gen_attrs),
    row.names(x$overall_measures)    
  )

  width = max(stringr::str_length(allnames))

  gen_attrs <- replace_names(gen_attrs)
  gen_attr_names <- paste0(
                      format(
                        names(gen_attrs), 
                        justify = "right",
                        width = width
                      ),
                      ":"
                    )
  # Replace any empty names (just whitespace and semicolons)
  gen_attr_names[grep("\\s:", gen_attr_names)] <- ""
  gen_attrs_matrix <- cbind(
                        gen_attr_names,
                        gen_attrs
                      )
  
  
  print(
    remove_names(
      gen_attrs_matrix
      ),
    quote = FALSE
  )
  
  # Print the iteration specific attributes:
  cat("\nSummary of attributes per test iteration:\n\n")

  
  row.names(overall_attributes) <- format(row.names(overall_attributes),
                                          #Account for semicolon
                                          width = width + 2,
                                          justify = "right")
  print(overall_attributes, 
        quote=FALSE,
        digits = digits)
  
  # Print the performance measures
  cat("\nPerformance measures:\n\n")


  row.names(overall_measures) <- format(row.names(overall_measures),
                                        #Account for semicolon
                                        width = width + 2,
                                        justify = "right")
  print(overall_measures,
        quote = FALSE,
        digits = digits)
        
  invisible(x)
}

#' Print a multitest_evaluation
#' 
#' @inheritParams base::print
#' @export
print.multitest_evaluation <- function(x, ...){
  summary_x <- summary(x)
  print(summary_x$measures)
  invisible(x)
}