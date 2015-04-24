#' Utility functions
#' @name util
NULL

#' Test function with missing arguments
#' 
#' Utility for testing how a function deals with missing required arguments. It calls the function \code{length(args)} times, each time
#' omitting one argument
#' @param fun Function to test
#' @param args Complete \code{list} of required arguments to \code{fun}
#' @param testthat Test function to run on every iteration
#' @param outcomes List of length \code{args} with expected outcomes for each test. Names should match those of \code{args}
#' @rdname util
missing_argument_test <- function(fun, args, testthat=testthat::expect_error, outcomes){
  # Only works if the names in outcomes are the same as those in args
  if(all(names(outcomes)%in%names(args))){
    # Go through the arguments by name
    sapply(names(args), 
           function(x){
             # Take out one argument
             incomplete_args <- args[-which(names(args)==x)]
             # Run the testthat function
             testthat(do.call(fun,incomplete_args), outcomes[[x]])
           })  
  } else {
    stop("Outcomes names do not match argument names")
  }
  
}

#' Test function with non-matching arguments
#' 
#' Goes through all the parameters of fun which have a vector of default values. It then calls \code{fun} with a different value.
#' @param fun Function to test
#' @param args Complete \code{list} of required arguments to \code{fun}
argument_match_test <- function(fun, args){
  # Retrieve the formal arguments to fun
  formal_args <- formals(fun)
  lapply(names(args), 
         function(x){
           #See if this argument has multiple default values
            if(length(formal_args[[x]])>1){
              # Evaluate defaults, so the actual vector is returned
              defaults <- eval(formal_args[[x]])
              # Generate a random string for testing 
              # The length should be 5 greater than the combination of the string lengths in formal_args,
              # to make sure the random string cannot be in the defaults
              total_length <- sum(stringr::str_length(defaults)) + 5
              args_copy <- args
              # Put a random string in place of the provided value
              args_copy[[x]] <- random_string(total_length)
              # Call the function, expecting a match.arg error
              testthat::expect_error(do.call(fun, args_copy), "Error in match.arg")
            } else {
              ""
            }
  })
}

#' Generate a random string
#' 
#' Generates a random string of length \code{length}
#' @param length Length of string to generate
#' @return If \code{length>0}: A random sequence of characters of length \code{length}, otherwise an empty string
random_string <- function(length){
  if(length > 0){
    paste(sample(letters, 
                 length, 
                 replace=TRUE
                 ),
          collapse=""
    )
  } else {
    ""
  }
}

#' Determine the length of the factors in a data.frame
#' 
#' Goes through every column in the data.frame, and return the length of its levels
#' @param df A data.frame
#' @return A vector of length \code{n}, with \code{n} the number of factor columns in the data frame, containing the length of the levels of those factors
factor_length <- function(df){
  # Unlist so the result is a vector, not a list, and so the NULL values are removed
  unlist(
    lapply(df,
           # Check for each column if it is a factor, 
           # and if so: return the length of the levels in that factor
           function(col){
             if(is.factor(col)){
               length(levels(col))
             } 
           })
  )
}

#' Set any names of x to ""
#' 
#' @param x An object that has a 'names' property, typically a matrix, list or data.frame
remove_names <- function(x) UseMethod("remove_names")

#' Set all row and column names of a matrix to ""
#' 
#' Set row and column names to "" for pretty printing
#' @param matrix Matrix to 'remove' colnames and rownames from
#' @return Matrix where colnames and rownames consist of only ""
#' @describeIn remove_names
remove_names.matrix <- function(x){
  rownames(x) <- rep("", 
                          nrow(x))
  colnames(x) <- rep("", 
                          ncol(x))
  x
}

#' Capitalize the first letter of a word
#' 
#' Takes a string, and converts its first letter to upper case
#' @param word A string
#' @return String with first letter converted to capital
capitalize_first <- function(word){
  paste0(toupper(
          substring(word, 
                    1,
                    1)
          ),
         substring(word,
                   2)
         )

}

#' Replace strings in the names of an object
#' 
#' Replaces strings matching the pattern in the names of the object by the replacement. If applicable, both row and column names could be replaced. This function is a simple wrapper to \code{\link[stringr]{str_replace_all}}
#' @param object Object of which the names are to be changed
#' @param pattern Pattern to look for, as defined by a POSIX regular expression
#' @param replacement Replacement string
#' @param ... extra arguments to \code{replace_names}
#' @seealso \code{\link[stringr]{str_replace_all}}
replace_names <- function(object, pattern, replacement, ...) UseMethod("replace_names")

#' Default method that replaces names(object)
#' @inheritParams replace_names
#' @describeIn replace_names
replace_names.default <- function(object, pattern = "\\.", replacement = " ", ...){
  names(object) <- stringr::str_replace_all(names(object), 
                               pattern = pattern, 
                               replacement = replacement)
  object
}

#' Replaces row.names in the object, then dispatches to the default
#' @inheritParams replace_names
#' @describeIn replace_names
#' @param replace_rownames Logical. Should row names be replaced?
#' @param replace_colnames Logical. Should column names be replaced?
replace_names.data.frame <- function(object, pattern = "\\.", replacement = " ", replace_rownames = TRUE, replace_colnames = TRUE, ...){
  if(replace_rownames){
      row.names(object) <- stringr::str_replace_all(row.names(object), 
                                       pattern, 
                                       replacement)
  }
  if(replace_colnames){
    NextMethod("replace_names")
  } else {
    object
  }
}

#' Replace row.names and col.names in the object
#' @inheritParams replace_names.data.frame
#' @describeIn replace_names
replace_names.matrix <- function(object, pattern = "\\.", replacement = " ", replace_rownames = TRUE, replace_colnames = TRUE, ...){
  if(replace_rownames){
    row.names(object) <- stringr::str_replace_all(row.names(object), 
                                         pattern, 
                                         replacement)
  }
  if(replace_colnames){
    colnames(object) <- stringr::str_replace_all(colnames(object),
                                        pattern,
                                        replacement)
  }
  object
}
