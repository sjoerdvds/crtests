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
missing_argument_test <- function(fun, args, testthat=expect_error, outcomes){
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
              expect_error(do.call(fun, args_copy), "Error in match.arg")
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