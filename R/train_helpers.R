#' Extract a formula from a test
#' 
#' Extracts a formula of the form \code{dependent ~ .} from the test object
#' @param test An object of class 'regression' or 'classification'
#' @importFrom stats as.formula
extract_formula <- function(test){
  as.formula(paste(test$dependent, "~."))
}