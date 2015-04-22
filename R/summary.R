#------------Summary functions for multitest results------------------------------------------------------------
#' Make a summary of multiple executions of a test
#' 
#' Summary implementation for the results of a multitest of a classification problem
# Arguments:
#'@param 	results_list			A list of results of the test
# Returns:
#'@return		list of minimum accuracy, maximum accuracy, mean accuracy, standard deviation of accuracy
# TODO refine...
summary.multitest_results_classification <- function(object,...){
  accuracies <- sapply(object, function(x){
    x$overall["Accuracy"]
  })
  c(Minimum=min(accuracies), Mean=mean(accuracies), SD=mean(accuracies), Maximum=max(accuracies))
}
