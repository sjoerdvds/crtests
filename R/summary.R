#------------Summary functions for multitest results------------------------------------------------------------
# Summary implementation for the results of a multitest of a classification problem
# Arguments:
#  	results_list			A list of results of the test
# Returns:
#		list of minimum accuracy, maximum accuracy, mean accuracy, standard deviation of accuracy
# TODO refine...
summary.multitest_results_classification <- function(results_list){
  accuracies <- sapply(results_list, function(x){
    x$overall["Accuracy"]
  })
  c(Minimum=min(accuracies), Mean=mean(accuracies), SD=mean(accuracies), Maximum=max(accuracies))
}
