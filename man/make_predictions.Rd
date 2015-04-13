% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/make_predictions.R
\name{make_predictions}
\alias{make_predictions}
\alias{make_predictions.default}
\alias{make_predictions.rpart}
\title{Make predictions using a model}
\usage{
make_predictions(model, data, test, ...)

\method{make_predictions}{default}(model, data, test, ...)

\method{make_predictions}{rpart}(model, data, test, ...)
}
\arguments{
\item{model}{A classification or regression model}

\item{data}{The list of train and holdout data sets}

\item{test}{The test being conducted}
}
\description{
Generic function for testing a model by making predictions
}
\section{Methods (by class)}{
\itemize{
\item \code{default}: This function is a simple wrapper to \code{\link[base]{predict}}, which it with the trained model and holdout data. Model classes that require extra arguments to predict can do so through a separate implementations or, less desirably, through the extra arguments.

\item \code{rpart}: Calls predict.rpart with appropriate type: "class" for classifcation problems and "vector" for regression problems. Other problem types are not supported, providing a test with another class throws an error.
}}
\seealso{
predict.rpart
}
