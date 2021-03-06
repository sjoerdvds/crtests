% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_helpers.R
\name{evaluation}
\alias{evaluation}
\title{Create an evaluation object}
\usage{
evaluation(test, measures)
}
\arguments{
\item{test}{Object of class 'regression' or 'classification'}

\item{measures}{List of test measures and their values}
}
\value{
Object of class 'evaluation', with attributes: 'test_attributes', 'measures' and 'test'
}
\description{
Creates an evaluation object from the test and measures. Reads out the attributes of the test
}

