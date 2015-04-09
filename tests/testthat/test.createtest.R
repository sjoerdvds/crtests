library(crtests)
context("Creating tests")

# Test if the function throws an error if one of its arguments is missing
missing_argument_test <- function(fun, default_args){
  lapply(names(formals))
}

test_that("Cannot create a test without data, problem, dependent, method, train_index and name",{
  # TODO replace this by a generic method tester
    data(iris)
    # missing data
    expect_error(createtest(dependent= "Species", problem="classification", method="randomForest", name="A test", train_index=sample(1:nrow(iris), 100)), "")
    # missing dependent
    expect_error(createtest(original_data = iris, problem="classification", method="randomForest", name="A test", train_index=sample(1:nrow(iris), 100)), "")
    # missing problem
    expect_error(createtest(original_data = iris, dependent= "Species", method="randomForest", name="A test", train_index=sample(1:nrow(iris), 100)), "")
    # missing method
    expect_error(createtest(original_data = iris, problem="classification", dependent= "Species", name="A test", train_index=sample(1:nrow(iris), 100)), "")
    # missing train_index
    expect_error(createtest(original_data = iris, problem="classification", dependent= "Species", method="randomForest", name="A test"), "")
    # missing name
    expect_error(createtest(original_data = iris, problem="classification", dependent= "Species", method="randomForest", train_index=sample(1:nrow(iris), 100)), "")
  }
)

test_that("Cannot create a test with a train_index that has larger length than nrow(original_data)",{
    data(iris) 
  }
)