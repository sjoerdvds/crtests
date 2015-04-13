library(crtests)
context("Creating tests")


test_that("Cannot create a test without data, problem, dependent, method, train_index and name",{
  data(iris)
  # List of required (non-optional) arguments
  args <- list(problem ="classification",
               method ="randomForest",
               original_data = iris,
               name = "test test",
               train_index = sample(150,100),
               dependent= "Species"
               )
  # List of expected outcomes
  outcomes <- rep("",length(args))
  names(outcomes) <- names(args)
  # Test what happens when createtest is called with missing arguments, to see if missing values are handled correctly
  missing_argument_test(createtest, args, expect_error, outcomes)
  # Test what happens when createtest is called with values that lead to an match.arg error
  argument_match_test(createtest,args)
}
)


test_that("Cannot create a test with a train_index that has larger length than nrow(original_data)",{
  data(iris) 
}
)