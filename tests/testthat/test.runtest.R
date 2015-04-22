library(crtests)
library(randomForest)
library(caret)
context("Running a test")

# Create a default test object
provide_test <-function(){
  createtest(problem ="classification",
             method ="randomForest",
             original_data = iris,
             name = "test test",
             train_index = sample(150,100),
             dependent= "Species")
}

test_that("Running a classification test with all default arguments works",
  {
    # Create a default test
    test <- provide_test()
    # Run the classification test, expecting an 'evaluation' object
    expect_is(runtest(test), "evaluation")

  }
)

test_that("Running a regression test with all default arguments works",
  {
    # Create a default test, and change it to regression
    test <- provide_test()
    test$problem <- "regression"
    class(test) <- "regression"
    test$dependent <- "Sepal.Length"
    expect_is(runtest(test), "evaluation")
  })
