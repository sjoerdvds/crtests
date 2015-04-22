context("Test data preparation")
library(crtests)



create_test_with_data <- function(){
  sample_data <- c(letters, c(1:10))
  data <- data.frame(a=sample(sample_data, 
                              300, 
                              replace=T), 
                     b=sample(sample_data, 
                              300, 
                              replace=T), 
                     c=sample(sample_data, 
                              300, 
                              replace=T)
                     )
  data <- rbind(data, c(NA, NA, NA))
  createtest(problem="classification",
             method="randomForest",
             original_data = data,
             dependent = "b",
             name = "A test",
             train_index = sample(301, 100)
             )
}
  

test_that("Prepare data results in a dataset without NAs",
  {
    test <- create_test_with_data()
    
    data <- suppressWarnings(prepare(test))
    expect_equal(FALSE, 
                 any(is.na(data$train)))
    expect_equal(FALSE, 
                 any(is.na(data$holdout)))
  })

# Test if factors are reduced properly
factor_length_test <- function(test, max_levels=32){
  data <- suppressWarnings(prepare(test))
  expect_equal(FALSE,
               any(factor_length(data$train)>max_levels))
  expect_equal(FALSE,
               any(factor_length(data$holdout)>max_levels))
}

test_that("Method-specific data preparation results in a dataset without 'long' factors",
  {
    test <- create_test_with_data()
    factor_length_test(test)
    # Now the same for regression
    test$problem <- "regression"    
    factor_length_test(test)
  })

