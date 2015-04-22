library(crtests)
context("Creating tests")

# List of required (non-optional) arguments
# @param omit Vector of named arguments to omit from the argument list
make_args <- function(omit=c()){
  args <- list(problem ="classification",
       method ="randomForest",
       original_data = iris,
       name = "test test",
       train_index = sample(150,100),
       dependent= "Species"
       )
  args[!(names(args) %in% omit)]
}



test_that("Creating a test with a problem, method, name, dependent creates an object with correct attributes",
  {
  args <- make_args(omit=c("problem"))
  #Expectation: createtest with problem="class" returns an object of class "class"
  lapply(c("classification", "regression"),
         function(class_name){
           expect_is(do.call(createtest, 
                             c(args, 
                               problem=class_name)
                             ), 
                     class_name)                   
         })
  args <- make_args(omit=c("method"))
  #Expectation: createtest with method="method" returns an object with attribute method of class "method"
  lapply(c(letters),
         function(method_name){
           test <- do.call(createtest,
                           c(args,
                             method=method_name)
                  )
           expect_is(test$method, method_name)
         })
  args <- make_args()
  #Expectation: arguments end up as attributes of the test object
  test <- do.call(createtest, args)
  expect_equal(test$method, structure(args$method, class=args$method))
  expect_equal(test$name, args$name)
  expect_equal(test$dependent, args$dependent)
  }
)

test_that("Training and test samples are of the expected size",
  {
  args <- make_args(omit=c("train_index"))
  lapply(seq(from=10, to=140, by=10),
         function(x){
           train_index <- seq_len(x)
           
           test <- do.call(createtest,
                           c(args, 
                             list(train_index=train_index)
                             )
                           )
           data <- test$data
           # Expect that the number of rows in the training set is equal to the provided 'x'
           expect_equal(nrow(data$train), 
                        x)
           # Expect that the number of rows in the holdout set is equal to nrow(iris)-x
           expect_equal(nrow(data$holdout), 
                        nrow(iris)-x)
         }
         )
  }

)

test_that("Cannot create a test without data, problem, dependent, method, train_index and name",
  {
  data(iris)
  args <- make_args()
  # List of expected outcomes
  outcomes <- rep("",length(args))
  names(outcomes) <- names(args)
  # Test what happens when createtest is called with missing arguments, to see if missing values are handled correctly
  missing_argument_test(createtest, args, expect_error, outcomes)
  # Test what happens when createtest is called with values that lead to an match.arg error
  argument_match_test(createtest,args)
}
)

test_that("Cannot create a test if data_transform is NULL or not a function",
{
  args <- make_args()
  args <- c(args, list(data_transform=NULL))
  #Expectation: cannot call createtest with data_transform explicitly set to NULL
  expect_error(do.call(createtest,
                       args),
               "")
  args$data_transform <- "Not a function"
  expect_error(do.call(createtest,
                       args),
               "")
}
)

test_that("Cannot create a test with a train_index that has larger length than nrow(original_data) or has length 0",{
  data(iris) 
  # Take out train_index from the default args
  args <- make_args(omit=c("train_index"))
  # Put in train_index that is too long
  too_long <- c(args, list(train_index=sample(150,150)))
  expect_error(do.call(createtest, 
                       too_long),
               "")
  # Put in train_index that is too short
  too_short <- c(args, list(train_index=c()))
  expect_error(do.call(createtest, 
                       too_short),
               "")
  
}
)
