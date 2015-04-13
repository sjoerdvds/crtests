library(crtests)
context("Creating tests")

# List of required (non-optional) arguments
make_args <- function(){
  list(problem ="classification",
       method ="randomForest",
       original_data = iris,
       name = "test test",
       train_index = sample(150,100),
       dependent= "Species"
       )
}
args <- make_args() 


test_that("Creating a test with a problem, method, name, dependent creates an object with correct attributes",
  {
  #Expectation: createtest with problem="class" returns an object of class "class"
  lapply(c("classification", "regression"),
         function(class_name){
           expect_is(do.call(createtest, 
                             c(args[-1], 
                               problem=class_name)
                             ), 
                     class_name)                   
         })
    
  #Expectation: createtest with method="method" returns an object with attribute method of class "method"
  lapply(c(letters),
         function(method_name){
           test <- do.call(createtest,
                           c(args[-2],
                             method=method_name)
                  )
           expect_is(test$method, method_name)
         })
  
  #Expectation: arguments end up as attributes of the test object
  test <- do.call(createtest, args)
  expect_equal(test$method, structure(args$method, class=args$method))
  expect_equal(test$name, args$name)
  expect_equal(test$dependent, args$dependent)
  }
)

test_that("Training and test samples are of the expected size",
  {
  args <- make_args()
  args <- args[-3]
  lapply(seq(from=10, to=150, by=10),
         function(x){
           
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


test_that("Cannot create a test with a train_index that has larger length than nrow(original_data)",{
  data(iris) 
}
)