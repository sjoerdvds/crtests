
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/sjoerdvds/crtests.svg?branch=master)](https://travis-ci.org/sjoerdvds/crtests)

The aim of `Classification and Regression Tests` is to make the process of running classifier and regression tests easier. Each classifier and regression technique available in R, such as Adaboost, Random Forests, or Classification and Regression Trees, seems to have slightly different requirements for the data, such as whether `NA`s are allowed. Furthermore, each technique can have (slightly) different parameters for training a model and making predictions.

This package provides a single API for making predictions based on a regression model or classification model, that makes sure the data is cleaned in such a way that the technique can process it with a minimal amount of problems. The results of a test are presented in a consistent way.

Basically, the aim of `crtests` is to make running a classifier or regression test as simple as providing data, selecting a technique. Several techniques are supported and verified 'out of the box': Random Forests for regression and classification through the [`randomForest` package](https://cran.r-project.org/web/packages/randomForest/index.html); CART for regression and classification through the [`rpart` package](https://cran.r-project.org/web/packages/rpart/index.html); Adaboost for regression through the [`gbm` package](https://cran.r-project.org/web/packages/gbm/index.html) and Adaboost for classification through the `boosting` function from the [`adabag` package](https://cran.r-project.org/web/packages/adabag/index.html); linear regression through [`lm`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html).

`crtests` splits the classifier/regression testing process into multiple steps, each of which can be adapted to other techniques with requirements of their own. See the "extending" vignette to see how `crtests` can be made to work with your favorite technique, if it does not work out of the box.

`crtests` can be used both for single tests, where a single sample of data is used as a training set, and for multiple tests, where multiple samples of training data are created, and a test is run on each. The latter supports cross validation.

Using crtests: example
----------------------

### Single test with one technique and one training sample

``` r
library(crtests)
library(randomForest)
#> randomForest 4.6-12
#> Type rfNews() to see new features/changes/bug fixes.
library(caret)
#> Loading required package: lattice
#> Loading required package: ggplot2
#> 
#> Attaching package: 'ggplot2'
#> The following object is masked from 'package:randomForest':
#> 
#>     margin

data(iris)
# A classification test
test <- createtest(data = iris, 
                   dependent = "Species",
                   problem = "classification",
                   method = "randomForest",
                   name = "An example classification test",
                   train_index = sample(150, 100)
                   )
runtest(test)
#> Classification Test Evaluation: An example classification test
#> 
#> Test attributes:
#>                                                
#>                    Method : randomForest       
#>        Dependent variable : Species            
#>       Percentage held out : 33.33333% (50 rows)
#>        Total rows in data : 150                
#>       Data transformation : None               
#> 
#> Performance measures & statistics:
#>                                                 
#>                  Accuracy : 0.98                
#>                    95% CI : 0.8935305, 0.9994938
#>       No information rate : 0.44                
#>  P-value (accuracy > NIR) : 9.618575e-17        
#>    McNemar's test P-value : NaN

# A regression test
test <- createtest(data = iris,
                   dependent = "Sepal.Width",
                   problem = "regression",
                   method = "randomForest",
                   name = "An example regression test",
                   train_index = sample(150,100)
                   )
runtest(test)
#> Regression Test Evaluation: An example regression test
#> 
#> Test attributes:
#>                                                      
#>                          Method : randomForest       
#>              Dependent variable : Sepal.Width        
#>             Percentage held out : 33.33333% (50 rows)
#>              Total rows in data : 150                
#>             Data transformation : None               
#> 
#> Performance measures & statistics:
#>                                             
#>                      Mean error : 0.02801924
#>             Mean absolute error : 0.2244926 
#>               Mean square error : 0.08519311
#>  Mean absolute percentage error : 7.702197  
#>          Root mean square error : 0.2918786
```

### Multiple tests with one technique and multiple training samples

``` r
library(crtests)
library(randomForest)
library(rpart)
library(caret)
library(stringr)

# A classification multitest
summary(
  multitest(data = iris,
            dependent = "Species",
            problem = "classification",
            method = "randomForest",
            name = "An example classification multitest",
            iterations = 10,
            cross_validation = TRUE,
            preserve_distribution = TRUE
           )
)
#> Classification Multiple Test Evaluation: An example classification multitest 
#> 
#> Test attributes:
#> 
#> General:
#>                                                         
#>                    Method: randomForest                 
#>        Dependent variable: Species                      
#>       Data transformation: identity                     
#>           Sampling method: 10-fold cross validation with
#>                            preservation of class        
#>                            distribution                 
#> 
#> Summary of attributes per test iteration:
#> 
#>                            Min. 1st Qu. Median Mean 3rd Qu. Max.
#>              Rows held out   30      30     30   30      30   30
#>         Total rows in data  150     150    150  150     150  150
#> 
#> Performance measures:
#> 
#>                                Min.  1st Qu.   Median     Mean  3rd Qu.
#>                   Accuracy 8.67e-01 9.67e-01 9.67e-01 9.67e-01 1.00e+00
#>      Lower bound of 95% CI 6.93e-01 8.28e-01 8.28e-01 8.32e-01 8.84e-01
#>      Upper bound of 95% CI 9.62e-01 9.99e-01 9.99e-01 9.95e-01 1.00e+00
#>        No information rate 3.33e-01 3.33e-01 3.33e-01 3.33e-01 3.33e-01
#>   P-value (accuracy > NIR) 4.86e-15 4.86e-15 2.96e-13 2.31e-10 2.96e-13
#>     McNemar's test P-value       NA       NA       NA      NaN       NA
#>                                Max.
#>                   Accuracy 1.00e+00
#>      Lower bound of 95% CI 8.84e-01
#>      Upper bound of 95% CI 1.00e+00
#>        No information rate 3.33e-01
#>   P-value (accuracy > NIR) 2.30e-09
#>     McNemar's test P-value       NA

# A regression multitest
summary(
  multitest(data = iris,
            dependent = "Sepal.Width",
            problem = "regression",
            method = "rpart",
            name = "An example regression multitest",
            iterations = 15,
            cross_validation = FALSE
           )
)
#> Regression Multiple Test Evaluation: An example regression multitest 
#> 
#> Test attributes:
#> 
#> General:
#>                                                   
#>                          Method: rpart            
#>              Dependent variable: Sepal.Width      
#>             Data transformation: identity         
#>                 Sampling method: 15 random samples
#> 
#> Summary of attributes per test iteration:
#> 
#>                                  Min. 1st Qu. Median Mean 3rd Qu. Max.
#>                    Rows held out   10      10     10   10      10   10
#>               Total rows in data  150     150    150  150     150  150
#> 
#> Performance measures:
#> 
#>                                     Min. 1st Qu.  Median      Mean 3rd Qu.
#>                       Mean error -0.1682 -0.0752 -0.0070 -0.000822   0.051
#>              Mean absolute error  0.1247  0.2140  0.2320  0.238600   0.256
#>                Mean square error  0.0316  0.0725  0.0845  0.095990   0.103
#>   Mean absolute percentage error  4.2410  6.6740  7.9530  8.021000   8.421
#>           Root mean square error  0.1777  0.2693  0.2907  0.300100   0.321
#>                                    Max.
#>                       Mean error  0.168
#>              Mean absolute error  0.411
#>                Mean square error  0.227
#>   Mean absolute percentage error 13.380
#>           Root mean square error  0.476
```

Installing the latest version of crtests
----------------------------------------

1.  Install the release version of `devtools` from CRAN: `install.packages(devtools)`
2.  Install `crtests` from GitHub: `devtools::install_github("sjoerdvds/crtests")`
