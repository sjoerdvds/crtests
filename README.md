
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/sjoerdvds/crtests.svg?branch=master)](https://travis-ci.org/sjoerdvds/crtests)

The aim of `Classification and Regression Tests` is to make the process of running classifier and regression tests easier. Each classifier and regression technique available in R, such as Adaboost, Random Forests, or Classification and Regression Trees, seems to have slightly different requirements for the data, such as whether `NA`s are allowed. Furthermore, each technique can have (slightly) different parameters for training a model and making predictions.

This package provides a single API for making predictions based on a regression model or classification model, that makes sure the data is cleaned in such a way that the technique can process it with a minimal amount of problems. The results of a test are presented in a consistent way.

Basically, the aim of `crtests` is to make running a classifier or regression test as simple as providing data, selecting a technique. Several techniques are supported and verified 'out of the box': Random Forests for regression and classification through the [`randomForest` package](https://cran.r-project.org/package=randomForest); CART for regression and classification through the [`rpart` package](https://cran.r-project.org/package=rpart); Adaboost for regression through the [`gbm` package](http://cran.r-project.org/package=gbm) and Adaboost for classification through the `boosting` function from the [`adabag` package](http://cran.r-project.org/package=adabag); linear regression through [`lm`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html).

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
#>       No information rate : 0.46                
#>  P-value (accuracy > NIR) : 8.200386e-16        
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
#>                      Mean error : -0.1257484
#>             Mean absolute error : 0.2032545 
#>               Mean square error : 0.07694968
#>  Mean absolute percentage error : 6.02664   
#>          Root mean square error : 0.2773981
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
#>                   Accuracy 9.00e-01 9.00e-01 9.67e-01 9.50e-01 9.92e-01
#>      Lower bound of 95% CI 7.35e-01 7.35e-01 8.28e-01 8.07e-01 8.70e-01
#>      Upper bound of 95% CI 9.79e-01 9.79e-01 9.99e-01 9.91e-01 1.00e+00
#>        No information rate 3.33e-01 3.33e-01 3.33e-01 3.33e-01 3.33e-01
#>   P-value (accuracy > NIR) 4.86e-15 7.77e-14 2.96e-13 6.67e-11 1.66e-10
#>     McNemar's test P-value       NA       NA       NA      NaN       NA
#>                                Max.
#>                   Accuracy 1.00e+00
#>      Lower bound of 95% CI 8.84e-01
#>      Upper bound of 95% CI 1.00e+00
#>        No information rate 3.33e-01
#>   P-value (accuracy > NIR) 1.66e-10
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
#>                                     Min. 1st Qu.   Median    Mean 3rd Qu.
#>                       Mean error -0.1597 -0.0881 0.000791 0.00278  0.0403
#>              Mean absolute error  0.1760  0.2044 0.214800 0.23850  0.2638
#>                Mean square error  0.0458  0.0656 0.084580 0.09507  0.1072
#>   Mean absolute percentage error  5.6550  6.7110 7.674000 8.01900  9.3140
#>           Root mean square error  0.2141  0.2560 0.290800 0.30220  0.3273
#>                                    Max.
#>                       Mean error  0.188
#>              Mean absolute error  0.354
#>                Mean square error  0.223
#>   Mean absolute percentage error 11.840
#>           Root mean square error  0.472
```

Installing the latest version of crtests
----------------------------------------

1.  Install the release version of `devtools` from CRAN: `install.packages(devtools)`
2.  Install `crtests` from GitHub: `devtools::install_github("sjoerdvds/crtests")`
