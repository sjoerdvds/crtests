# ----------1.a) Data Preparation-utility functions-------------------------------------------------------------------
#' Prepare data for training or testing. 
#' 
#' This function removes all missing values, including those introduced after (optional) releveling
# Arguments:
#'@param	df The data frame that is to be prepared
#'@param	df_reference An optional reference data frame, whose factor levels are to be applied to df
#'@param  dependent The dependent variable of the data
#'@param	relevel			Logical. Should the df be releveled with df_reference's factor levels?
#'@param  drop.nas    Character vector denoting of which columns the NAs should be removed. See \code{\link{drop_na}} for the available strategies
#'@importFrom stats na.omit
#'@return A data frame stripped of missing values
prepare_data <- function(df, df_reference, dependent, relevel=TRUE, drop.nas = c("dependent","predictors", "all", "none")){
  if(missing(df)){
    stop("Cannot prepare data if data is missing.")
  }
  
  drop.nas <- match.arg(drop.nas)
  
  # The original number of rows should be stored, so the number of rows that are removed can be messaged.  
  original_rows <- nrow(df)
  missing_rows <- 0
  
  if(drop.nas %in% c("dependent", "predictors") & missing(dependent)){
    stop("Cannot remove NAs in predictors or dependent if the dependent is missing.")
  } 
  df_prepared <- drop_na(strategy = drop.nas, 
                         df = df, 
                         dependent = dependent)
  
  # Only if rows were potentially dropped by the NA operation, should the user be notified of how many rows were dropped (if any).
  if(drop.nas %in% c("predictors", "dependent", "all")){
    missing_rows <- original_rows - nrow(df_prepared) 
    if(missing_rows > 0){
      label <- "rows"
      if(missing_rows==1){
        label <- "row"
      }
      warn <- c("Removed",missing_rows, label, "with missing data.")
      warning(paste(warn,collapse=" "))
    }
  }
  
  if (relevel & missing(df_reference)){
    stop("Cannot relevel data without reference")
  }
  if(relevel & !missing(df_reference)){
    # Apply the releveling function to make sure all columns in df have the same factors as those in 
    # the reference data frame. 
    df_prepared <- apply_levels(df_prepared, df_reference)
  }
  
  # A complete row is a row that does not have a NA where it should not have one (e.g. in the predictors or dependent)
  new_missing_rows <- switch(drop.nas,
                              "predictors" = na_count(df_prepared[, -which(names(df_prepared) == dependent)]),
                              "dependent"  = na_count(df_prepared[[dependent]]),
                              "all"        = na_count(df_prepared),
                              "none"       = na_count(df_prepared)
  )
  nas_introduced <- new_missing_rows - missing_rows
  # Determine if NAs were introduced by releveling, and if so, if something should be done about it.
  if(nas_introduced > 0){
    label <- "rows"
    if(nas_introduced == 1){
      label <- "row"
    }
    warn <- c(nas_introduced, label, "with NAs introduced after releveling.")
    if(drop.nas %in% c("predictors", "dependent", "all")){
      df_prepared <- drop_na(strategy = drop.nas, 
                             df = df,
                             dependent = dependent)
      
      warn <- c(warn, "Therefore,", nas_introduced, label, "were removed.")
    }
    warning(paste(warn, collapse=" "))
  }

  # If somehow all rows in the df set were removed, it has no purpose anymore.
  # Therefore, throw an error
  if(nrow(df_prepared)==0){
    stop("After preparing data, 0 rows remained in the data set")
  }
  
  df_prepared

}

#' Remove NAs according to a strategy
#' 
#' @param strategy Character string denoting how NAs should be dealt with. "dependent" means rows with NA in the dependent variable are dropped. "predictors" means rows with NA in an independent variable are dropped. "all" means rows with NA in any column are dropped. "none" means NAs are ignored.
#' @param df Data frame to remove NAs from
#' @param dependent Dependent variable of the data frame
#' @return A data.frame where \code{strategy} has been applied to remove data
drop_na <- function(strategy = c("dependent","predictors", "all", "none"), df, dependent){
  switch(strategy,
         "predictors" = df[is_complete_row(df[,-which(names(df) == dependent)]),],
         "dependent"  = df[!is.na(df[[dependent]]),],
         "all"        = na.omit(df),
         "none"       = identity(df))
}


#'Converts the column factor levels in \code{df} to those in \code{df_reference}
#'
#' Each column in the result is a factor with the values of df and the levels of df_reference. 
#' This means that if there are levels in df_to_change that are not in df_reference, NAs will be introduced.
#' The main use of this function is in classifier problems, where the training and the test set need to have equal factors.
#' To work, all names(df_reference) need to be %in% names(df)
# Arguments:
#'@param df_reference	A reference df, which column levels will be applied to df if that column is a factor
#'@param df	The df that is to be releveled
#'@return A data.frame where all factor's levels where changed. Through applying new levels, NAs could have been introduced
apply_levels <- function(df, df_reference){
  #Iff the vector names(df_reference) %in% names(df) consists of only TRUEs, the factor application can be done
  if(all(names(df_reference) %in% names(df))) {
    # For each column in df_reference that is a factor, make a factor of the same column in df with the reference levels
    # The result is put in a data frame explicitly. If this would not be done explicitly, the result is a matrix.
    data.frame(sapply(names(df_reference), 
                      function(x){ 
                        if(is.factor(df[,x])){
                          factor(df[,x], levels=levels(df_reference[,x]))
                        } else {
                          df[,x]
                        }
                      },
                      # sapply needs the simplify=FALSE option, otherwise factors are converted to integers. This makes it equal to
                      # lapply (names(df_reference), function(x)) with the exception that sapply provides the USE.NAMES argument.
                      # Without this, the column names would have to be set explicitly.
                      simplify=FALSE
    )
    )
  }
  else{
    stop("Not all names(df_reference) are %in% names(df)")
  }
  
}


#' Group infrequent levels in \code{data}, either a factor or a data.frame
#' 
#' @param data A data.frame or factor. In the first case, \code{group_levels} is applied to each factor in the data.frame.
#' @param maximum_levels Numeric. The maximum number of levels allowed per factor
#' @return A factor with at most \code{maximum_levels}, or a data.frame where each factor matches that requirement
#' 
group_levels <- function(data, maximum_levels=32) UseMethod("group_levels")

#' @describeIn group_levels Group infrequent levels in a factor. Takes a factor, and if that factor has more than 'maximum_levels', it makes a table of level frequencies. The top (maximum_levels-1) are left unchanged, all less frequent levels are grouped into the level "other".
#' 
group_levels.factor <- function(data, maximum_levels=32){
  if(length(levels(data))> maximum_levels){
    # Make a table of the factor to determine level frequencies
    frequencies 	<- table(data)
    # Order the frequencies from high to low
    frequencies		<- frequencies[order(frequencies, decreasing=TRUE)]	
    # Take the top (maximum_levels-1) levels. These will be left unchanged. 
    # If there is a number of levels with equal frequencies, an arbitrary
    # subset of those levels is taken.
    top_levels 		<- names(frequencies[1:(maximum_levels-1)]) 
    # The levels with lower frequencies will be grouped as "other"
    levels_to_group	<- names(frequencies[-which(names(frequencies) %in% top_levels)])
    # The actual regrouping
    levels(data)[levels(data) %in% levels_to_group] <- "Other"
  } 
  data
}

#' Group infrequent factor levels in a data.frame
#'
#'@describeIn group_levels  Takes a data.frame, and applies group_levels.factor to each column
#'
group_levels.data.frame <- function(data, maximum_levels=32){
  # Make sure the result is a data.frame, to maintain the original structure
  data.frame(
    sapply(data, 
           # group_levels needs to be wrapped: otherwise sapply doesn't know where to find the correct group_levels
           function(x, maximum_levels){
             group_levels(x, maximum_levels)
           }, 
           maximum_levels=maximum_levels,
           #Do not simplify, as this turns factors into numeric vectors
           #sapply is still necessary, as it retains the column names
           simplify=FALSE
    )
  )
}

#' Group infrequent factor levels in a list of data.frames
#'
#'@describeIn group_levels Takes a list of data.frames and applies \code{group_levels.data.frame} to each
#'
group_levels.list <- function(data, maximum_levels=32){
   lapply(data,
          # group_levels needs to be wrapped: otherwise sapply doesn't know where to find the correct group_levels
          function(x, maximum_levels){
            group_levels(x, maximum_levels)
            },
          maximum_levels = maximum_levels)  
 }


#' Group infrequent factor levels
#' 
#' The default group_levels does nothing. This is desirable behavior for any structure that is not a list, data.frame or factor: there is no meaningful way apply group_levels to this type of structure.
#' @inheritParams group_levels 
#' 
group_levels.default <- function(data, maximum_levels=32){
  identity(data)
}


#' Count the number of NAs in an object
#' 
#' @param x An object, either a vector or a data.frame
#' @param ... Extra arguments to na_count
na_count <- function(x, ...) UseMethod("na_count")

#' @inheritParams  na_count
#' @param columns Vector of column names
#' @describeIn na_count If columns are specified, returns the maximum of the count of NAs for those columns. Otherwise, it returns the number of rows that have a NA in any column.
na_count.data.frame <- function(x, columns = c(), ...){
  if(!missing(columns) & length(columns) > 0){
    na_counts <- lapply(columns, 
                        function(column){
                          na_count(x[[column]])
                        })
    max(unlist(na_counts))
  } else {
    original <- nrow(x)
    nas <- nrow(na.omit(x))
    original - nas
  }
}

#' @inheritParams na_count
#' @describeIn na_count Calls \code{\link{na.omit}} on \code{x}, and returns the length of the result. This is only meaningful for one-dimensional objects (vectors).
na_count.default <- function(x, ...){
  original <- length(x)
  nas <- length(na.omit(x))
  original - nas
}

#' Determine if the rows in a data.frame have NAs
#' 
#' @param data A data.frame
#' @return A vector of length \code{nrow(data)} containing whether that row has \code{\link{NA}}s.
is_complete_row <- function(data){
  complete <- data.frame(!is.na(data))
  # The sum of a row is the number of 'TRUE' values in that column, i.e. how many complete elements there are. If it is lower than the number of columns in the data, the row is incomplete.
  rowSums(complete) == ncol(data)
  
}
