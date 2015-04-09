# ----------1.a) Data Preparation-utility functions-------------------------------------------------------------------
# Prepare data for training or testing. This function removes all missing values, including those
# introduced after (optional) releveling
# Arguments:
#  	df				The data frame that is to be prepared
#		df_reference	An optional reference data frame, whose factor levels are to be applied to df
#		relevel			Logical. Should the df be releveled with df_reference's factor levels?
prepare_data <- function(df, df_reference, relevel=TRUE){
  
  if (relevel & missing(df_reference)){
    stop("Cannot relevel data without reference")
  }
  else if(relevel & !missing(df_reference)){
    # Apply the releveling function to make sure all columns in df have the same factors as those in 
    # the reference data frame. 
    df <- apply_levels(df, df_reference)
  }
  # Remove missing values.
  # Values in df that had a level not in df_reference were replaced by NA by releveling.
  df_prepared <- na.omit(df)
  
  # Determine if rows were removed due to NAs introduced by releveling. If so, throw a warning
  df_rows_removed <- (nrow(df) - nrow(df_prepared))
  
  if(df_rows_removed > 0){
    warn <- c("NAs introduced in preparing data set. Therefore,", df_rows_removed)
    if(df_rows_removed==1){
      warn <- c(warn, "row was removed")
    }
    else{
      warn <- c(warn, "rows were removed")
    }
    warning(paste(warn, collapse=" "))
  }
  
  # If somehow all rows in the df set were removed, it has no purpose anymore.
  # Therefore, throw an error
  if(nrow(df_prepared)==0){
    stop("After removing NAs, 0 rows remained in the data set")
  }
  df_prepared
}

# Returns a data frame of size ncol(df) * nrow(df)
# Each column in the result is a factor with the values of df and the levels of df_reference. 
# This means that if there are levels in df_to_change that are not in df_reference, NAs will be introduced.
# The main use of this function is in classifier problems, where the training and the test set need to have 
# equal factors.
# To work, all names(df_reference) need to be %in% names(df)
# Arguments:
#		df_reference 	A reference df, which column levels will be applied to df if that column is a factor
#		df				The df that is to be releveled
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

# Groups infrequent levels in the data, either a factor or a data.frame
group_levels <- function(data, maximum_levels=32) UseMethod("group_levels")

# Takes a factor, and if that factor has more than 'maximum_levels', it makes a table of level frequencies. The top (maximum_levels-1)
# are left unchanged, all less frequent levels are grouped into the level "other".
group_levels.factor <- function(factor, maximum_levels=32){
  if(length(levels(factor))> maximum_levels){
    # Make a table of the factor to determine level frequencies
    frequencies 	<- table(factor)
    # Order the frequencies from high to low
    frequencies		<- frequencies[order(frequencies, desc=TRUE)]	
    # Take the top (maximum_levels-1) levels. These will be left unchanged. 
    # If there is a number of levels with equal frequencies, an arbitrary
    # subset of those levels is taken.
    top_levels 		<- names(frequencies[1:(maximum_levels-1)]) 
    # The levels with lower frequencies will be grouped as "other"
    levels_to_group	<- names(frequencies[-which(names(frequencies) %in% top_levels)])
    # The actual regrouping
    levels(factor)[levels(factor) %in% levels_to_group] <- "Other"
  } 
  factor
}

# Takes a data.frame, and applies group_levels to each column
group_levels.data.frame <- function(df, maximum_levels=32){
  # Make sure the result is a data.frame, to maintain the original structure
  data.frame(
    sapply(df, 
           group_levels, 
           maximum_levels=32,
           #Do not simplify, as this turns factors into numeric vectors
           #sapply is still necessary, as it retains the column names
           simplify=FALSE
    )
  )
}

# The default group_levels does nothing. This is desirable behavior for any structure that is not a data.frame or factor: 
# there is no meaningful way apply group_levels to this type of structure.
group_levels.default <- function(data, maximum_levels=32){
  identity(data)
}
