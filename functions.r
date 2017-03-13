# Culled from A. Shenkin's functions library

factor.to.numeric <- function(x) {
  # necessary for converting factor to numeric.  Converting factors to numeric
  # with as.numeric gets their ranks, not levels. This is kludgy, since I assume
  # all numeric columns have been automatically converted to factors by
  # as.data.frame.  It would be better to address this in the as.data.frame
  # call, but there is no colClasses argument available to force column types.
  as.numeric(levels(x))[as.integer(x)]
}

factor.to.integer <- function(x) {
  as.integer(levels(x))[as.integer(x)]
}

set_df_coltypes <- function (df, col_types) {
  # Function to set column types in dataframes.  col_types is a character vector or a dataframe whose column types you want to duplicate.
  # col_types is recycled if ncol(df) > length(col_types)
  
  coerce_fun = list (
    "character"   = `as.character`,
    "factor"      = `as.factor`,
    "numeric"     = `as.numeric`,
    "integer"     = `as.integer`,
    "POSIXct"     = `as.POSIXct`,
    "logical"     = `as.logical`,
    "date"        = `as.Date` )
  
  if (class(col_types) == "data.frame") {
    col_types = unlist(lapply(col_types, class))
  }
  
  for (i in 1:length(df)) {
    df[,i] = coerce_fun[[ col_types[(i-1) %% length(col_types) + 1] ]]( df[,i] ) #apply coerce function
  }
  #lapply(coerce_fun, `[`, col_types) #list of functions to apply
  return(df)
}

coerce <- function( invar, totype ) {
  # coerce a varible to the appropriate type.  totype = character|factor|numeric|integer|POSIXct|logical
  coerce_fun = list (
    "character"   = `as.character`,
    "factor"      = `as.factor`,
    "numeric"     = `as.numeric`,
    "integer"     = `as.integer`,
    "POSIXct"     = `as.POSIXct`,
    "logical"     = `as.logical`,
    "date"        = `as.Date` )
  
  coerce_fun[[ totype ]]( invar )
}

as.level <- function( fac, level_class = "integer" ) {
  # get levels of a factor, instead of the default behavior which returns ordinal position when as.* is used
  #  pass in level_class for the class of the levels to output, default behavior is to treat as integer
  
  if (level_class == "integer") {
    levels_out = as.integer( levels(fac) )[as.integer(fac)]
    
  } else if (level_class == "character") {
    levels_out = as.character( levels(fac) )[as.integer(fac)]
    
  } else if(level_class == "numeric") {
    levels_out = as.numeric( levels(fac) )[as.integer(fac)]
  }
  
  return( levels_out )
}