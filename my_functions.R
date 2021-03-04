library(tidyverse)
library(magrittr) # magrittr::set_colnames()

# test dataframe ----------------------------------------------------
mtcars2 <- mtcars

for(ii in 1:ncol(mtcars2)) {
  ww <- sample(1:32, size = 12, replace = F)
  mtcars2[ww,ii] <- NA 
}

# my functions ------------------------------------------------------
#' @title removeNA
#' @description to remove observations/variables with excessive proportion of NAs
#' @param df data.frame/tibble
#' @param index 1 for rows, 2 for columns
#' @param prop proportion of NA in index to be considered to remove
#' @returns data.frame/tibble that has excessive NA-containing rows/columns removed
removeNA <- function(df, index = 1, prop = 0.2) {
  if(index == 1) { 
    ww <- apply(df, index, function(x) sum(is.na(x))/dim(df)[2]) >= prop
    df[!ww,]
  } else if (index == 2) {
    ww <- apply(df, index, function(x) sum(is.na(x))/dim(df)[1]) >= prop
    df[,!ww]
  } else { stop("Unknown index") }
}

#' @description mac version for "clipboard" in windows
clipboard <- pipe("pbpaste")