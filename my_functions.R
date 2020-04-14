library(tidyverse)
library(magrittr) # set_colnames()

# test dataframe ----------------------------------------------------
mtcars2 <- mtcars

for(ii in 1:ncol(mtcars2)) {
  ww <- sample(1:32, size = 12, replace = F)
  mtcars2[ww,ii] <- NA 
}

# my functions ------------------------------------------------------
# to remove observations/variables with excessive proportion of NAs
removeNA <- function(df, index = 1, prop = 0.2) {
  if(index == 1) { 
    ww <- apply(df, index, function(x) sum(is.na(x))/dim(df)[2]) >= prop
    df[!ww,]
  } else if (index == 2) {
    ww <- apply(df, index, function(x) sum(is.na(x))/dim(df)[1]) >= prop
    df[,!ww]
  } else { stop("Unknown index") }
}

# test area ---------------------------------------------------------
apply(mtcars2, 1, function(x) sum(is.na(x))/ncol(mtcars2)) >= .2
removeNA(mtcars2, index = 1)

