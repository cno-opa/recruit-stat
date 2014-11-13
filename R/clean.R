#!usr/bin/Rscript
#Cleans and transforms data for easier use

require(lubridate)
require(gdata)
require(stringr)

#wrap in fn so scripts can be called in order
init_clean <- function() {
  #
  #

  #load and clean
  d <- read.xls( "./data/AllDataOct31.xls", na.strings = c("", "#N/A", "NA", "#DIV/0!") )
  d$X <- NULL
  d$X.1 <- NULL
  d$X.2 <- NULL

  slugify <- function(col_names) {
    col_names <- tolower( gsub( '\\.', '_', str_trim(col_names) ) )
    return( gsub('(_)$|^(_)', '', col_names) )
  }

  names(d) <- slugify( names(d) )

  levels(d$sex) <- c( "F", "M", "M", "U" )

  #make some useful bins
  age_brks <- c( "18", "20", "25", "30", "35", "40", "45", "50", "60", "70" )
  d$age_group <- cut( d$age, age_brks )
  d$age_group <- gsub( "\\(", "", d$age_group )
  d$age_group <- gsub( "]", "", d$age_group )
  d$age_group <- gsub( ",", "-", d$age_group )

  d$month_applied <- paste0( month(d$date_applied, label = TRUE), " ", year(d$date_applied) )

  

  #
  #end init_clean
}
