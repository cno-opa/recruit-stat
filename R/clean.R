#clean.R
#Cleans and transforms data for easier use
#Requires geocall.R -- see main.R initiation sequence

#TODO: PROFIT

require(lubridate)
require(gdata)
require(stringr)


init_clean <- function() {
  #
  #

  #load and clean
  d <- read.xls( "./data/AllDataNov25.xls", na.strings = c("", "#N/A", "NA", "#DIV/0!") )
  d$X <- NULL
  d$X.1 <- NULL
  d$X.2 <- NULL

  slugify <- function(col_names) {
    col_names <- tolower( gsub( '\\.', '_', str_trim(col_names) ) )
    return( gsub('(_)$|^(_)', '', col_names) )
  }

  names(d) <- slugify( names(d) )

  levels(d$sex) <- c( "F", "M", "M", "U" )
  d$identifier <- gsub( ",", "", d$identifier )
  d$identifier <- as.numeric(d$identifier)
  d$disposition <- tolower(d$disposition)

  #make some useful bins
  age_brks <- c( "18", "20", "25", "30", "35", "40", "45", "50", "60", "70" )
  d$age <- as.numeric((ymd(today()) - ymd(d$age))/365)
  d$age_group <- cut( d$age, age_brks )
  d$age_group <- gsub( "\\(", "", d$age_group )
  d$age_group <- gsub( "]", "", d$age_group )
  d$age_group <- gsub( ",", "-", d$age_group )

  d$month_applied <- paste0( month(d$date_applied, label = TRUE), " ", year(d$date_applied) )

  d$days_to_mc <- as.numeric( (ymd(d$written_test) - ymd(d$date_applied)), units = "days" )
  d$days_to_mc[d$days_to_mc < 0] <- NA

  #initialize and execute geocall for most recent two months worth of applicants in lieu of full d$geo <- geoloop(d$zip)
  d$geo <- NA
  cutoff <- max(ymd(d$date_applied)) - days(62)
  d$geo[ymd(d$date_applied) > cutoff] <- geoloop( d$zip[ymd(d$date_applied) > cutoff] )
  d$geo <- as.character(d$geo)

  save(d, file = "./data/master.Rdata")
  write.csv( d, "./output/data-cleaned.csv", row.names = FALSE )

  #
  #end init_clean
}
