#clean.R
#Cleans and transforms data for easier use
#Requires geocall.R -- see main.R initiation sequence

#TODO: PROFIT

init_clean <- function() {
#
#

#let user choose data file
f <- list.files("./data", pattern = "*.xls")

cat("Enter the number next to the Excel file you want to use: \n \n")
for(i in 1:length(f)) {
  cat(paste(i, f[i]), sep = "\n" )
}

n <- readLines("stdin", 1, warn=FALSE)
n <- as.numeric(n)

#load and clean
cat(paste("Using", f[n], "\n \n", sep = " "))
d <- read.xls( paste0("./data/", f[n]), na.strings = c("", "#N/A", "NA", "#DIV/0!"), strip.white = TRUE )
d$X <- NULL
d$X.1 <- NULL
d$X.2 <- NULL

slugify <- function(col_names) {
  col_names <- tolower( gsub( '\\.', '_', str_trim(col_names) ) )
  return( gsub('(_)$|^(_)', '', col_names) )
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

names(d) <- slugify( names(d) )
if( names(d)[1] == "id") {
  names(d)[1] <- "identifier"
}
d$disposition <- trim(d$disposition)
d$m_c__result <- trim(d$m_c__result)
d$w_e__result <- trim(d$w_e__result)
d$agility_result <- trim(d$agility_result)
levels(d$sex) <- c( "F", "F", "M", "M", "U" )
d$identifier <- gsub( ",", "", d$identifier )
d$identifier <- as.numeric(d$identifier)
d$disposition <- tolower(d$disposition)
d$date_applied <- ymd(d$date_applied)
d$written_test <- ymd(d$written_test)
d$writing_exercise <- ymd(d$writing_exercise)
d$agility_test <- ymd(d$agility_test)

#make some useful bins
# age_brks <- c( "18", "20", "25", "30", "35", "40", "45", "50", "60", "70" )
# d$age_group <- cut( d$age, age_brks )
# d$age_group <- gsub( "\\(", "", d$age_group )
# d$age_group <- gsub( "]", "", d$age_group )
# d$age_group <- gsub( ",", "-", d$age_group )

d$month_applied <- paste0( month(d$date_applied, label = TRUE), " ", year(d$date_applied) )

d$days_to_mc <- as.numeric( (ymd(d$written_test) - ymd(d$date_applied)), units = "days" )
d$days_to_mc[d$days_to_mc < 0] <- NA

# check for things that don't make sense
d$errors <- NA

for(i in 1:nrow(d)) {

  # check if test result is missing or wrong given subsequent testing or sent to bg date
  if( is.na(d$m_c__result[i]) & !is.na(d$w_e__result[i]) | is.na(d$m_c__result[i]) & !is.na(d$sent_to_background[i]) ) {
    d$errors[i] <- "result"
  }
  if( !is.na(d$m_c__result[i]) & d$m_c__result[i] == "F" & !is.na(d$w_e__result[i]) | !is.na(d$m_c__result[i]) & d$m_c__result[i] == "F" & !is.na(d$sent_to_background[i]) ) {
    d$errors[i] <- "result"
  }
  if( !is.na(d$w_e__result[i]) & d$w_e__result[i] == "F" & !is.na(d$agility_result[i]) & d$agility_result[i] == "P" | !is.na(d$w_e__result[i]) & d$w_e__result[i] == "F" & !is.na(d$sent_to_background[i]) ) {
    d$errors[i] <- "result"
  }

  # check if dates make sense or are complete
  if( !is.na(d$written_test[i]) ) {
    if( !is.na(d$writing_exercise[i]) & d$writing_exercise[i] < d$written_test[i] ) {
      d$errors[i] <- "date"
    } else if( !is.na(d$date_applied[i]) & d$date_applied[i] > d$written_test[i] ) {
      d$errors[i] <- "date"
    } else if ( !is.na(d$agility_test[i]) & d$agility_test[i] < d$written_test[i]) {
      d$errors[i] <- "date"
    }
  }

  if( !is.na(d$writing_exercise[i]) ) {
    if( !is.na(d$agility_test[i]) & d$agility_test[i] < d$writing_exercise[i] ) {
      d$errors[i] <- "date"
    }
  }

}


#initialize and execute geocall for most recent 300 days (just for good measure) worth of applicants in lieu of full d$geo <- geoloop(d$zip)
# d$geo <- NA
# cutoff <- max(ymd(d$date_applied)) - days(120)
# d$geo[ymd(d$date_applied) > cutoff] <- geoloop( d$zip[ymd(d$date_applied) > cutoff] )
# d$geo <- unlist(d$geo)

save(d, file = "./data/master.Rdata")
write.csv( d, paste("./output/data-cleaned", Sys.Date(), ".csv", sep = "-"), row.names = FALSE )

#
#end init_clean
}
