#clean.R
#Cleans and transforms data for easier use
#Requires geocall.R -- see main.R initiation sequence

#TODO: PROFIT

require(lubridate)
require(gdata)
require(stringr)
require(xtermStyle)

init_clean <- function() {
#
#

#let user choose data file
f <- list.files("./data", pattern = "*.xls")

cat( style("Enter the number next to the Excel file you want to use: \n \n", fg = 208) )
for(i in 1:length(f)) {
  cat( style(paste(i, f[i]), fg = 069), sep = "\n" )
}

n <- readLines("stdin", 1, warn=FALSE)
n <- as.numeric(n)

#load and clean
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
levels(d$sex) <- c( "F", "M", "M", "U" )
d$identifier <- gsub( ",", "", d$identifier )
d$identifier <- as.numeric(d$identifier)
d$disposition <- tolower(d$disposition)
d$date_applied <- ymd(d$date_applied)

#make some useful bins
age_brks <- c( "18", "20", "25", "30", "35", "40", "45", "50", "60", "70" )
#d$age <- as.numeric((ymd(today()) - ymd(d$age))/365) <= CS changed from DOB to calculated age
d$age_group <- cut( d$age, age_brks )
d$age_group <- gsub( "\\(", "", d$age_group )
d$age_group <- gsub( "]", "", d$age_group )
d$age_group <- gsub( ",", "-", d$age_group )

d$month_applied <- paste0( month(d$date_applied, label = TRUE), " ", year(d$date_applied) )

d$days_to_mc <- as.numeric( (ymd(d$written_test) - ymd(d$date_applied)), units = "days" )
d$days_to_mc[d$days_to_mc < 0] <- NA

#initialize and execute geocall for most recent 300 days (just for good measure) worth of applicants in lieu of full d$geo <- geoloop(d$zip)
d$geo <- NA
cutoff <- max(ymd(d$date_applied)) - days(300)
d$geo[ymd(d$date_applied) > cutoff] <- geoloop( d$zip[ymd(d$date_applied) > cutoff] )
d$geo <- unlist(d$geo)

save(d, file = "./data/master.Rdata")
write.csv( d, paste("./output/data-cleaned", Sys.Date(), ".csv"), row.names = FALSE )

#
#end init_clean
}
