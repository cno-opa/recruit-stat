#analysis.R
#performs various analyses on cleaned data

#TODO: make prop table generator fn less ugly. find way to not harcode list of data tables in env

require(lubridate)
require(dplyr)
require(xtermStyle)

init_analysis <- function() {
#
#

#utility functions
month_str2num <- function(str) {
  str <- tolower(str)
  months <- list("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  match(str, months)
}

div_by_left <- function(table) {
  p <- table
  for(i in seq(3, ncol(table), 1)) {
    p[,i] <- table[,i]/table[,i - 1]
  }
  return(p)
}

#load
load("./data/master.Rdata")

#create subsets of successful groups at each step
qual <- subset(d, disposition != "not qualified")
docs <- subset(qual, disposition != "documents needed")
mc <- subset(docs, !is.na(written_test))
mc_attend <- subset(mc, m_c__result == "P" | m_c__result == "F")
mc_pass <- subset(mc_attend, m_c__result == "P")
we <- subset(mc_pass, !is.na(writing_exercise))
we_attend <- subset(we, w_e__result == "P" | w_e__result == "F")
we_pass <- subset(we_attend, w_e__result == "P")
agil <- subset(we_pass, !is.na(agility_test))
agil_attend <- subset(agil, agility_result == "P" | agility_result == "F")
agil_pass <- subset(agil_attend, agility_result == "P")

step_yields <- list(all = d,
                    qualified = qual,
                    docsSubmitted = docs,
                    scheduleMC = mc,
                    attendMC = mc_attend,
                    passMC = mc_pass,
                    scheduleWE = we,
                    attendWE = we_attend,
                    passWE = we_pass,
                    scheduleAgility = agil,
                    attendAgility = agil_attend,
                    passAgility = agil_pass)

#step yields. this calculates three points in time to compare: the baseline and the previous two complete months. month is considered "complete" if more than 25 days have passed
make_step_table <- function() {

  #fn to summarize step yield size and proportion by time period
  success_table <- function(l, lower_limit = "2013-12-01", upper_limit = max(ymd(d$date_applied))) {
    s <- subset( l, ymd(l$date_applied) >= ymd(lower_limit) & ymd(l$date_applied) <= ymd(upper_limit) )
    nrow(s)
  }

  #set months to compare to baseline
  mx <- day( max(ymd(d$date_applied)) )

  if(mx > 25) {
    current <- max(ymd(d$date_applied))
  } else {
    current <- seq(max(ymd(d$date_applied)), length = 2, by = "-1 month")[2]
  }

  prev <- seq(current, length = 2, by = "-1 month")[2]

  #set lower and upper limits for prev and current
  current_l <- paste( year(current), month(current), "01", sep = "-" )
  current_u <- paste( year(current), month(current), days_in_month(month(current)), sep = "-" )

  prev_l <- paste( year(prev), month(prev), "01", sep = "-" )
  prev_u <- paste( year(prev), month(prev), days_in_month(month(prev)), sep = "-" )

  #get tables of successful applicants for each step for each time period
  step_baseline <- as.data.frame(sapply(step_yields, success_table, lower_limit = "2014-01-01", upper_limit = "2014-05-31"))
    colnames(step_baseline) <- "Baseline"
  step_prev <- as.data.frame(sapply(step_yields, success_table, lower_limit = prev_l, upper_limit = prev_u))
    colnames(step_prev) <- paste( month(prev, label = TRUE), year(prev) )
  step_current <- as.data.frame(sapply(step_yields, success_table, lower_limit = current_l, upper_limit = current_u))
    colnames(step_current) <- paste( month(current, label = TRUE), year(current) )

  #combine
  step_success_table <- cbind(step_baseline, step_prev, step_current)

  return(step_success_table)
}

make_step_prop_table <- function(x) {

  #makes naive step table
  calc_prop <- function(col) {
    l <- list()
    for(i in seq(2, r, 1)) {
      prop <- col[i]/col[i-1]
      l <- append(l, prop)
    }
    return(l)
  }

  #make prop table that calculates attendance proportion only by applicants who have test scheduled before end of reporting period
  make_aware <- function(a, b) {
    mx <- max(ymd(d$date_applied))
    prev <- paste("2014", month_str2num( colnames(b)[2] ) , "01", sep = "-")
    current <- paste("2014", month_str2num( colnames(b)[3] ) , "01", sep = "-")

    a[4,2] <- b[5,2]/nrow( as.data.frame(step_yields[4])[ymd(unlist(as.data.frame(step_yields[4])[2])) >= ymd(prev) & ymd(unlist(as.data.frame(step_yields[4])[2])) < ymd(current) & ymd(unlist(as.data.frame(step_yields[4])[4])) < ymd(mx), ] )
    a[4,3] <- b[5,3]/nrow( as.data.frame(step_yields[4])[ymd(unlist(as.data.frame(step_yields[4])[2])) >= ymd(current) & ymd(unlist(as.data.frame(step_yields[4])[4])) < ymd(mx), ] )

    a[10,2] <- b[11,2]/nrow( as.data.frame(step_yields[10])[ymd(unlist(as.data.frame(step_yields[10])[2])) >= ymd(prev) & ymd(unlist(as.data.frame(step_yields[10])[2])) < ymd(current) & ymd(unlist(as.data.frame(step_yields[10])[8])) < ymd(mx), ] )
    a[10,3] <- b[11,3]/nrow( as.data.frame(step_yields[10])[ymd(unlist(as.data.frame(step_yields[10])[2])) >= ymd(current) & ymd(unlist(as.data.frame(step_yields[10])[8])) < ymd(mx), ] )

    return(a)
  }

  r <- nrow(x)
  t <- as.data.frame(sapply(x, calc_prop))
  row.names(t) <- row.names(step_success_table[2:12,])
  t <- make_aware(t,x)
  return(t)
}

#fn calls
step_success_table <- make_step_table()
step_success_prop_table <- make_step_prop_table(step_success_table)
  step_success_prop_table$steps <- row.names(step_success_prop_table)
  step_success_prop_table <- as.data.frame(lapply(step_success_prop_table, unlist))


mc_outcomes <- left_join( tally(group_by(mc, written_test)), tally(group_by(mc_attend, written_test)), by = "written_test") %.%
  left_join( tally(group_by(mc_pass, written_test)), by = "written_test" )

colnames(mc_outcomes) <- c("date", "scheduled", "attended", "passed")
mc_outcomes_prop <- div_by_left(mc_outcomes)

we_outcomes <- left_join( tally(group_by(we, writing_exercise)), tally(group_by(we_attend, writing_exercise)), by = "writing_exercise") %.%
  left_join( tally(group_by(we_pass, writing_exercise)), by = "writing_exercise")

colnames(we_outcomes) <- c("date", "scheduled", "attended", "passed")
we_outcomes_prop <- div_by_left(we_outcomes)

median_days_to_mc <- summarise( group_by(d, written_test), all = median(days_to_mc), attendees = median(days_to_mc[m_c__result == "F" | m_c__result == "P"], na.rm = TRUE ), absentees = median(days_to_mc[is.na(m_c__result) | m_c__result == "A"], na.rm = TRUE) )

save(list = ls(), file = "./data/analysis-obj.Rdata")

#this should not be hardcoded, but how to get the fns out of ls()?
my_tables <- list(step_success_table,
                  step_success_prop_table,
                  mc_outcomes,
                  mc_outcomes_prop,
                  we_outcomes,
                  we_outcomes_prop,
                  median_days_to_mc)

my_tables_names <- c("step_success_table",
                    "step_success_prop_table",
                    "mc_outcomes",
                    "mc_outcomes_prop",
                    "we_outcomes",
                    "we_outcomes_prop",
                    "median_days_to_mc")

cat( style( "Save the following tables as CSVs? (y/n)", fg = 208), style(my_tables_names, fg = 069), sep = "\n" )
save_r <- readLines("stdin", 1, warn=FALSE)

if(save_r == "y" | save_r == "Y") {
  for(i in seq(1, length(my_tables))) {
    fpath = paste("./output/", my_tables_names[i], Sys.Date(), ".csv" )
    write.csv(my_tables[i], fpath, row.names = FALSE)
  }
}

#
#end init_analysis
}
