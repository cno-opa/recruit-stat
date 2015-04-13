#analysis.R
#performs various analyses on cleaned data

#TODO: Remove hacks in countSuccess() and ln 159

require(lubridate)
require(dplyr)

init_analysis <- function() {
#
#

#utility functions
monthStrToNum <- function(str) {
  str <- tolower(str)
  months <- list("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  match(str, months)
}

properize <- function(str) {
  f <- toupper( substr(str, 1, 1) )
  r <- substr(str, 2, nchar(str))
  return( paste0(f, r) )
}

#count individual step yields
countSuccess <- function(data, period) {
  if(period == "baseline") {
    l <- ymd("2014-01-01")
    u <- ymd("2014-06-01")
    period_name <- "baseline"
  } else {
    l <- ymd(paste(
              year(period),
              month(period),
              "01",
              sep = "-"
            ))

    u <- ymd(paste(
              year(period),
              month(period),
              days_in_month(period),
              sep = "-"
            ))

    period_name <- paste(
                    month(period, label = TRUE),
                    year(period),
                    sep = " "
                    )
  }

  all         <-  filter(data, date_applied >= l, date_applied <= u)
  qual        <-  filter(data, date_applied >= l, date_applied <= u, disposition != "not qualified")
  docs        <-  filter(qual, date_applied >= l, date_applied <= u, disposition != "documents needed")
  mc          <-  filter(docs, date_applied >= l, date_applied <= u, !is.na(written_test))
  mc_actual   <-  filter(mc, written_test <= max(data$date_applied))
  mc_attend   <-  filter(mc, date_applied >= l, date_applied <= u, m_c__result == "P" | m_c__result == "F")
  mc_pass     <-  filter(mc_attend, date_applied >= l, date_applied <= u, m_c__result == "P")
  we          <-  filter(mc_pass, date_applied >= l, date_applied <= u, !is.na(writing_exercise))
  we_actual   <-  filter(we, writing_exercise <= max(data$date_applied))
  we_attend   <-  filter(we, date_applied >= l, date_applied <= u, w_e__result == "P" | w_e__result == "F")
  we_pass     <-  filter(we_attend, date_applied >= l, date_applied <= u, w_e__result == "P")
  agil        <-  filter(we_pass, date_applied >= l, date_applied <= u, !is.na(agility_test))
  agil_actual <-  filter(agil, agility_test <= max(data$date_applied))
  agil_attend <-  filter(agil, date_applied >= l, date_applied <= u, agility_result == "P" | agility_result == "F")
  agil_pass   <-  filter(agil_attend, date_applied >= l, date_applied <= u, agility_result == "P")

  rbind(
    summarise(all, step = "applied", period = as.character(period_name), count = n(), prop = NA),
    summarise(qual, step = "qualified", period = as.character(period_name), count = n(), prop = count/nrow(all)),
    summarise(docs, step = "docs submitted", period = as.character(period_name), count = n(), prop = count/nrow(qual)),
    summarise(mc, step = "scheduled mc", period = as.character(period_name), count = n(), prop = count/nrow(docs)),
    summarise(mc_attend, step = "attended mc", period = as.character(period_name), count = n(), prop = count/nrow(mc_actual)),
    summarise(mc_pass, step = "passed mc", period = as.character(period_name), count = n(), prop = count/nrow(mc_attend)),
    summarise(we, step = "scheduled we", period = as.character(period_name), count = n(), prop = count/nrow(mc_pass)),
    summarise(we_attend, step = "attended we", period = as.character(period_name), count = n(), prop = count/nrow(we_actual)),
    summarise(we_pass, step = "passed we", period = as.character(period_name), count = n(), prop = count/nrow(we_attend)),
    summarise(agil, step = "scheduled agility", period = as.character(period_name), count = n(), prop = count/nrow(we_pass)),
    if( nrow(agil_attend) == 0 ) {
      c("attended agility", as.character(period_name), 0, 0)
    } else {
      summarise(agil_attend, step = "attended agility", period = as.character(period_name), count = n(), prop = count/nrow(agil_actual))
    },
    if( nrow(agil_pass) == 0 ){
      c("passed agility", as.character(period_name), 0, 0)
    } else {
      summarise(agil_pass, step = "passed agility", period = as.character(period_name), count = n(), prop = count/nrow(agil_pass))
    }
  )
}

#misc tables
mc_outcomes <- function() {
  s <- filter(d, !is.na(written_test))%>%
        group_by(written_test)%>%
        summarise(scheduled = n())
  a <- filter(d, m_c__result == "P" | m_c__result == "F")%>%
        group_by(written_test)%>%
        summarise(attended = n())
  p <- filter(d, m_c__result == "P")%>%
        group_by(written_test)%>%
        summarise(passed = n())

  left_join(s, a, by = "written_test")%>%
    left_join(p, by = "written_test")
}

we_outcomes <- function() {
  s <- filter(d, !is.na(writing_exercise))%>%
        group_by(writing_exercise)%>%
        summarise(scheduled = n())
  a <- filter(d, w_e__result == "P" | w_e__result == "F")%>%
        group_by(writing_exercise)%>%
        summarise(attended = n())
  p <- filter(d, w_e__result == "P")%>%
        group_by(writing_exercise)%>%
        summarise(passed = n())

  left_join(s, a, by = "writing_exercise")%>%
    left_join(p, by = "writing_exercise")
}

mc_median <- function() {
  s <- filter(d, !is.na(written_test))%>%
        group_by(written_test)%>%
        summarise(scheduled = median(days_to_mc))
  a <- filter(d, m_c__result == "P" | m_c__result == "F")%>%
        group_by(written_test)%>%
        summarise(attended = median(days_to_mc))
  p <- filter(d, m_c__result == "P")%>%
        group_by(written_test)%>%
        summarise(passed = median(days_to_mc))

  left_join(s, a, by = "written_test")%>%
    left_join(p, by = "written_test")
}

#load
load("./data/master.Rdata")

#set cohort months
mx <- day( max(ymd(d$date_applied)) )
cutoff <- (days_in_month(month(max(d$date_applied)))) - 7
if(mx > cutoff) {
  current <- max(ymd(d$date_applied))
} else {
  current <- seq(max(ymd(d$date_applied)), length = 2, by = "-1 month")[2]
}
prev <- seq(current, length = 2, by = "-1 month")[2]

#execute
steps <- rbind(
                countSuccess(d, "baseline"),
                countSuccess(d, prev),
                countSuccess(d, current)
              )
steps$prop <- as.numeric(steps$prop) #hack
mc_outcomes <- mc_outcomes()
we_outcomes <- we_outcomes()
mc_median <- mc_median()

#save
cat("Saving data tables.. \n")
save(list = ls(), file = "./data/analysis-obj.Rdata")
write.csv(steps, paste("./output/steps", Sys.Date(), ".csv", sep = "-"), row.names = FALSE)
write.csv(mc_outcomes, paste("./output/mc-outcomes", Sys.Date(), ".csv", sep = "-"), row.names = FALSE)
write.csv(we_outcomes, paste("./output/we-outcomes", Sys.Date(), ".csv", sep = "-"), row.names = FALSE)
write.csv(mc_median, paste("./output/mc-median", Sys.Date(), ".csv", sep = "-"), row.names = FALSE)

#
#end init_analysis
}
