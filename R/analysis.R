#analysis.R
#performs various analyses on cleaned data

#TODO: calculate proportion table in non-naive manner for mc test attendance


init_analysis <- function() {
  #
  #

  load("./data/master.Rdata")

  #create subsets of successful groups at each step
  qual <- subset(d, disposition != "not qualified")
  docs <- subset (qual, disposition != "documents needed")
  mc <- subset (docs, !is.na(written_test))
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

  #summarize step yield size and proportion by time period
  success_table <- function(l, lower_limit = "2013-12-01", upper_limit = max(ymd(d$date_applied))) {
    s <- subset( l, ymd(l$date_applied) >= ymd(lower_limit) & ymd(l$date_applied) <= ymd(upper_limit) )
    nrow(s)
  }

  #step yields -- note: this calculates three points in time to compare: the baseline and the previous two complete months
  make_step_table <- function() {
    #set months to compare to baseline
    mx <- day( max(ymd(d$date_applied)) )

    if(mx > 25) {
      current <- month( max(ymd(d$date_applied)) )
    } else {
      current <- month( max(ymd(d$date_applied)) ) - 1
    }

    prev <- current - 1

    #set lower and upper limits for prev and current
    current_l <- paste( year(max(ymd(d$date_applied))), current, "01", sep = "-" )
    current_u <- paste( year(max(ymd(d$date_applied))), current, days_in_month(current), sep = "-" )

    prev_l <- paste( year(max(ymd(d$date_applied))), prev, "01", sep = "-" )
    prev_u <- paste( year(max(ymd(d$date_applied))), prev, days_in_month(prev), sep = "-" )

    #get tables of successful applicants for each step for each time period
    step_baseline <- as.data.frame(sapply(step_yields, success_table, lower_limit = "2014-01-01", upper_limit = "2014-05-31"))
      colnames(step_baseline) <- "Baseline"
    step_prev <- as.data.frame(sapply(step_yields, success_table, lower_limit = prev_l, upper_limit = prev_u))
      colnames(step_prev) <- month(prev, label = TRUE)
    step_current <- as.data.frame(sapply(step_yields, success_table, lower_limit = current_l, upper_limit = current_u))
      colnames(step_current) <- month(current, label = TRUE)

    #combine
    step_success_table <- cbind(step_baseline, step_prev, step_current)

    return(step_success_table)
  }

  make_step_prop_table <- function(x) {
      r <- nrow(x)
      calc_prop <- function(col) {
        l <- list()
        for(i in seq(2, r, 1)) {
          prop <- col[i]/col[i-1]
          l <- append(l, prop)
        }
        return(l)
      }
      t <- as.data.frame(sapply(x, calc_prop))
      row.names(t) <- row.names(step_success_table[2:12,])
      return(t)
  }

  #fn calls
  step_success_table <- make_step_table()
  step_success_prop_table <- make_step_prop_table(step_success_table)


  #
  #
}
