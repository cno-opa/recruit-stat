#analysis.R
#performs various analyses on cleaned data

#TODO: make tables for: individual step yields (month), CS pipeline, applicantion rates and geographies, CS timeliness, exam attendance rates

init_analysis() {
  #
  #

  load("./data/master.Rdata")

  #create subsets of successful groups at each step
  qual <- subset(d, disposition != "Not qualified")
  docs <- subset (qual, disposition != "Documents Needed")
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
                      passmMC = mc_pass,
                      scheduleWE = we,
                      attendWE = we_attend,
                      passWE = we_pass,
                      scheduleAgility = agil,
                      attendAgility = agil_attend,
                      passAgility = agil_pass)

  #summarize step yield size and proportion by time period
  success_table <- function(l, lower_limit = "2013-12-01", upper_limit = "2014-10-31") {
    s <- subset( l, ymd(l$date_applied) > ymd(lower_limit) & ymd(l$date_applied) < ymd(upper_limit) )
    nrow(s)
  }


  #fn calls
  #step yields
  step_baseline <- sapply(step_yields, success_table, lower_limit = "2014-01-01", upper_limit = "2014-06-01")

  #
  #
}
