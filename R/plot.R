#plot.R
#makes charts from analysis data objects

#TODO: PROFIT

init_plot <- function() {
#
#

load("./data/analysis-obj.Rdata")
load("./data/geobase.Rdata")

#data transformation for plotting
step_names <- factor( c("Applied",
                        "Qualified",
                        "Submitted documents",
                        "Scheduled MC",
                        "Attended MC",
                        "Passed MC",
                        "Scheduled WE",
                        "Attended WE",
                        "Passed WE",
                        "Scheduled Agility",
                        "Attended Agility",
                        "Passed Agility"),
                        levels = c("Applied",
                        "Qualified",
                        "Submitted documents",
                        "Scheduled MC",
                        "Attended MC",
                        "Passed MC",
                        "Scheduled WE",
                        "Attended WE",
                        "Passed WE",
                        "Scheduled Agility",
                        "Attended Agility",
                        "Passed Agility")
                      )
steps$step <- as.factor(steps$step)
steps$step <- step_names
steps$period <- properize(steps$period)

#charts

theme_set(theme_opa())

# step_hist <- function() {
#   ggplot(data = steps[steps$step != "Applied",], aes(x = step, y = prop, fill = period)) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
#     theme(axis.text.x = element_text(angle = 45, hjust = .97)) +
#     scale_fill_manual( name = "Cohorts", values = c("#FF726B","#82ACDB", "#225A98" ) ) +
#     labs( title = "Individual step yields", x = "Steps", y = "Success rate" ) +
#     ggsave("./output/rel-steps.png", width = 10, height = 5.5)
#     cat("Saving individual step yields histogram...\n")
# }
step_hist <- function() {
  p <- barOPA(data = steps[steps$step != "Applied",], "step", "prop", "Individual step yields", fill = "period", position = "dodge", percent = TRUE)
  p <- buildChart(p)
  ggsave("./output/step-yields.png", plot = p, width = 7.42, height = 5.75)
}

apps <- function() {
  project_apps <- function() { #project apps for incomplete month
    last <- as.character(apps$month[nrow(apps)])
    last_ndays <- ymd( paste(
                      strsplit(last, " ")[[1]][2],
                      monthStrToNum(strsplit(last, " ")[[1]][1]),
                      days_in_month(monthStrToNum(strsplit(last, " ")[[1]][1])),
                      sep = "-") )
    last_measured <- max(ymd(d$date_applied))

    if( (last_ndays - last_measured) > 5 ) {
      cat("Projecting applications for latest month...\n")
      ratio <- as.numeric(format(last_measured, "%d"))/as.numeric(format(last_ndays, "%d"))
      projection <- round(apps$applications[nrow(apps)]/ratio)
      apps$applications[nrow(apps)] <- projection
      levels(apps$month)[levels(apps$month) == last] <- paste(last, "(projected)", sep = " ")
    }
    return(apps)
  }

  apps <- as.data.frame( table(d$month_applied) )
  dimnames(apps)[[2]] <- c("month", "applications")
  d <- d[order(ymd(d$date_applied)),]
  m_order <- unique(d$month_applied)
  m_order <- append(m_order, c("Jun 2013", "Jul 2013", "Aug 2013", "Sep 2013", "Oct 2013", "Nov 2013"), after = 0) #add in historical months
  h <- data.frame( month = c("Jun 2013", "Jul 2013", "Aug 2013", "Sep 2013", "Oct 2013", "Nov 2013"), applications = c(44,45,36,26,44,70)) #actual historical data
  apps <- rbind(h, apps)
  apps$month <- factor(apps$month, levels = m_order)
  apps <- arrange(apps, month)
  apps <- project_apps() #run projection

  apps_d <- filter(d, date_applied > (max(date_applied) - days(62)))%>%
            group_by(date_applied)%>%
            summarise(n = n())

  #monthly apps
  # ggplot(data = apps, aes(x = month, y = applications, group = 1, label = applications)) +
  #   geom_line( colour = "#225A98", size = 1 ) +
  #   geom_text( size = 3, vjust = -.9, hjust = 1 ) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = .97)) +
  #   labs( title = "Applications by month", x = "Month", y = "Applications" ) +
  #   ggsave("./output/apps.png", width = 10, height = 5.5)
  #   cat("Saving application line chart...\n")

  p_apps <- lineOPA(apps, "month", "applications", "Applications by month", labels = "applications", last_label = FALSE)
  p_apps <- buildChart(p_apps)
  ggsave("./output/apps-monthly.png", plot = p_apps, width = 7.42, height = 5.75)

  #daily apps
  # ggplot(data = apps_d, aes(x = date_applied, y = n, group = 1)) +
  #   geom_line( colour = "#225A98", size = 1 ) +
  #   geom_hline( aes(yintercept = mean(n)), colour = "#FF726B", linetype = "dashed" ) +
  #   labs(title = "Applications by day", x = "Day", y = "Applications") +
  #   ggsave("./output/apps-daily.png", width = 10, height = 5.5)
  #   cat("Saving daily application line chart...\n")
  p_d <- lineOPA(apps_d, "date_applied", "n", "Applications by day")
  p_d <- buildChart(p_d)
  ggsave("./output/apps-daily.png", plot = p_d, width = 7.42, height = 5.75)
}

geos <- function() {
  #histogram
  periods <- unique(steps$period)[unique(steps$period) != "Baseline"]
  for(p in periods) {
    add <- filter(d, month_applied == p) %>%
            group_by(geo) %>%
            summarise(period = p, count = n())
    add$prop <- add$count/sum(add$count)
    t <- rbind_list(t, add)
  }

  t$period <- properize(t$period)
  t$period <- factor(t$period, levels = unique(t$period) )
  tf <- filter(t, geo == "GNO" | geo == "New Orleans" | geo == "LA" | geo == "TX" | geo == "MS" | geo == "FL")
  tf$geo <- factor(tf$geo, levels = c("New Orleans", "GNO", "LA", "MS", "TX", "FL"))

  ggplot(data = tf,
    aes(x = geo, y = prop, fill = period)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs( title = "Applicant geography", x = "Geography", y = "Proportion of applicantions" ) +
    scale_fill_manual( name = "Cohorts", values = c("#FF726B","#82ACDB", "#225A98" ) ) +
    ggsave("./output/geos.png", width = 10, height = 5.5)
    cat("Saving applicant geo histogram...\n")

  #line
  l <- ymd(paste(
       strsplit(periods[2], " ")[[1]][2],
       monthStrToNum(strsplit(periods[2], " ")[[1]][1]),
       "01",
       sep = "-"
       ))

  w <- filter(d, date_applied > l)%>%
       group_by(geo, period = paste( year(date_applied), week(date_applied), sep="-" ) ) %>%
       summarise(n = n()) %>%
       arrange(period)
  w$period <- as.factor(w$period)
  wf <- filter(w, geo == "GNO" | geo == "New Orleans" | geo == "LA" | geo == "TX" | geo == "MS" | geo == "FL")
  ggplot(wf, aes(x = period, y = n, group = geo, colour = geo)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 45, hjust = .97)) +
    labs( title = "Applicant geography", x = "Year and week", y = "Count of applications" ) +
    scale_colour_discrete(name = "Geographies")
    ggsave("./output/geos-line.png", width = 10, height = 5.5)
    cat("Saving applicant geo line chart...\n")
}

cs_exams <- function() {
  d <- filter(mc_outcomes, !is.na(attended))
  l <- nrow(d)
  d <- d[(l-10):l,] %>%
       group_by(written_test) %>%
       summarise(pcnt_attended = (attended/scheduled), pcnt_passed = (passed/attended)) %>%
       melt(id.vars = "written_test")

  d$written_test <- factor(format(d$written_test, "%b %d %Y"), levels = format(d$written_test, "%b %d %Y"))

  p <- barOPA(d, "written_test", "value", "Multiple choice exam", fill = "variable", position = "dodge", percent = TRUE, legend.labels = c("Attended", "Passed"))
  p <- buildChart(p)
  ggsave("./output/mc-exam-outcomes.png", plot = p_d, width = 7.42, height = 5.75)
}

#execution
step_hist()
apps()
#geos()

#
#end init_plot
}
