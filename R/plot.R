#plot.R
#makes charts from analysis data objects

#TODO: make a theme

require(ggplot2)
require(scales)
require(reshape2)
require(xtermStyle)
require(lubridate)
require(dplyr)

init_plot <- function() {
#
#

load("./data/analysis-obj.Rdata")
load("./data/geobase.Rdata")

#data transformation for plotting
step_titles <- factor( c("Qualified", "Submitted documents", "Scheduled MC", "Attended MC", "Passed MC", "Scheduled WE", "Attended WE", "Passed WE", "Scheduled Agility", "Attended Agility", "Passed Agility"), levels = c("Qualified", "Submitted documents", "Scheduled MC", "Attended MC", "Passed MC", "Scheduled WE", "Attended WE", "Passed WE", "Scheduled Agility", "Attended Agility", "Passed Agility") )
step_success_prop_table$steps <- step_titles

#relative step success rates
steps <- melt(step_success_prop_table)
ggplot(data = steps, aes(x = steps, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = .97)) +
  scale_fill_manual( name = "Cohorts", values = c("#FF726B","#82ACDB", "#225A98" ) ) +
  scale_y_continuous(labels = percent) +
  labs( title = "Individual step yields", x = "Steps", y = "Success rate" ) +
  ggsave("./output/rel-steps.png", width = 10, height = 5.5)
  cat( style( "Saving individual step yields histogram...", fg = 208) )

#applications and applicant geographies
apps <- as.data.frame( table(d$month_applied) )
  dimnames(apps)[[2]] <- c("month", "applications")
  d <- d[order(ymd(d$date_applied)),]
  m_order <- unique(d$month_applied)
  m_order <- append(m_order, c("Jun 2013", "Jul 2013", "Aug 2013", "Sep 2013", "Oct 2013", "Nov 2013"), after = 0) #add in historical months
  h <- data.frame( month = c("Jun 2013", "Jul 2013", "Aug 2013", "Sep 2013", "Oct 2013", "Nov 2013"), applications = c(44,45,36,26,44,70)) #actual historical data
  apps <- rbind(h, apps)
  apps$month <- factor(apps$month, levels = m_order)
  apps <- arrange(apps, month)

  #project apps for incomplete month
  project_apps <- function() {
    last <- as.character(apps$month[nrow(apps)])
    last_ndays <- ymd( paste(
                      strsplit(last, " ")[[1]][2],
                      month_str2num(strsplit(last, " ")[[1]][1]),
                      days_in_month(month_str2num(strsplit(last, " ")[[1]][1])),
                      sep = "-") )
    last_measured <- max(ymd(d$date_applied))

    if( (last_ndays - last_measured) > 5 ) {
      cat("Projecting applications for latest month...")
      ratio <- as.numeric(format(last_measured, "%d"))/as.numeric(format(last_ndays, "%d"))
      projection <- round(apps$applications[nrow(apps)]/ratio)
      apps$applications[nrow(apps)] <- projection
      levels(apps$month)[levels(apps$month) == last] <- paste(last, "(projected)", sep = " ")
    }
    return(apps)
  }

  #run projection
  apps <- project_apps()

ggplot(data = apps, aes(x = month, y = applications, group = 1, label = applications)) +
  geom_line( colour = "#225A98", size = 1) +
  geom_text( size = 3, vjust = -.9, hjust = 1 ) +
  theme(axis.text.x = element_text(angle = 45, hjust = .97)) +
  labs( title = "Applications by month", x = "Month", y = "Applications" ) +
  ggsave("./output/apps.png", width = 10, height = 5.5)
  cat( style( "Saving application line chart...", fg = 208) )

#geography of applicants


#
#end init_plot
}
