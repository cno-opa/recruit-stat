#plot.R
#makes charts from analysis data objects

#TODO: make a theme

require(ggplot2)
require(scales)
require(reshape2)

init_plot <- function() {
#
#

load("./data/analysis-obj.Rdata")

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



#
#end init_plot
}
