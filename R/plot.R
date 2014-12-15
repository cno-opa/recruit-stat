#plot.R
#makes charts from analysis data objects

#TODO: relevel prop table steps in correct order (see: http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/)

require(ggplot2)
require(reshape2)

init_plot <- function() {
#
#

load("./data/analysis-obj.Rdata")

#relative step success rates

steps <- melt(step_success_prop_table)
ggplot(data = steps, aes(x = steps, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge())


#
#end init_plot
}
