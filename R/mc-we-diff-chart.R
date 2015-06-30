diff <- read.csv("./data/mc-we-diff.csv")
diff$date_n <- 1:nrow(diff)
diff <- diff[1:(nrow(diff) - 1),]

theme_set(theme_opa())

p <- ggplot(diff, aes(x = date_n)) +
     scale_x_continuous(breaks = diff$date_n, labels = diff$date)

p <- p + geom_line(aes(y = pass_mc, group = 1, colour = "Pass MC"), size = 1.2) +
         geom_line(aes(y = pass_we, group = 1, colour = "Pass WE"), size = 1.2) +
         geom_ribbon(aes(ymax = pass_mc, ymin = (pass_mc - fail_we), fill = "Failed WE")) +
         geom_ribbon(aes(ymax = (pass_mc - fail_we), ymin = ((pass_mc - fail_we) - absent_we), fill = "Absent WE")) +
         geom_ribbon(aes(ymax = (pass_we + we_later), ymin = pass_we, fill = "Scheduled WE later")) +
         labs(title = "Civil Service MC and WE test output", y = "", x = "") +
         scale_y_continuous(breaks = c(0, 20, 40, 60, 80), limits = c(0, 80)) +
         scale_colour_manual(values = c("black", "gray33"), labels = c("Pass MC", "Pass WE")) +
         scale_fill_manual(values = c("#FF6347","#FFA38F", "#005983"), breaks = c("Failed WE", "Absent WE", "Scheduled WE later")) +
         theme(legend.text = element_text(size = rel(0.75)), plot.title = element_blank(), legend.justification = c(0, 0.5))

p <- buildChart(p)

ggsave("./output/mc-we-diff.png", plot = p, width = 7.42, height = 5.75)
