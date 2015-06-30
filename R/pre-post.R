prePost <- function(event, n_days = 21) {
  # event: date string in yyyy-mm-dd
  # n_days: number of days to look pre and post
  #
  #
  require(lubridate)
  require(dplyr)
  load("./data/master.Rdata")
  theme_set(theme_opa())

  event <- as.Date(event)

  l <- event - n_days
  u <- event + n_days
  pre <- filter(d, date_applied >= ymd(l) & date_applied < ymd(event))
  post <- filter(d, date_applied > ymd(event) & date_applied <= ymd(u))

  pre_t <- data.frame(table(pre$date_applied), var = "pre")
  post_t <- data.frame(table(post$date_applied), var = "post")
  names(pre_t) <- c("date", "apps", "var")
  names(post_t) <- c("date", "apps", "var")

  t <- rbind(pre_t, post_t)

  d <- group_by(t, var) %>%
       summarise( mean = mean(apps), sd = sd(apps), max = (mean(apps) + sd(apps)) )

  # bar chart
  ymax <- max(d$max) + max(d$max)/6
  brks <- pretty_breaks(4)(0:ymax)
  limits <- aes(ymax = mean + sd, ymin = mean - sd)
  pb <- barOPA(d, "var", "mean", "Mean daily applications") +
        geom_errorbar(limits, width = 0.25) +
        scale_y_continuous(limits = c(0, brks[length(brks)]), breaks = brks)

  pb <- buildChart(pb)

  # density chart
  pd <- ggplot(t, aes(x = apps, fill = var)) +
        geom_density(alpha = 0.4) +
        labs(title = "Density distribution", y = "Density", x = "Number of applications") +
        scale_fill_manual(values = c("tomato", "cornflowerblue"), labels = c("Pre", "Post")) +
        theme(legend.title = element_blank(),
              legend.position = c(0,1),
              legend.direction = "horizontal",
              legend.justification = c(-0.05, 0.8),
              plot.title = element_text(size = rel(1.2), hjust = 0.05)
              )

  # save
  ggsave( paste("./output/pre-post-bar-", event, "-", n_days, ".png", sep = ""), plot = pb, width = 7.42, height = 5.75 )
  theme_set(theme_minimal())
  pd <- buildChart(pd)
  ggsave( paste("./output/pre-post-density-", event, "-", n_days, ".png", sep = ""), plot = pd, width = 7.42, height = 5.75 )

  # reset theme
  theme_set(theme_opa())
  #
  #
}
