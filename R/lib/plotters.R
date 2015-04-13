# plotters.R
# A ggplot theme and some helpful functions to make making charts a breeze
#
# Important: Either use the function here, buildChart, to cut up the chart elements are arrange them properly, or include three line breaks ('\n\n\n') in the chart title of your ggplot call
#
#
# This is what is included:
# ==============================
#
# theme_opa : A sweet little diddy.
# buildChart : A function that cuts up and rearranges grobs to get that left-justification we've always dreamed of.
# lineOPA : A generic line chart generator with a few options. See below for details.
# barOPA : A generic histogram generator. Also, see below.
# schigoda : All hail the area chart of might.
# some aesthetic variables used by the chart functions for their color schemes.
#
# ===============================
#
#
# TODO: fn for facet grids, fn to control ggsave defaults

library("grid")
library("gridExtra")
library("gtable")

theme_opa <- function (base_size = 14, base_family = "")
{
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),

    axis.text = element_text(size = rel(0.6), colour = "grey50"),
    strip.text = element_text(size = rel(0.8)),
    axis.line = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(vjust = 1, angle = 45, hjust = .97),
    axis.text.y = element_text(hjust = 1),
    axis.ticks = element_blank(),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),

    legend.background = element_rect(colour = NA),
    legend.margin = unit(0.2, "cm"),
    legend.key = element_rect(colour = NA, fill = "white"),
    legend.key.size = unit(.8, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.75)),
    legend.text.align = NULL,
    legend.title = element_blank(),
    legend.title.align = NULL,
    legend.position = c(0,1),
    legend.direction = "horizontal",
    legend.justification = c(-0.02, 0.8),
    legend.box = NULL,

    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey40"),
    panel.grid.minor = element_blank(),
    panel.margin = unit(0.25, "lines"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,

    strip.background = element_rect(fill = "grey80", colour = NA),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),

    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2), hjust = 0.05),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    complete = TRUE)
}

#aesthetic variables
lightBlue <- "#72d0ff"
darkBlue <- "#005983"
red <- "tomato"

#chart builder!
buildChart <- function(p) {

  gt <- ggplot_gtable(ggplot_build(p))

    gtl <- gt[[1]]

  t <- gtable_filter(gt, "title")[[1]]
  t[[1]]$just <- "left"

  p <- p + theme(legend.position = "none", plot.title = element_blank())

  if( TRUE %in% grepl("guide-box", gtl) ) {
    l <- gtable_filter(gt, "guide-box")[[1]]

    built <- arrangeGrob(
              t[[1]],
              l[[1]],
              p,
              nrow = 3,
              heights = c(1, 1.2, 10)
             )
  } else {
    built <- arrangeGrob(
              t[[1]],
              p,
              nrow = 2,
              heights = c(1, 10)
             )
  }

  return(built)
}

lineOPA <- function(data, x, y, title = "Title!", group = 1, percent = FALSE, last_label = TRUE, ...) {
  # most of the options are passed as dots parameters:
  # set data labels with `labels = "label_column"`
  # set highlight with `highlight = "group_to_highlight"`
  # set y-axis label with `ylab = "label"`
  # set legend labels with `legend.labels = character vector`
  # percent = FALSE refers to whether or not y-axis should be in percent

  dots <- eval(substitute(alist(...)))

  #make y variable continuous
  data[y] <- as.numeric(data[y][[1]])

  #get max y value
  ymax <- max(data[y], na.rm = TRUE)

  #function used by highlighting feature
  remap <- function(input, matcher, value) {
    matcher <- paste0(matcher, "(?! .)")
    i <- grep(matcher, names(input), perl = TRUE)
    input[i] <- value
    return(input)
  }

  if(group == 1) {
    blues <- darkBlue
  } else {
    blues <- colorRampPalette( c(darkBlue, lightBlue) )(nrow(unique(data[group])))
    names(blues) <- as.matrix(unique(data[group]))[,1]
  }

  if( !is.null(dots$highlight) ) {
    blues <- remap(blues, dots$highlight, red)
  }

  #the very basic base
  base <- ggplot(data, aes_string(x = x, y = y, group = group, colour = group)) +
          geom_line(size = 1.5) +
          labs(title = title, x = "", y = "")

  if( !is.null(dots$ylab) ) {
    base <- base + labs(y = dots$ylab)
  }

  if(group == 1) {
    base <- base + geom_line(colour = blues) + guides(colour = FALSE)
  } else {
    base <- base + scale_colour_manual( values = blues )
  }

  if( !is.null(dots$legend.labels) ) {
    legend.labels <- eval(dots$legend.labels)
    base <- base + scale_colour_manual( values = blues, labels = legend.labels )
  }

  if( !is.null(dots$labels) & last_label == TRUE ) {
    #hacky way to get labels data when there is more than one series. pulls all data in df for the named period
    getLabelsData <- function() {
      if(group != 1) {
        d <- data[data[,1] == r_period,]
        return(d)
      } else {
        return(data[nrow(data), ])
      }
    }

    labels_data <- getLabelsData()

    base <- base +
            geom_text(data = labels_data, size = 4, colour = "grey33", hjust = -0.2, aes_string(label = dots$labels, y = y)) +
            scale_x_discrete(expand = c(0, 2.4)) #extend the width of the plot area so label doesn't get cut off
  } else if( !is.null(dots$labels) ) {
    base <- base +
            geom_text(size = 4, colour = "grey33", vjust = -0.5, aes_string(label = dots$labels, y = y))
  }

  if(percent == FALSE) {
    brks <- pretty_breaks(4, min.n = 4)(0:ymax)
    yul  <- brks[length(brks)]
    base <- base + scale_y_continuous(breaks = brks) + expand_limits(y = c(0, yul))
  } else {
    #hack to get pretty_breaks to work for percents. can also use prettyPercentBreaks() in utils
    ymax <- ymax * 100
    brks <- (pretty_breaks(4, min.n = 4)(0:ymax))/100
    for(i in 1:length(brks)) {
      if(brks[i] > 1) {
        brks[i] <- 1
      }
    }
    brks <- unique(brks)
    base <- base + scale_y_continuous(breaks = brks, labels = percent(brks)) + expand_limits(y = c(0, brks[length(brks)]))
  }

  return(base)
}

barOPA <- function(data, x, y, title = "Title", stat = "identity", position = "identity", ...) {
  # set fill with `fill = "variable"` if you have multiple groups
  # set y-axis label with `ylab = "label"`
  # set data labels with `labels = "label_column"`
  # set legend labels with `legend.labels = character vector`

  dots <- eval(substitute(alist(...)))

  #get max y value and set breaks
  ymax <- max(data[y], na.rm = TRUE)
  brks <- pretty_breaks(4, min.n = 4)(0:ymax)
  yul  <- brks[length(brks)]

  #the basic base
  base <- ggplot(data, aes_string(x = x, y = y, fill = dots$fill)) +
          geom_bar(stat = stat, position = position) +
          labs(title = title, y = "", x = "") +
          scale_y_continuous(breaks = brks) + expand_limits(y = c(0, yul))

  if( !is.null(dots$ylab) ) {
    base <- base + labs(y = dots$ylab)
  }

  if( !is.null(dots$labels) ) {
    base <- base +
            geom_text(size = 4, colour = "grey33",  vjust = ifelse(data[y] >= 0, -0.5, 1.5), aes_string(label = dots$labels, y = y))
  }

  if( !is.null(dots$fill) ) {
    blues <- colorRampPalette( c(darkBlue, lightBlue) )(nrow(unique(data[dots$fill])))
    base <- base + geom_bar(stat = stat, position = position) + scale_fill_manual(values = blues)
  } else {
    base <- base + geom_bar(stat = stat, position = position, fill = darkBlue)
  }

  if( !is.null(dots$legend.labels) ) {
    legend.labels <- eval(dots$legend.labels)
    base <- base + scale_fill_manual( values = blues, labels = legend.labels )
  }

  return(base)
}

schigoda <- function(data, x, y, title = "Schigoda!", fill, ...) {
  # fill is not optional on this area chart of might
  # set legend labels with `legend.labels = character vector`

  dots <- eval(substitute(alist(...)))

  blues <- colorRampPalette( c(darkBlue, lightBlue) )(nrow(unique(data[fill])))

  #area
  data[, x] <- as.Date(as.yearmon(data[, x]))
  base <- ggplot(data, aes_string(x = x, y = y, fill = fill)) +
          geom_area(position = "identity") +
          labs(title = title, x = "", y = "") +
          scale_fill_manual(values = blues) +
          scale_x_date(breaks = pretty_breaks(9)(data[, x]), labels = date_format("%b %Y"))

  if( !is.null(dots$legend.labels) ) {
    legend.labels <- eval(dots$legend.labels)
    base <- base + scale_fill_manual( values = blues, labels = legend.labels )
  }

  return(base)
}
