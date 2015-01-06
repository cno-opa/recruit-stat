#!usr/bin/Rscript
# runs the whole kit and kaboodle

require(xtermStyle)

#initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
  l <- as.character( ls( globalenv() ) )
  cat( style( "Loaded the following functions:", fg = 208), style(l, fg = 069), sep = "\n" )
}

#sequence of script executions
init("R")
init_clean()
init_analysis()
#init_plot()

#final communiques
init_comms()
