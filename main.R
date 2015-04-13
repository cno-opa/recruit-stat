#runs the whole kit and kaboodle

.libPaths("C:\\Rpackages")

#initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
  l <- as.character( ls( globalenv() ) )
  cat( "Loaded the following functions:", l, sep = "\n" )
}

#sequence of script executions
init("R/lib")
init("R")
init_clean()
init_analysis()
init_plot()

#final communiques
init_comms()
