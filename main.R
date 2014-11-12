#!usr/bin/Rscript

# runs the whole kit and kaboodle

#initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
}
