#runs the whole kit and kaboodle

.libPaths("C:\\Rpackages")

#initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
  l <- as.character( ls( globalenv() ) )
  cat( "Loaded the following functions:", l, sep = "\n" )
}

#fn to source files stored on Github
source_https <- function(u, unlink.tmp.certs = FALSE) {
    # load package
    require(RCurl)

    # read script lines from website using a security certificate
    if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
    script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
    if(unlink.tmp.certs) unlink("cacert.pem")

    # parase lines and evealuate in the global environement
    eval(parse(text = script), envir= .GlobalEnv)
}

# sequence of script executions
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R")

#sequence of script executions
init("R/lib")
init("R")
init_clean()
init_analysis()
init_plot()

#final communiques
init_comms()
