# Analysis and plotting for Police Recruit STAT

## To do

 * Figure out geocall conundrum. Use historical data maybe?
 * Is passing data through master .Rdata file?

## Usage

Make sure all the packages listed in __Dependencies__ (see below) are installed on your machine and then run `main.R`.

__How it works:__ `main.R` looks in the `R` subdirectory and sources all the R files in there. It then executes those files in a sequence explicitly declared in `main.R`. If you extend this bundle of scripts, make sure you add a call to execute any new files you add to `R` or any other subdirectory, and execute these new scripts _after_ any scripts they depend on.

After the `clean.R` cleans and transforms the data for use, the data object is saved as `data/master.Rdata`. Any extensions should access that data file and not overwrite it. If you need to modify data and save it to be accessed by other scripts, please save the file as something else. I know this isn't the most robust way of maintaining data state, but it's simpler than passing data objects between scripts in `R` and `main.R`.

## Dependencies

 * lubridate
 * gdata
 * stringr
 * jsonlite
 * RCurl
