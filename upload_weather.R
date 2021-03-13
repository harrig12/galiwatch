# Process weatherstation data and upload minutes to google sheets

library(tidyverse)
library(lubridate)
library(googlesheets4)
sheets_deauth()

# require data file
args = commandArgs(trailingOnly=TRUE)
if (length(args)!=1) {
  stop("exactly one argument must be supplied (expected data csv)", call.=FALSE)
} 

# check data file exists
if (!file.exists(args[1])){
  stop(paste0("file ", args[1], " not found"), call.=FALSE)
}

# get sheet id 
id <- readLines('.sheet_id')

# parse data
data <- read_csv(args[1], n_max = inf)


# upload to google
