library(foreign)
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(maps)
getwd()

person_99 <- foreign::read.dbf("data-raw/yearly_person_data/person_1999.dbf")
person_00 <- foreign::read.dbf("data-raw/yearly_person_data/person_2000.dbf")
person_01 <- foreign::read.dbf("data-raw/yearly_person_data/person_2001.dbf")
person_02 <- foreign::read.dbf("data-raw/yearly_person_data/person_2002.dbf")
person_03 <- foreign::read.dbf("data-raw/yearly_person_data/person_2003.dbf")
person_04 <- foreign::read.dbf("data-raw/yearly_person_data/person_2004.dbf")
person_05 <- foreign::read.dbf("data-raw/yearly_person_data/person_2005.dbf")
person_06 <- foreign::read.dbf("data-raw/yearly_person_data/person_2006.dbf")
person_07 <- foreign::read.dbf("data-raw/yearly_person_data/person_2007.dbf")
person_08 <- foreign::read.dbf("data-raw/yearly_person_data/person_2008.dbf")
person_09 <- foreign::read.dbf("data-raw/yearly_person_data/person_2009.dbf")
person_10 <- foreign::read.dbf("data-raw/yearly_person_data/person_2010.dbf")

# Use the following code only if you downloaded .dbf files by hand, instead of
# writing them out as .csv files using R. The fars_dbf_to_csv function reads
# in .dbf files as data frames, and then writes them as csv files to the
# "data-raw/yearly_person_data" directory. The `map` step iterates the
# function across all of the .dbf files saved in the
# "data-raw/yearly_person_data" directory.
fars_dbf_to_csv <- function(year) {
  # Save the directory where .dbf files are saved.
  dir <- "data-raw/yearly_person_data"
  # Read the .dbf file for a year into R.
  person_data <- foreign::read.dbf(paste0(dir,"/PERSON_", year, ".DBF"))
  # Save each file as a csv to the "data-raw/yearly_person_data" directory. 
  person_file <- paste0("data-raw/yearly_person_data/person_", year, ".csv") 
  readr::write_csv(person_data, path = person_file)
# Return NULL so that the function doesn't print out anything.
return(NULL) }
# Iterate the fars_dbf_to_csv across all files.
purrr::map(1999:2010, fars_dbf_to_csv)
