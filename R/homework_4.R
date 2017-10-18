install.packages("foreign")
library(foreign)

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

