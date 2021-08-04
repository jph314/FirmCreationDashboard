# ---
# Get and clean data for the app
# Yannis Galanakis; <i.galanakis@kent.ac.uk>
# Mar 2021
#---

# Packages ----
packages <- c('tidyverse', 'naniar', 'haven', 'survey',
              'data.table', 'lubridate', 'ggalt', 'cowplot','animation',
              'patchwork', 'sp', 'scales', 'raster', 'rgeos', 'mapproj',
              'rgdal', 'maptools', 'emojifont', 'nord', 'paletteer', 'plotly', 'tibble')
pkg_notinstall <-  packages[!(packages %in% installed.packages()[,"Package"])]

lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
if (!require(gpclib)) install.packages("gpclib", type="source");library(gpclib)
gpclibPermit()  # Gives maptool permisssion to use gpclib
load.fontawesome()

# Download data ----
# Create a temp. file
temp <- tempfile()
# Use `download.file()` to fetch the file into the temp. file
download.file("http://download.companieshouse.gov.uk/BasicCompanyDataAsOneFile-2021-08-01.zip",temp)
# Use unz() to extract the target file from temp. file
repD<- read_csv(unz(temp, "BasicCompanyDataAsOneFile-2021-08-01.csv"))
# Remove the temp file via 'unlink()'
unlink(temp)
# Make incorporation date as date format.
repD$IncorporationDate <- as.Date(repD$IncorporationDate, "%d/%m/%Y")
# keep since 2019
Total192021 <- repD[which(repD$IncorporationDate >= "2019-01-01" &
                            repD$IncorporationDate <= "2021-07-31"), ]
Total192021 <- Total192021[c(2,7,9:15,23:30)]
Total192021 <- Total192021[-c(8,10:13)]
write.csv(Total192021, "data/registerSince2019.csv", row.names=F)
### Make usable dataframe to the app
# delete unnecessary columns
register <- Total192021[-c(2:3, 5:6)]

# SIC codes
# Keep only the code from SIC
register$SIC5dg1 <- as.numeric(gsub("([0-9]+).*$", "\\1", register$SICCode.SicText_1)) # pattern is by finding a set of numbers in the start and capturing them
register$Class   <- register$SIC5dg1/10
register$SIC5dg1 <- as.integer(register$SIC5dg1)
register$Class   <- as.integer(register$Class)
convert <- read.csv('C:/Users/ygala/Dropbox-UKC/Dropbox/FirmsCovid_YG/ReportMarch2021/data/sic2007conversion.csv')
convert <- convert[-c(1)]
# add abbreviation 
abbreviation <- read.csv("data/SectionAbbreviation.csv")
convert <- merge(convert, abbreviation, by="Section")
convert1<-convert[-c(9:10)]
write.csv(convert1, "data/convert.csv", row.names=F)
register <- merge(register, convert1, by="Class")
#register <- register[-c(21:22)]
# Remove duplicates based on week columns
register <- register %>%
  distinct(Class, IncorporationDate, CompanyNumber, .keep_all = TRUE)
write.csv(register, "data/registerSectorsUK.csv", row.names=F)

# make one file with PC, Areas, Country
areaPC <- read.csv("data/Postcodes summaryCLEAN.csv")
# add countries
postcod2country <- read.csv("data/convertedPC2country.csv")
postcod2country <- postcod2country[-c(1)]
postcod2country <- postcod2country %>% rename (PostcodeArea= Postcode.area)
PCcountryArea <- merge(areaPC[ , c("PostcodeArea","Area.covered")],postcod2country,by = "PostcodeArea")
PC2regions <- read_csv("data/PostcodeArea2Regions.csv")
PCcountryArea <- merge(PCcountryArea, PC2regions, by="PostcodeArea")
#save it
write.csv(PCcountryArea, "data/regionsUK.csv", row.names = F)
# by Country
# keep only the first 1 or 2 letters before the numbers in the Postcode
register <- register %>% rename(postcode = RegAddress.PostCode)
register$postcodeArea <- sub("^([[:alpha:]]*).*", "\\1", register$postcode)
PCcountryArea <- PCcountryArea %>% rename (postcodeArea = PostcodeArea)
registerPC <- merge(register, PCcountryArea, by="postcodeArea")
registerPC <- registerPC %>%
  distinct(postcodeArea, IncorporationDate, CompanyNumber,
           Area.covered, Region, Country, .keep_all = TRUE)
registerPC <- registerPC[-c(5, 8:10)]
write.csv(registerPC, "data/registerSectorsRegionsUK.csv", row.names = F)

library(fst)
write_fst(registerPC, "data/registerSectorsRegionsUK.fst", 100)
