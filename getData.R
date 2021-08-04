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
# add abbreviation of section names as in https://www.hithorizons.com/uk/companies/stats-and-charts
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
areaPC <- read.csv("C:/Users/ygala/Dropbox-UKC/Dropbox/FirmsCovid_YG/ReportMarch2021/data/Postcodes summaryCLEAN.csv")
# add countries
postcod2country <- read.csv("C:/Users/ygala/Dropbox-UKC/Dropbox/FirmsCovid_YG/ReportMarch2021/data/convertedPC2country.csv")
postcod2country <- postcod2country[-c(1)]
postcod2country <- postcod2country %>% rename (PostcodeArea= Postcode.area)
PCcountryArea <- merge(areaPC[ , c("PostcodeArea","Area.covered")],postcod2country,by = "PostcodeArea")
PC2regions <- read_csv("C:/Users/ygala/Dropbox-UKC/Dropbox/FirmsCovid_YG/UKmapPC/PostcodeArea2Regions.csv")
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

## Aggregate
n_incorp <- f %>%
  group_by(IncorporationDate) %>%
  count()

f<-Total192021[which(Total192021$IncorporationDate >= "2020-07-01" &
                       Total192021$IncorporationDate <= "2020-11-01"), ]

aggregate(n_incorp$n, by=list(IncorporationDate=n_incorp$IncorporationDate), FUN=sum)

#rolling avgs
RollAvgs <-n_incorp
RollAvgs$av3 <- frollmean(RollAvgs$n, n=3)
RollAvgs$av7 <- frollmean(RollAvgs$n, n=7)


# AGGREGATE STATS MAP
n_iPC <- registerPC %>%
  group_by(postcode) %>%
  count()


pc2nuts <- read.csv("C:/Users/ygala/Dropbox-UKC/Dropbox/postcodes2NUTS/output/UKpc2NUTS.csv")
pc2nuts <- pc2nuts %>% rename(postcode = postcodeUnit)
pcReg <- pc2nuts %>%
  left_join(n_iPC, by = c("postcode"))
pcReg <- pcReg %>% distinct(postcode, .keep_all = TRUE)

#measure by nuts2
n_pcReg <- pcReg %>%
  group_by(NUTS218CD) %>%
  summarise(n = sum(n, na.rm=TRUE))
n_pcReg <- n_pcReg %>% left_join(pc2nuts, by = c("NUTS218CD"))
n_pcReg <- n_pcReg %>% distinct(NUTS218CD, .keep_all = TRUE)

#https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=name&tags=all(PRD_ONSPD%2CFEB_2021)
postcodes <- read.csv("data/ukpostcodes.csv")
postcodes <- postcodes[-c(1)]
leaflet(postcodes) %>% addCircles(lng = ~longitude, lat = ~latitude)





# sectoral
sectorsIwant <- c(11:15,293)
Tfirmsfilt2 <- registerPC[which(registerPC$Group %in% sectorsIwant),]

n_incorp <- Tfirmsfilt2 %>%
  group_by(IncorporationDate, Group) %>%
  count()

n_incorp$av7 <- frollmean(n_incorp$n, n=7)

n_incorp <- left_join(n_incorp,convert,by="Group")
# Remove duplicates based on week columns
n_incorp <- n_incorp %>%
  distinct(IncorporationDate, Group, .keep_all = TRUE)

sec <- unique(n_incorp$Section)

plot_ly(x=n_incorp$IncorporationDate, y= n_incorp$av7, linetype = n_incorp$Group.name, mode = 'lines')
