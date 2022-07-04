# Download latest register ----
# Create a temp. file.
# temp <- tempfile()
# # Use `download.file()` to fetch the file into the temp. file.
# download.file("http://download.companieshouse.gov.uk/BasicCompanyDataAsOneFile-2022-05-01.zip",temp)
# # Unzip and read csv file.
# repD <- vroom(temp)
# # Remove the temp file via 'unlink()'
# unlink(temp)
# # Make incorporation date as date format.
# repD <- repD %>% rename(date=IncorporationDate)
# repD$date <- as.Date(repD$date, "%d/%m/%Y")
# repD$SIC5 <- as.integer(gsub("([0-9]+).*$", "\\1", repD$SICCode.SicText_1)) # pattern is by finding a set of numbers in the start and capturing them
# ### Make usable dataframe for the app.
# # Delete unnecessary columns.
# register <- repD[which(repD$date >= "2019-01-01" &
#                             repD$date <= "2022-04-30"), c(2,10,15,56)]
# register <- register %>% rename(postcode = RegAddress.PostCode)
# rm(repD)

# Archive data ----
# archive <- readRDS("data/incorporations_archive_byMonthPostcodeSIC5.rds")
# archive <- archive %>% rename(date = IncorporationDate)

# Harmonise the dataframes ----
# register$archive <- "Latest register"
# archive$archive <- "Archive"
# archive <- archive %>% select(!postcode2)
# register <- rbind(register, archive)
# rm(archive)

# # SIC codes ----
# # Keep only the code from SIC.
# register$Class <- as.integer(register$SIC5/10)
# # Overseas companies are not supplied with SIC codes. They are therefore lost in the next step.
# # Read SIC conversion file and merge to obtain Section, Division, Group and Class.
# convert1 <- fread("data/convertSIC.csv")
# register <- merge(register, convert1, by="Class")
# 
# # LA Districts ----
# # Read postcode conversion file and merge to obtain LA District, County (in England and Wales only) and Country.
# postcodes <- fread("data/postcodes.csv") %>% rename(postcode = Postcode)
# register$postcode <- gsub("[ .]", "", register$postcode)
# registerLA <- merge(register, postcodes, by="postcode", all.x = T)
# registerLA$District[is.na(registerLA$District)] <- "" 

# #Save full register if required.
# saveRDS(registerLA, "data/registerClassLA.rds")

# Aggregation ----
register2 <- registerLA %>% 
  group_by(date, Class, District, archive) %>%
  count() %>%
  as.data.table()
# Restore SIC and regional data
register2 <- merge(register2, convert1, by="Class")
convert2 <- fread("data/convertLA.csv")
register2 <- merge(register2, convert2, by="District")
# Save ready for use.
# saveRDS(register2, "data/registerAgg2.rds")


# Load pre-aggregated data
register1 <- readRDS("data/registerAgg2.rds")

# 2019 data for comparison
daily2019 <- fread("data/Daily2019.csv")

# prepare dissolution data ----
# dissolutions <- readRDS("data/dissolutions_snapshot.rds")
#
# dissolutions$Class <- as.integer(as.integer(dissolutions$SIC.1)/10)
# # Overseas companies are not supplied with SIC codes. They are therefore lost in the next step.
# # Read SIC conversion file and merge to obtain Section, Division, Group and Class.
# convert1 <- fread("data/convertSIC.csv")
# dissolutions <- merge(dissolutions, convert1, by="Class")
# ## add NUTS ----
# dissolutions$postcodeDistrict <- gsub("[ .]", "", dissolutions$postcode)
# dissolutions$postcodeDistrict <- sub("...$", "", dissolutions$postcodeDistrict)
# pcd2NUTS <- fread("data/convertNUTS.csv")
# dissolutions <- merge(dissolutions, pcd2NUTS, by="postcodeDistrict")
# dissolutions <- dissolutions %>% rename(date = dissolution_date)
# saveRDS(dissolutions, "data/dissolutions_snapshot_ready.rds")

dissolutions <- readRDS("data/dissolutions_snapshot_ready.rds")
dissolutions$date <- as.Date(dissolutions$date)
