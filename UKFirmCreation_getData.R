# # Download data ----
# # Create a temp. file.
# temp <- tempfile()
# # Use `download.file()` to fetch the file into the temp. file.
# download.file("http://download.companieshouse.gov.uk/BasicCompanyDataAsOneFile-2022-01-01.zip",temp)
# # Unzip and read csv file.
# repD <- vroom(temp)
# # Remove the temp file via 'unlink()'
# unlink(temp)
# # Make incorporation date as date format.
# repD <- repD %>% rename(date=IncorporationDate)
# repD$date <- as.Date(repD$date, "%d/%m/%Y")
# ### Make usable dataframe for the app.
# # Delete unnecessary columns.
# register <- repD[which(repD$date >= "2019-01-01" &
#                             repD$date <= "2021-12-31"), c(2,10,15,27)]
# 
# # SIC codes ----
# # Keep only the code from SIC.
# register$SIC5dg1 <- as.integer(gsub("([0-9]+).*$", "\\1", register$SICCode.SicText_1)) # pattern is by finding a set of numbers in the start and capturing them
# register$Class <- as.integer(register$SIC5dg1/10)
# # Overseas companies are not supplied with SIC codes. They are therefore lost in the next step.
# # Read SIC conversion file and merge to obtain Section, Division, Group and Class.
# convert1 <- fread("data/convertSIC.csv")
# register <- merge(register, convert1, by="Class")
# 
# # NUTS2 ----
# #Keep only postcode district
# register$postcodeDistrict <- gsub("[ .]", "", register$RegAddress.PostCode)
# register$postcodeDistrict <- sub("...$", "", register$postcodeDistrict)
# #Convert PC district to NUTS2
# pcd2NUTS <- fread("data/convertNUTS.csv")
# registerPC <- merge(register, pcd2NUTS, by="postcodeDistrict")
# #Save full register ready for use.
# fwrite(registerPC, "data/registerSectorsRegionsUK.csv", row.names = F)
# # save as RDS for smaller file
# saveRDS(registerPC, "data/registerSectorsRegionsUK.rds")

registerPC <- readRDS("data/registerSectorsRegionsUK.rds")
daily2019 <- fread("data/Daily2019.csv")