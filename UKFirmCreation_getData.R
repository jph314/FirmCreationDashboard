# Set default start and end dates
startDate <- ymd("2020-01-01")
endDate <- ymd("2022-06-30")

# # Read archive data ----
# register <- readRDS("data/incorporations_archive_byMonthPostcodeSIC5.rds")
# register <- register %>% rename(date = IncorporationDate)
#
# # SIC codes ----
# # Keep only the code from SIC.
# register$Class <- as.integer(register$SIC5/10)
# # Overseas companies are not supplied with SIC codes. They are therefore lost in the next step.
# # Read SIC conversion file and merge to obtain Section, Division, Group and Class.
# convertSIC <- fread("data/convertSIC.csv")
# register <- merge(register, convertSIC, by="Class")
#
# # LA Districts ----
# # Read postcode conversion file and merge to obtain LA District, County (in England and Wales only) and Country.
# convertPostcodes <- fread("data/postcodes.csv") %>% rename(postcode = Postcode)
# register$postcode <- gsub("[ .]", "", register$postcode)
# registerLA <- merge(register, convertPostcodes, by="postcode", all.x = T)
# # registerLA$District[is.na(registerLA$District)] <- ""
#
# #Save full register if required.
# saveRDS(registerLA, "data/registerClassLA.rds")
#
# # Aggregation ----
# registerLA <- setDT(registerLA)
# register <- registerLA[,.N,keyby=list(
#   date,Class,Group,Section,Class.name,Group.name,Division.name,Section.name,
#   SectionAbb,District,County,Country)] %>% rename(n=N)
# write_fst(register, "data/registerAgg.fst")

# Load pre-aggregated data ====
register <- read_fst("data/registerAgg.fst")

# prepare dissolution data ----
# dissolutions <- readRDS("data/dissolutions_snapshot.rds")
# 
# dissolutions$Class <- as.integer(as.integer(dissolutions$SIC.1)/10)
# # Overseas companies are not supplied with SIC codes. They are therefore lost in the next step.
# # Read SIC conversion file and merge to obtain Section, Division, Group and Class.
# convertSIC <- fread("data/convertSIC.csv")
# dissolutions <- merge(dissolutions, convertSIC, by="Class")
# # # LA Districts ----
# # # Read postcode conversion file and merge to obtain LA District, County (in England and Wales only) and Country.
# convertPostcodes <- fread("data/postcodes.csv") %>% rename(postcode = Postcode)
# dissolutions$postcode <- gsub("[ .]", "", dissolutions$postcode)
# dissolutionsLA <- merge(dissolutions, convertPostcodes, by="postcode", all.x = T)
# dissolutionsLA$District[is.na(dissolutionsLA$District)] <- ""
# # #Save full register if required.
# saveRDS(dissolutionsLA, "data/dissolutionsClassLA.rds")
#
# # Aggregation ----
# dissolutionsLA <- setDT(dissolutionsLA)
# dissolutions <- dissolutionsLA[,.N,keyby=list(date,Class,Group,Section,Class.name,Group.name,Division.name,Section.name,SectionAbb,District,County,Country)] %>%
#   rename(n=N)
# # Save ready for use.
# write_fst(dissolutions, "data/dissolutionsAgg.fst")

# Load pre-aggregated data ====
dissolutions <- read_fst("data/dissolutionsAgg.fst")
