# Set default start and end dates
startDate <- ymd("2020-01-01")
endDate <- ymd("2022-09-30")

# # Read archive data ----
# register <- readRDS("data/incorporations.rds")
# register <- register %>% rename(date = IncorporationDate)
# 
# # SIC codes ----
# # Keep only the code from SIC.
# register$Class <- as.integer(register$SIC5/10)
# # Overseas companies are not supplied with SIC codes. They are therefore lost in the next step.
# # Read SIC conversion file and merge to obtain Section, Division, Group and Class.
# convertSIC <- fread("data/convertSIC.csv")
# register <- merge(register, convertSIC, by="Class", all.x = T)
# 
# # LA Districts ----
# # Read postcode conversion file and merge to obtain LA District, County (in England and Wales only) and Country.
# convertPostcodes <- fread("data/postcodes.csv")
# register$postcode <- gsub("[ .]", "", register$postcode)
# registerLA <- merge(register, convertPostcodes, by="postcode", all.x = T)
# # registerLA$District[is.na(registerLA$District)] <- ""
# 
# #Save full register if required.
# saveRDS(registerLA, "data/registerClassLA.rds")
# 
# # Aggregation ----
# registerLA <- setDT(registerLA)
# register <- registerLA[,list(n=sum(incorporations)),keyby=list(
#   date,Class,Group,Section,Class.name,Group.name,Division.name,Section.name,
#   SectionAbb,District,County,Country)]
# write_fst(register, "data/registerAgg.fst")

# Load pre-aggregated data ====
register <- setDT(read_fst("data/registerAgg.fst"))

# # prepare dissolution data ----
# dissolutions <- readRDS("data/dissolutions.rds")
# 
# dissolutions <- dissolutions %>% rename(date = dissolution_date)
# dissolutions$Class <- as.integer(as.integer(dissolutions$SIC.1)/10)
# # Overseas companies are not supplied with SIC codes. They are therefore lost in the next step.
# # Read SIC conversion file and merge to obtain Section, Division, Group and Class.
# convertSIC <- fread("data/convertSIC.csv")
# dissolutions <- merge(dissolutions, convertSIC, by="Class", all.x = T)
# # # LA Districts ----
# # # Read postcode conversion file and merge to obtain LA District, County (in England and Wales only) and Country.
# convertPostcodes <- fread("data/postcodes.csv") #%>% rename(postcode = Postcode)
# dissolutions$postcode <- gsub("[ .]", "", dissolutions$postcode)
# dissolutionsLA <- merge(dissolutions, convertPostcodes, by="postcode", all.x = T)
# # dissolutionsLA$District[is.na(dissolutionsLA$District)] <- ""
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
dissolutions <- setDT(read_fst("data/dissolutionsAgg.fst"))

# # Survival data ----
# setDT(dissolutionsLA)
# setDT(registerLA)
# #Aggregate dissolutions by incorp date, county and section
# dissolutionsByInc <- dissolutionsLA[!(is.na(date)),.N,keyby=list(incorporation_date, date, County, Section, SectionAbb)] %>%
#   rename(dissolutions = N, dissolution_date = date, date = incorporation_date)
# # Age at dissolution
# dissolutionsByInc$age <- dissolutionsByInc$dissolution_date - dissolutionsByInc$date
# # Aggregate incorporations by date, county and section
# survival <- registerLA[!(is.na(date)),list(incorporations=sum(incorporations)), keyby=list(date, County, Section, SectionAbb)]
# # Number of companies incorporated by date, county, section which dissolved within 6, 12 and 18 months
# survival <- merge(survival,
#                              dissolutionsByInc[age<=182,list(D6month=sum(dissolutions, na.rm=T)),keyby=list(date, County, Section, SectionAbb)],
#                              all.x=T)
# survival <- merge(survival,
#                              dissolutionsByInc[age<=365,list(D12month=sum(dissolutions, na.rm=T)),keyby=list(date, County, Section, SectionAbb)],
#                              all.x=T)
# survival <- merge(survival,
#                              dissolutionsByInc[age<=547,list(D18month=sum(dissolutions, na.rm=T)),keyby=list(date, County, Section, SectionAbb)],
#                              all.x=T)
# # Save ready for use
# write_fst(survival, "data/survival.fst")

# Load prepared data
survival <- setDT(read_fst("data/survival.fst"))

# List Section/Group/County/District for pickers in UI
listSec <- sort(unique(dissolutions[Section!="",]$Section))
listSecName <- unique(dissolutions[Section!="",]$Section.name)[order(unique(dissolutions[Section!="",]$Section))]
listGrp <- sort(unique(dissolutions$Group))
listGrpName <- unique(dissolutions$Group.name)[order(unique(dissolutions$Group))]
listDst <- sort(unique(dissolutions[District!="",]$District))
listCty <- sort(unique(dissolutions[County!="",]$County))
