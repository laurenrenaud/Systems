library(dplyr)
library(ggplot2)
library(knitr)
library(tidyr)
library(readr)
library(lubridate)

# from the sample data from Blackboard -----
retailVancouver <- read.csv("../data/retail analysis dataset final - cmu.csv", sep=",", header=T)
dispensing.cmu <- read.csv("../data/dispensing - cmu - raw.csv", sep=",", header=T)
inventory <- read.csv("../data/inventory - cmu - raw.csv", sep=",", header=T)
# reference for what inventory types mean
inventory.types <- read.csv("../data/Dec2016/cleaned/inventory_type.csv", sep=",", header=T)
# massive file:
#inventorylog <- readr::read_csv("../data/Dec2016/biotrackthc_inventorylog.csv")


# location Data --------------

# first file is all locations - producers, processors, retailers, 
#locations <- read.csv("../data/Labs & Liscensees/biotrackthc_locations.csv", sep=",", header=T)
# don't know what 'locationid' means. All values are 1 or 2. Is not same as unique identifies (3-4 digits),
# or location types (which are indicated by locationtype). 
# changing to locationid_unknownvariable to avoid further confusion by other users
locations <- rename(locations, locationid_unknownvariable = locationid)
locations <- rename(locations, location_id = id)
write.table(locations, file="../data/Dec2016/cleaned/locations/all_locations.csv",
            sep=",", row.names = F, col.names = T)

locations <- read.csv("../data/Dec2016/cleaned/locations/all_locations.csv", sep=",", header=T)

locations.name.type <- select(locations, orgid, name, locationtypeNames, licensenum, location_id)

locationtypes <- read.csv("../data/Dec2016/cleaned/locations/locationtypes.csv", sep=",", header=T)
locations <- left_join(locations, locationtypes, by=c("locationtype" = "locationtypeCodes"))


list.storesNotinDispensing <- unique(locations$location_id[!locations$location_id %in% list.storesindispensing & 
                                                             locations$retail==1])

storesNotInDispensing <- filter(locations, location_id %in% list.storesNotinDispensing)


# file that has codes for different location types
locationtypes <- read.csv("../data/Dec2016/cleaned/locations/locationtypes.csv", sep=",", header=T)

# create dataframe of only retailers
retailers <- dplyr::filter(locations, retail==1)
write.table(retailers, file="../data/Dec2016/cleaned/locations/retailers.csv", row.names=F)
# looking at mailing address counts
retailers %>%
  dplyr::group_by(mailaddress1) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::filter(count > 1) %>%
  dplyr::arrange(desc(count))
# create dataframe of only producers
producers <- dplyr::filter(locations, producer==1)
write.table(producers, file="../data/Dec2016/cleaned/locations/producers.csv", row.names=F)
# create dataframe of only processors
processors <- dplyr::filter(locations, processor==1)
write.table(processors, file="../data/Dec2016/cleaned/locations/processors.csv", row.names=F)

###
### need to figure out 
### locationid
### locationtype
### locationexp
### locationissue
### in biotrackthc_locations.csv


# Sales Data --------------
dispensing <- read.csv("../data/Dec2016/cleaned/dispensing.csv", sep=",", header=T)
dispensing$monthtime <- as.POSIXct(dispensing$sessihontime,
                                   origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
# already run to get above dataset
dispensing$sale_year <- year(dispensing$monthtime)
dispensing$sale_month <- month(dispensing$monthtime)
dispensing$sale_day <- day(dispensing$monthtime)
dispensing$sale_hour <- hour(dispensing$monthtime)
dispensing$dayofweek <- weekdays(dispensing$monthtime)
dispensing$dayofweek = with(dispensing, factor(dayofweek,c("Monday", "Tuesday", "Wednesday",
                                                           "Thursday", "Friday", "Saturday", "Sunday")))
dispensing$date <- as.Date(dispensing$monthtime)
dispensing <- left_join(dispensing, inventory.types, by="inventorytype")
#write.table(dispensing, file="../data/Dec2016/cleaned/dispensing.csv", sep=",")

# sampling for smaller files
dispensing.list <- sample(dispensing$id, 35000, replace=F)
dispensing.sample <- dplyr::filter(dispensing, id %in% dispensing.list)
write.table(dispensing.sample, file="../data/Dec2016/cleaned/samples/dispensing_sample.csv", 
            sep=",", row.names = F, col.names = T)
dispensing.sample <- read.csv("../data/Dec2016/cleaned/samples/dispensing_sample.csv")
dispensing.sample$monthtime <- as.Date(dispensing.sample$monthtime)

# create table of all stores total sales, sorted by total sales
stores.by.sales <- dispensing %>%
  group_by(location) %>%
  summarise(total_sales = sum(price, na.rm=T)) %>%
  arrange(desc(total_sales)) %>%
  left_join(locations, by=c("location" = "location_id")) %>%
  select(location, total_sales, name, address1, city, zip, locationtype,
         status, loclatitude, loclongitude) %>%
  left_join(locationtypes, by=c("locationtype" = "locationtypeCodes"))
write.table(stores.by.sales, file="../data/Dec2016/cleaned/samples/stores_by_totalsales.csv", sep=",",
            row.names = F, col.names = T)


# create table of all stores total sales by month, sorted by total sales
stores.sales.by.month <- dispensing %>%
  group_by(location, sale_year, sale_month) %>%
  summarise(total_sales = sum(price, na.rm=T)) %>%
  arrange(desc(total_sales)) %>%
  left_join(locations, by=c("location" = "location_id")) %>%
  select(location, sale_year, sale_month, total_sales, name, address1, city, zip, locationtype,
         status, loclatitude, loclongitude) %>%
  left_join(locationtypes, by=c("locationtype" = "locationtypeCodes"))
write.table(stores.sales.by.month, file="../data/Dec2016/cleaned/samples/stores_by_totalsales_month.csv",
            sep=",", row.names = F, col.names = T)

# dispensing -  week selections
summary(dispensing$monthtime)
# Min.               1st Qu.                Median                  Mean               3rd Qu. 
# "2014-07-08 10:46:59" "2015-11-02 20:29:36" "2016-04-14 13:49:54" "2016-03-06 20:27:51" "2016-08-04 13:36:41" 
# Max. 
# "2016-11-01 02:58:33" 

# week of 2015 July 19-25
dispensing_jul_19_25_2015 <- dispensing %>%
  filter(sale_year==2015 & sale_month==7 & sale_day>=19 & sale_day<=25)
write.table(dispensing_jul_19_25_2015,
            file="../data/Dec2016/cleaned/samples/dispensing_jul_19_25_2015.csv", 
            sep=",", row.names = F, col.names = T)

# week of 2015 Oct 18 - 24
dispensing_oct_18_24_2015 <- dispensing %>%
  filter(sale_year==2015 & sale_month==10 & sale_day>=18 & sale_day<=24)
write.table(dispensing_oct_18_24_2015,
            file="../data/Dec2016/cleaned/samples/dispensing_oct_18_24_2015.csv", sep=",",
            row.names = F, col.names = T)

# week of 2016 Jan 17 - 23
dispensing_jan_17_23_2016 <- dispensing %>%
  filter(sale_year==2016 & sale_month==1 & sale_day>=17 & sale_day<=23)
write.table(dispensing_jan_17_23_2016,
            file="../data/Dec2016/cleaned/samples/dispensing_jan_17_23_2016.csv", sep=",",
            row.names = F, col.names = T)

# week of 2016 Apr 3 - 9
dispensing_apr_03_09_2016 <- dispensing %>%
  filter(sale_year==2016 & sale_month==4 & sale_day>=3 & sale_day<=9)
write.table(dispensing_apr_03_09_2016,
            file="../data/Dec2016/cleaned/samples/dispensing_apr_03_09_2016.csv", sep=",",
            row.names = F, col.names = T)

# get all sales for top ten stores

# create list of the location number for the top 10 stores
top.10.list <- stores.by.sales %>%
  arrange(desc(total_sales)) %>%
  head(10) %>%
  select(location, total_sales, name)

# filter dispensing df to only top 10 sellers
top.10.stores <- dispensing %>%
  filter(location %in% top.10.list$location)
write.table(top.10.stores,
            file="../data/Dec2016/cleaned/samples/top_10_stores.csv", sep=",",
            row.names = F, col.names = T)

# create list of the location number for the top 10 stores
top.5.list <- stores.by.sales %>%
  arrange(desc(total_sales)) %>%
  head(5) %>%
  select(location, total_sales, name)

# filter dispensing df to only top 10 sellers
top.5.stores <- dispensing %>%
  filter(location %in% top.5.list$location)
write.table(top.5.stores,
            file="../data/Dec2016/cleaned/samples/top_5_stores.csv", sep=",",
            row.names = F, col.names = T)

# number of transcations, over time, divided by stores
dispensing %>%
  group_by(date) %>%
  summarise(avg_transactions = n() / length(unique(location))) %>%
  ggplot(aes(x=date, y=avg_transactions)) +
  geom_point(color="darkgreen") + 
  geom_smooth(color="lightgreen", method = "loess")

# sum of sales, over time, divided by stores
dispensing %>%
  group_by(date) %>%
  summarise(avg_sales = sum(price) / length(unique(location))) %>%
  ggplot(aes(x=date, y=avg_sales)) +
  geom_point(color="darkgreen") + 
  geom_smooth(color="lightgreen", method = "loess")


# cleaning inventory --------------
inventory.sample <- read.csv("../data/Dec2016/cleaned/samples/inventorysample2.csv", sep=",", header=T)
inventoryLog.sample <- read.csv("../data/Dec2016/cleaned/samples/inventoryLogSample.csv", sep=",", header=T)

# -------------- cleaning inventory time based variables
inventory.sample$monthtime <- as.POSIXct(inventory.sample$sessiontime,
                                   origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
inventory.sample$sale_year <- year(inventory.sample$monthtime)
inventory.sample$sale_month <- month(inventory.sample$monthtime)
inventory.sample$sale_day <- day(inventory.sample$monthtime)
inventory.sample$dayofweek <- weekdays(inventory.sample$monthtime)
inventory.sample$dayofweek = with(inventory.sample, factor(dayofweek,
                                                            c("Monday", "Tuesday", "Wednesday",
                                                              "Thursday", "Friday", "Saturday", "Sunday")))
inventory.sample$sale_hour <- hour(inventory.sample$monthtime)
inventory.sample$date <- as.Date(inventory.sample$monthtime)
inventory.sample <- left_join(inventory.sample, inventory.types, by="inventorytype")
write.table(inventory.sample, file="../data/Dec2016/cleaned/samples/inventorysample2.csv", sep=",",
            row.names=F)

inventoryLog.sample$monthtime <- as.POSIXct(inventoryLog.sample$sessiontime,
                                         origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
inventoryLog.sample$sale_year <- year(inventoryLog.sample$monthtime)
inventoryLog.sample$sale_month <- month(inventoryLog.sample$monthtime)
inventoryLog.sample$sale_day <- day(inventoryLog.sample$monthtime)
inventoryLog.sample$dayofweek <- weekdays(inventoryLog.sample$monthtime)
inventoryLog.sample$dayofweek = with(inventoryLog.sample, factor(dayofweek,
                                                           c("Monday", "Tuesday", "Wednesday",
                                                             "Thursday", "Friday", "Saturday", "Sunday")))
inventoryLog.sample$sale_hour <- hour(inventoryLog.sample$monthtime)
inventoryLog.sample <- left_join(inventoryLog.sample, inventory.types, by="inventorytype")
write.table(inventoryLog.sample, file="../data/Dec2016/cleaned/samples/inventoryLogSample.csv", sep=",", row.names=F)


# joining inventory samples + dispensing
inven.dispense <- inventory.sample %>%
  select(inventoryid = id, strain, weight, inv.location = location, inv.parentid = parentid, inv.inventorytype = inv_type_name,
         inv.useableweight = usableweight, inv.removescheduled = removescheduled,
         inv.removescheduletime = removescheduletime,  inv.inventoryparentid = inventoryparentid,
         inv.productname = productname,
         inv.removereason = removereason, inv.inventorystatustime = inventorystatustime, inv.sourceid = source_id,
         inv.date = date) %>%
  left_join(dispensing, by="inventoryid") 

test <- inven.dispense %>%
  select(id, inv.date, date)

# using inventory to get open dates for locations ---------------
# csvs from the following SQL queries:
# 
# select location, min(sessiontime)
# from biotrackthc_inventory
# group by location;
# 
# select location, min(sessiontime)
# from biotrackthc_inventory
# group by location;
# 
mindate <- read.csv("../data/Dec2016/cleaned/inventorymindate.csv", header=T, sep=",")
maxdate <- read.csv("../data/Dec2016/cleaned/maxsessiontime.csv", header=T, sep=",")
mindate$min.sessiontime. <- as.POSIXct(mindate$min.sessiontime.,
                                       origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
maxdate$max.sessiontime. <- as.POSIXct(maxdate$max.sessiontime.,
                                       origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
locationDates <- left_join(mindate, maxdate, by="location")

locationDates <- rename(locationDates, minDate = min.sessiontime., maxDate = max.sessiontime.)
locationDates <- read.csv("../data/Dec2016/cleaned/locations/locationDates.csv")
locationDates$minDate <- as.Date(locationDates$minDate)
locationDates$maxDate <- as.Date(locationDates$maxDate)
#write.table(locationDates, file="../data/Dec2016/cleaned/locations/locationDates.csv", sep=",", row.names=F)


# first opened + length of time
reportdate <- max(dispensing.sample$monthtime)
locations <- locations %>%
  dplyr::left_join(locationDates, by=c("location_id" = "location")) %>%
  dplyr::mutate(
    months_open = ifelse(status == "ACTIVE (ISSUED)",
                         ceiling(as.numeric(reportdate - minDate)/30),
                         ifelse(
                           status=="CLOSED (PERMANENT)",
                           ceiling(as.numeric(maxDate - minDate)/30),
                           NA
                         )))
#write.table(locations, file="../data/Dec2016/cleaned/locations/all_locations.csv", row.names = F,
#            col.names = T, sep=",")

# 296 locations do not appear in the inventory file / do not have dates in the inventory file:
review <- locations %>%
  filter(is.na(months_open)) %>%
  select(location_id, status, locationtypeNames) %>%
  arrange(status, locationtypeNames)

# cleaning inventory location based variables --------------
inventory.sample <-  inventory.sample %>%
  left_join(locations, by=c("location" = "location_id")) %>%
  select(location, name, address1, city, zip, locationtype,
         status, loclatitude, loclongitude) %>%
  left_join(locationtypes, by=c("locationtype" = "locationtypeCodes"))
write.table(inventory.sample, file="../data/Dec2016/cleaned/samples/inventorysample2.csv", sep=",",
            row.names=F)


# cleaning inventory transfers --------------
inventorytransfers <- read.csv("../data/Dec2016/biotrackthc_inventorytransfers.csv", sep=",", header=T)
inventorytransfers <- left_join(inventorytransfers, locations.name.type, by=c("location" = "location_id"))
# sampling for smaller files
transfers.list <- sample(inventorytransfers$id, 20000, replace=F)
transfers.sample <- dplyr::filter(inventorytransfers, id %in% transfers.list)
write.table(transfers.sample, file="../data/Dec2016/cleaned/samples/transfers_sample.csv", 
            sep=",", row.names = F, col.names = T)

transfers.sample <- left_join(transfers.sample, locations.name.type, by=c("location" = "location_id"))
transfers.sample <- left_join(transfers.sample, locationtypes, by=c("locationtype" = "locationtypeCodes"))

transfers.sample <- read.csv("../data/Dec2016/cleaned/samples/transfers_sample.csv", header=T, sep=",")


locations <- locations %>%
  mutate(type_simp = ifelse(locationtypeNames == "Producer Tier 1" | locationtypeNames == "Producer Tier 2" |
                              locationtypeNames == "Producer Tier 3",
                            "Producer",
                            ifelse(locationtypeNames == "Producer + Processor Tier 1" | locationtypeNames == "Producer + Processor Tier 2" |
                                     locationtypeNames == "Producer + Processor Tier 3",
                                   "Producer-Processor",
                                   ifelse(locationtypeNames=="Processor",
                                          "Processor",
                                          "Retailer"))))

locations_type_in <- select(locations, inbound_license = licensenum, inboundtype = type_simp, inboundcity = city)
locations_type_out <- select(locations, outbound_license = licensenum, outboundtype = type_simp, outboundcity = city)

test <- transfers.sample %>%
  left_join(locations_type_in, by = c("inbound_license" = "inbound_license")) %>%
  left_join(locations_type_out, by= c("outbound_license" = "outbound_license"))

write.table(test, file="transfer_withtypes.csv", row.names = F, col.names = T, sep=",")

sort(table(test$inboundtype))
sort(table(test$outboundtype))

review <- transfers.sample %>%
  group_by(name) %>%
  summarise(count = n_distinct(locationtypeNames)) %>%
  arrange(desc(count)) %>%
  group_by(count) %>%
  summarise(pivot = n())

summary(transfers.sample$unitprice[transfers.sample$locationtypeNames=="Retailer"])

## inventory conversions
inventory_conversions <- read.csv("../data/Dec2016/biotrackthc_inventoryconversions.csv", sep=",", header=T)
## inventory combinations
inventory_combinations <- read.csv("../data/Dec2016/biotrackthc_inventorycombinations.csv", sep=",", header=T)

names(transfers.sample)

unique(inventorytransfers$description)

inventorytransfers$parentid[inventorytransfers$inventoryid==6032711860003000]


filter(inventorytransfers, inventoryid==6032711860002440)

childlist <- inventorytransfers$inventoryid[inventorytransfers$parentid==6032711860002440]

childdf <- inventorytransfers %>%
  filter(inventoryid %in% childlist)


childdf2 <- inventorytransfers %>%
  filter(inventoryid == 6033530000000000)

child2_combinations <- inventory_combinations %>%
  filter(id == 6033876720000360)
  
child2_conversions <- inventory_conversions %>%
  filter(id == 6033876720000360)

child2_transfers <- inventorytransfers %>%
  filter(parentid == 6033876720000360)


list.logID <- unique(inventoryLog.sample$i)



unique(transfers.sample$locationtypeNames[transfers.sample$description=="Flower Lot"])

sum(inventorytransfers$parentid==6033569260000780)

inventorytransfers$description[inventorytransfers$parentid==6033552540000180]

## number of distinct parentIds in inventory
# 616412




# I think location is the location it's at now
# and where it came from is `inbound_location`
# need to figure out difference between `saleprice` and `unitprice`
length(unique(inventorytransfers$description))
length(unique(retailers$name))
unique(inventorytransfers$description)
sum(is.na(inventorytransfers$description))
length((inventorytransfers$description))
names(inventorytransfers)
summary(inventorytransfers$saleprice)
summary(inventorytransfers$unitprice)
class(inventorytransfers$parentid)
nrow(dispensing)


# exploring plantderivatives --------------
derivatives <- read.csv("../data/Dec2016/biotrackthc_plantderivatives2.csv", sep=",", header=T)
derivatives <- left_join(derivatives, inventory.types, by="inventorytype")



# exploring retailVancouver --------------
names(retailVancouver)

hist(retailVancouver$mday)
# mid month way more popular -- real or data entry error?

hist(retailVancouver$wday)

# need to ask questions about weight and useable weight because
# in some useable is greater and some weight is greater
retailVancouver$percUseable <- retailVancouver$usableweight / retailVancouver$weight
summary(retailVancouver$percUseable)
hist(retailVancouver$percUseable)

names(dispensing)

# exploring refunds --------------
dispensing$refunded <- as.logical(dispensing$refunded)
# median amounts refunded are less than $1 more than median total amount
summary(dispensing$price[dispensing$refunded])
# are the refunds a new transaction
median(dispensing$price[dispensing$refunded], na.rm=T)
median(dispensing$price, na.rm=T)
# percentage refunded
sum(dispensing$refunded, na.rm=T) / nrow(dispensing)


# exploring dispensing (aka retail) data from sample --------------

dispensing.sample <- within(dispensing.sample,
                            inv_type_name <- factor(inv_type_name,levels=names(sort(table(inv_type_name), decreasing=T))))

dispensing.sample %>%
  dplyr::group_by(inv_type_name) %>%
  dplyr::summarise(
    Count = n(),
    meanPrice = round(mean(price), 2)
  ) %>%
  ggplot(aes(x=inv_type_name, y=Count)) + 
  geom_bar(stat="identity") + 
  labs(title = "Inventory by Type") + 
  coord_flip() 

dispensing.sample %>%
  dplyr::group_by(inv_type_name) %>%
  summarise(Count = n(),
            meanPrice = round(mean(price), 2)) %>%
  kable(col.names=c("Type", "Count", "Avg Price"))


# exploring inventorytypes -------------- 
summary(dispensing$weight)
summary(dispensing$usableweight)
# need to ask questions about weight and useable weight because
# in some useable is greater and some weight is greater
dispensing$percUseable <- dispensing$usableweight / dispensing$weight
summary(dispensing$percUseable)
hist(dispensing$percUseable)

# missingness for useable weight
sum(is.na(dispensing$usableweight)) / nrow(dispensing)


# exploring items in transaction --------------
by.transaction <- dispensing %>%
  dplyr::group_by(transactionid) %>%
  summarise(numItems = n(),
            meanPrice = round(mean(price), 2),
            sumPrice = sum(price))

# small number of items
hist(by.transaction$numItems[by.transaction$numItems<10])
hist(by.transaction$meanPrice[by.transaction$numItems<10])
summary(by.transaction$meanPrice[by.transaction$numItems<10])
hist(by.transaction$sumPrice[by.transaction$numItems<10])
summary(by.transaction$sumPrice[by.transaction$numItems<10])
# large number of items
hist(by.transaction$numItems[by.transaction$numItems>10])
hist(by.transaction$meanPrice[by.transaction$numItems>10])
summary(by.transaction$meanPrice[by.transaction$numItems>10])
hist(by.transaction$sumPrice[by.transaction$numItems>10])
summary(by.transaction$sumPrice[by.transaction$numItems>10])
# comparing
# # mean price
summary(by.transaction$meanPrice[by.transaction$numItems<10])
summary(by.transaction$meanPrice[by.transaction$numItems>10])
# # sum price
summary(by.transaction$sumPrice[by.transaction$numItems<10])
summary(by.transaction$sumPrice[by.transaction$numItems>10])

names(retailVancouver)
unique(retailVancouver$store_loc)
