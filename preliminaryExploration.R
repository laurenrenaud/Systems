library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(lubridate)

# from the sample data from Blackboard
retailVancouver <- read.csv("../data/retail analysis dataset final - cmu.csv", sep=",", header=T)
dispensing.cmu <- read.csv("../data/dispensing - cmu - raw.csv", sep=",", header=T)
inventory <- read.csv("../data/inventory - cmu - raw.csv", sep=",", header=T)
# reference for what inventory types mean
inventory.types <- read.csv("../data/Dec2016/cleaned/inventory_type.csv", sep=",", header=T)


# -------------- location Data

# first file is all locations - producers, processors, retailers, 
locations <- read.csv("../data/Labs & Liscensees/biotrackthc_locations.csv", sep=",", header=T)
# file that has codes for different location types
locationtypes <- read.csv("../data/Labs & Liscensees/locationtypes.csv", sep=",", header=T)

# create dataframe of only retailers (has been written out to 'labs & liscensee' folders as csvs)
retailers <- dplyr::filter(locations, retail==1)
# create dataframe of only producers
producers <- dplyr::filter(locations, producer==1)
# create dataframe of only processors
processors <- dplyr::filter(locations, processor==1)

###
### need to figure out 
### locationid
### locationtype
### locationexp
### locationissue
### in biotrackthc_locations.csv



# -------------- Sales Data
dispensing <- read.csv("../data/Dec2016/biotrackthc_dispensing2.csv", sep=",", header=T)
dispensing$sessiontime <- as.POSIXct(dispensing$sessiontime, origin = "1970-01-01")
dispensing$sale_month <- month(dispensing$sessiontime)
dispensing$sale_day <- day(dispensing$sessiontime)
dispensing$sale_hour <- hour(dispensing$sessiontime)


# -------------- exploring plantderivatives
derivatives <- read.csv("../data/Dec2016/biotrackthc_plantderivatives2.csv", sep=",", header=T)
derivatives <- left_join(derivatives, inventory.types, by="inventorytype")



# -------------- exploring retailVancouver
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

# -------------- exploring refunds
dispensing$refunded <- as.logical(dispensing$refunded)
# median amounts refunded are less than $1 more than median total amount
summary(dispensing$price[dispensing$refunded])
# are the refunds a new transaction
median(dispensing$price[dispensing$refunded], na.rm=T)
median(dispensing$price, na.rm=T)
# percentage refunded
sum(dispensing$refunded, na.rm=T) / nrow(dispensing)


# -------------- exploring dispensing (aka retail) data from sample
dispensing <- left_join(dispensing, inventory.types, by="inventorytype")
# sampling for smaller files
dispensing.list <- sample(dispensing$id, 15000, replace=F)
dispensing.sample <- dplyr::filter(dispensing, id %in% dispensing.list)
#write.table(dispensing.sample, file="../data/Dec2016/samples/dispensing_sample.csv", sep=",", row.names = F, col.names = T)

class(dispensing$inv_type_name)

dispensing <- within(dispensing,
                     inventorytype <- factor(inventorytype,levels=names(sort(table(inventorytype), decreasing=T))))

dispensing %>%
  dplyr::group_by(inventorytype) %>%
  dplyr::summarise(
    Count = n(),
    meanPrice = round(mean(price), 2)
  ) %>%
  ggplot(aes(x=inventorytype, y=Count)) + 
  geom_bar(stat="identity") + 
  labs(title = "Inventory by Type") + 
  coord_flip() 

dispensing %>%
  dplyr::group_by(inventorytype) %>%
  summarise(Count = n(),
            meanPrice = round(mean(price), 2)) %>%
  kable(col.names=c("Type", "Count", "Avg Price"))


# -------------- exploring inventorytypes
summary(dispensing$weight)
summary(dispensing$usableweight)
# need to ask questions about weight and useable weight because
# in some useable is greater and some weight is greater
dispensing$percUseable <- dispensing$usableweight / dispensing$weight
summary(dispensing$percUseable)
hist(dispensing$percUseable)

# missingness for useable weight
sum(is.na(dispensing$usableweight)) / nrow(dispensing)


# -------------- exploring items in transaction
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
