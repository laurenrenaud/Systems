library(dplyr)
library(ggplot2)
library(knitr)

retailVancouver <- read.csv("../data/retail analysis dataset final - cmu.csv", sep=",", header=T)
dispensing <- read.csv("../data/dispensing - cmu - raw.csv", sep=",", header=T)
inventory <- read.csv("../data/dispensing - cmu - raw.csv", sep=",", header=T)

names(inventory)
unique(inventory$location)
summary(inventory$usableweight)
nrow(inventory)


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

unique(dispensing$location)

# -------------- exploring inventorytypes
unique(dispensing$inventorytype)
dispensing$inventorytype[which(dispensing$inventorytype==22)] <- "Solid Marijuana Infused Edible"
dispensing$inventorytype[which(dispensing$inventorytype==23)] <- "Liquid Marijuana Infused Edible"
dispensing$inventorytype[which(dispensing$inventorytype==24)] <- "Marijuana Extract for Inhalation"
dispensing$inventorytype[which(dispensing$inventorytype==25)] <- "Marijuana Infused Topicals"
dispensing$inventorytype[which(dispensing$inventorytype==28)] <- "Usable Marijuana"
dispensing$inventorytype[which(dispensing$inventorytype==31)] <- "Marijuana Mix Package"
dispensing$inventorytype[which(dispensing$inventorytype==32)] <- "Marijuana Mix Infused"
dispensing$inventorytype[which(dispensing$inventorytype==37)] <- "Suppository"

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
