library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)

# set up ------------------
# locatons
locations <- read.csv("../data/Dec2016/cleaned/locations/all_locations.csv", sep=",", header=T)
processor.loc <- filter(locations, processor==1)

# inventory
inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
inventory.types <- read.csv("../data/Dec2016/cleaned/inventory_type.csv", sep=",", header=T)
# convert UNIX time
inventory$inv_date <- as.POSIXct(inventory$sessiontime,
                                    origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
inventory$inv_date <- as.Date(inventory$inv_date)
# add in inventory type names
inventory <- left_join(inventory, inventory.types, by="inventorytype")
inventory$sample_id <- as.numeric(inventory$sample_id)
# select needed variables and rename to be easier to understand later in the joins
inventory.select <- select(inventory, inv_invid = id, inv_strain = strain, inv_weight = weight,
                           inv_location = location, inv_inv_type = inv_type_name, seized, inv_deleted = deleted,
                           inv_usableweight = usableweight, inv_invparentid = inventoryparentid,
                           inv_productname = productname, inv_date)
#inventory.processor <- filter(inventory.select, inv_location %in% processor.loc$location_id)

# transfers
transfers <- readr::read_csv("../data/Dec2016/biotrackthc_inventorytransfers.csv")
transfers$inventoryid <- as.numeric(transfers$inventoryid)
transfers$usableweight <- as.numeric(transfers$usableweight)

transfers.select <- transfers %>%
  #filter(inventoryparentid %in% dis.select$inventoryid) %>%
  # select needed variables and rename to be easier to understand later in the joins
  select(trans_id = id, trans_invid = inventoryid, trans_strain = strain, trans_location = location,
         trans_parentid = parentid, trans_invtype = inventorytype, trans_usableweight = usableweight,
         trans_weight = weight,
         trans_out_lisc = outbound_license, trans_in_lisc = inbound_license, trans_in_loc = inbound_location,
         trans_saleprice = saleprice, trans_unitprice = unitprice)

# get type names
transfers.select <- transfers.select %>%
  left_join(inventory.types, by=c("trans_invtype" = "inventorytype")) %>%
  rename(trans_invtypename = inv_type_name)
# get location names
loc.name.city <- select(locations, trans_name = name, trans_license = licensenum, location_id,
                              trans_city = city, trans_loctype = locationtypeNames)
# join in some selected location infomation
transfers.select <- transfers.select %>%
  left_join(loc.name.city, by=c("trans_location" = "location_id"))
transfers.select$trans_in_lisc <- as.integer(transfers.select$trans_in_lisc)

# for transfers, get types of locations, in and outbound
loc.name.city <- select(locations, name, licensenum, location_id, city, locationtypeNames)
# this, adding location types, is where the count of trans_id and nrows loses sync ################
# we get the same number of trans_ids as before
# but now have 23853 extra rows, duplicate trans_ids
transfers.select <- transfers.select %>%
  # the increase comes from joining on liscense numbers
  # maybe because of the duplicate liscense numbers we found
  # using location instead keeps trans_id the same as nrows
  #left_join(loc.name.city, by=c("trans_out_lisc" = "licensenum")) %>%
  #rename(t_out_name = name, t_out_locid = location_id, t_out_city = city, t_out_type = locationtypeNames) %>%
  # trying taking out joining out, because I think that's just what's in there already, 
  # that's the trans_city, trans_location?
  left_join(loc.name.city, by=c("trans_in_loc" = "location_id")) %>%
  rename(t_in_name = name, t_in_liscense = licensenum, t_in_city = city, t_in_type = locationtypeNames)

# get processor transfers
transfers.processor <- filter(transfers.select, trans_out_lisc %in% processor.loc$licensenum)
# trying to join to inventory to times
# trying joining transfers inventory id to inventory id just to get *that* item, not going up a step
inv.snip <- select(inventory.select, inv_invparentid, inv_invid, inv_date)
transfers.processor <- left_join(transfers.processor, inv.snip, by=c("trans_invid" = "inv_invid"))

# processor to retail only -----------
process.to.retail <- transfers.processor %>%
  filter(t_in_type=="Retailer" | t_in_type=="Retailer & Medical")


# remove some anomolies
proc.retail.clean <- process.to.retail %>%
  filter(trans_loctype != "Producer Tier 2", 
         trans_unitprice > 0,
         trans_saleprice > 0,
         !is.na(trans_in_loc))


# sample processors to retail
#sample.list <- sample(proc.retail.clean$trans_id, 20000, replace=F)
#sample.process.to.retail <- dplyr::filter(proc.retail.clean, trans_id %in% sample.list)
#write.table(sample.process.to.retail, file="../data/Dec2016/cleaned/samples/processor_retail_sample.csv", 
#            sep=",", row.names = F, col.names = T)


## wholesale and retail prices -------------
## cleaning dispensing for comparing
dispensing <- readr::read_csv("../data/Dec2016/biotrackthc_dispensing.csv")
dispensing$saletime <- as.Date(as.POSIXct(dispensing$sessiontime,
                                                 origin = "1970-01-01", tz="America/Los_Angeles"))
# editing for post July tax change
# dispensing$price_x <- ifelse(dispensing$saletime >= "2015-07-01", 
#                              dispensing$price*1.37, 
#                              dispensing$price)

# price_x wasn't working, but is working on sample. 
sample.list <- sample(dispensing$id, 20000, replace=F)
sample.dispense <- dplyr::filter(dispensing, id %in% sample.list)
sample.dispense$price_x <- ifelse(sample.dispense$saletime >= "2015-07-01", 
                                  sample.dispense$price*1.37, 
                                  sample.dispense$price)


# now joining transfers to retail.pullman to get processor names attached to items
retail.test <- dispensing %>%
  # then the parent id from the dispening file matches the inventory id of the processor's inventory
  left_join(inventory, by=c("dis_parentid" = "inventoryid")) %>%
  select(dispensingid, retail_loc = location.x, saleprice = price, saleuseweight = usableweight.x, sale_invtype = inv_type_name.x,
         sale_strain = strain.x, sale_prodname, saleweight = weight.x, saletime, retail_invdate, transactionid = transactionid.x,
         refunded, dis_invid, retailname, trans_loc, trans_strain, trans_invtype, trans_parentid,
         processor_name, processortype = typesimp, process_strain = strain.y, process_weight = weight.y, process_transctionid = transactionid.y,
         process_productname = productname, processinv_date = inv_date, processor_invtype = inv_type_name.y,
         processor_invid = dis_parentid)



dispensing.select <- dplyr::select(dispensing, dispensingid = id, weight, saletime = sessiontime, 
                            price, inventorytype, location)

dispensing.select$saletime <- as.Date(as.POSIXct(dispensing.select$saletime,
                                          origin = "1970-01-01", tz="America/Los_Angeles"))

dispensing.select$price_x <- ifelse(dispensing.select$saletime >= "2015-07-01", 
                                    dispensing.select$price*1.37, 
                                    dispensing.select$price)




# overall comparision, without time
avg.saleprice <- dispensing.select %>%
  group_by(inventorytype) %>%
  summarise(avg_saleprice = median(price, na.rm=T))

avg.procprice <- process.to.retail %>%
  group_by(trans_invtype) %>%
  summarise(avg_processorprice = median(trans_unitprice, na.rm=T))

# plot avg sale price and avg producer price together
avg.saleprice %>%
  filter(!is.na(inventorytype)) %>%
  left_join(avg.procprice, by=c("inventorytype" = "trans_invtype")) %>%
  left_join(inventory.types, by="inventorytype") %>%
  # get one column with either sale price or processor price, another column with that value
  tidyr::gather(supplystep, avg_price, c(avg_saleprice, avg_processorprice)) %>%
  ggplot(aes(x=inv_type_name, y=avg_price, fill=supplystep)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# avg markup by type
avg.saleprice %>%
  filter(!is.na(inventorytype)) %>%
  left_join(avg.procprice, by=c("inventorytype" = "trans_invtype")) %>%
  group_by(inventorytype) %>%
  mutate(
    avg_markup_abs = avg_saleprice - avg_processorprice,
    avg_markup_perc = (avg_saleprice - avg_processorprice) / avg_saleprice
  ) %>%
  left_join(inventory.types, by="inventorytype") %>%
  ggplot(aes(x=reorder(inv_type_name, desc(avg_markup_abs)), y=avg_markup_abs)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

### including time
avg.sale.month <- dispensing.select %>%
  mutate(monthyear = floor_date(saletime, "month")) %>%
  group_by(inventorytype, monthyear) %>%
  summarise(avg_saleprice = median(price, na.rm=T)) %>%
  # for some reason the join didn't work on the monthyear / date variable
  # so I created a numeric variable for joining the dataframes
  mutate(monthjoin = as.numeric(monthyear))

### including time and post tax change
avg.sale.month <- dispensing.select %>%
  mutate(monthyear = floor_date(saletime, "month")) %>%
  group_by(inventorytype, monthyear) %>%
  summarise(avg_saleprice = ifelse(monthyear[1] >= "2015-07-01",
                                   median((price*1.37), na.rm=T),
                                   median(price, na.rm=T))
  ) %>%
  # for some reason the join didn't work on the monthyear / date variable
  # so I created a numeric variable for joining the dataframes
  mutate(monthjoin = as.numeric(monthyear))


# grouping by month to get avg by month
avg.proc.month <- process.to.retail %>%
  mutate(monthyear = floor_date(inv_date, "month")) %>%
  group_by(trans_invtype, monthyear) %>%
  summarise(avg_processorprice = median(trans_unitprice, na.rm=T)) %>%
  # for some reason the join didn't work on the monthyear / date variable
  # so I created a numeric variable for joining the dataframes
  mutate(monthjoin = as.numeric(monthyear))

## line graph of sale price and producer price, over time
avg.sale.month %>%
  left_join(avg.proc.month, by=c("monthjoin", "inventorytype" = "trans_invtype")) %>%
  # needed to drop and rename some variables because of the weird join
  select(inventorytype, month = monthyear.x, avg_saleprice, avg_processorprice) %>%
  # gather so we get one value per row
  tidyr::gather(supplystep, avg_price, c(avg_saleprice, avg_processorprice)) %>%
  left_join(inventory.types, by="inventorytype") %>%
  filter(!is.na(inventorytype), inv_type_name!="Capsule", inv_type_name!="Suppository",
                inv_type_name!="Tincture") %>%
  ggplot(aes(x=month, y=avg_price, color=supplystep)) +
  geom_line() +
  facet_wrap("inv_type_name") +
  theme(legend.position=c(0.65,0.15))

## line graph of sale price and producer price, over time
## for only useable, inhalation, edibles (solid)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
avg.sale.month %>%
  left_join(avg.proc.month, by=c("monthjoin", "inventorytype" = "trans_invtype")) %>%
  # needed to drop and rename some variables because of the weird join
  select(inventorytype, month = monthyear.x, avg_saleprice, avg_processorprice) %>%
  # gather so we get one value per row
  tidyr::gather(supplystep, avg_price, c(avg_saleprice, avg_processorprice)) %>%
  left_join(inventory.types, by="inventorytype") %>%
  filter(inv_type_name=="Usable Marijuana" | inv_type_name=="Marijuana Extract for Inhalation" |
         inv_type_name=="Solid Marijuana Infused Edible") %>%
  ggplot(aes(x=month, y=avg_price, color=supplystep)) +
  scale_color_manual(values=c(cbPalette[2], cbPalette[3])) +
  geom_line(size=1.5) +
  facet_wrap("inv_type_name") +
  theme(legend.position=c(0.9,0.9), legend.justification = c(1,1)) +
  labs(title="Avg Sale and Producer Prices \nfor Top Types",
       x="Month",
       y="Average Price") +
  geom_line(size=1.5) +
  facet_wrap("inv_type_name") +
  theme(legend.position=c(0.9,0.9), legend.justification = c(1,1))
  
## line graph of sale price and producer price, over time
## for only useable, inhalation, edibles (solid)
## ratio of retail / wholesale
avg.sale.month %>%
  left_join(avg.proc.month, by=c("monthjoin", "inventorytype" = "trans_invtype")) %>%
  # needed to drop and rename some variables because of the weird join
  select(inventorytype, month = monthyear.x, avg_saleprice, avg_processorprice) %>%
  # get ratio of retail / wholesale for each month
  group_by(inventorytype, month) %>%
  summarise(retail_wholesale_ratio = avg_saleprice / avg_processorprice) %>%
  left_join(inventory.types, by="inventorytype") %>%
  filter(inv_type_name=="Usable Marijuana" | inv_type_name=="Marijuana Extract for Inhalation" |
           inv_type_name=="Solid Marijuana Infused Edible",
         # there was a spike that hit 24 but it seemed like an anomoly
         # avg_saleprice = 63.860 while processorprice = 2.5993830
         # filtering it out
         retail_wholesale_ratio < 20) %>%
  ggplot(aes(x=month, y=retail_wholesale_ratio)) +
  geom_line(color=cbPalette[4], size=1) +
  facet_wrap("inv_type_name") +
  theme(legend.position=c(0.9,0.9), legend.justification = c(1,1)) +
  labs(title="Avg Sale Price / Avg Processor Price",
       x="Month",
       y="Retail to Wholesale Ratio")


# adding taxes -------------------------


### including time
avg.sale.month <- dispensing.select %>%
  mutate(monthyear = floor_date(saletime, "month")) %>%
  group_by(inventorytype, monthyear) %>%
  summarise(avg_saleprice = median(price, na.rm=T)) %>%
  # for some reason the join didn't work on the monthyear / date variable
  # so I created a numeric variable for joining the dataframes
  mutate(monthjoin = as.numeric(monthyear))

avg.proc.month <- process.to.retail %>%
  mutate(monthyear = floor_date(inv_date, "month")) %>%
  group_by(trans_invtype, monthyear) %>%
  summarise(avg_processorprice = median(trans_unitprice, na.rm=T)) %>%
  # for some reason the join didn't work on the monthyear / date variable
  # so I created a numeric variable for joining the dataframes
  mutate(monthjoin = as.numeric(monthyear))

## line graph of sale price and producer price, over time
avg.sale.month %>%
  left_join(avg.proc.month, by=c("monthjoin", "inventorytype" = "trans_invtype")) %>%
  # needed to drop and rename some variables because of the weird join
  select(inventorytype, month = monthyear.x, avg_saleprice, avg_processorprice) %>%
  # gather so we get one value per row
  tidyr::gather(supplystep, avg_price, c(avg_saleprice, avg_processorprice)) %>%
  left_join(inventory.types, by="inventorytype") %>%
  filter(!is.na(inventorytype), inv_type_name!="Capsule", inv_type_name!="Suppository",
         inv_type_name!="Tincture") %>%
  ggplot(aes(x=month, y=avg_price, color=supplystep)) +
  geom_line() +
  facet_wrap("inv_type_name") +
  theme(legend.position=c(0.65,0.15))

## line graph of sale price and producer price, over time
## for only useable, inhalation, edibles (solid)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
avg.sale.month %>%
  left_join(avg.proc.month, by=c("monthjoin", "inventorytype" = "trans_invtype")) %>%
  # needed to drop and rename some variables because of the weird join
  select(inventorytype, month = monthyear.x, avg_saleprice, avg_processorprice) %>%
  # gather so we get one value per row
  tidyr::gather(supplystep, avg_price, c(avg_saleprice, avg_processorprice)) %>%
  left_join(inventory.types, by="inventorytype") %>%
  filter(inv_type_name=="Usable Marijuana" | inv_type_name=="Marijuana Extract for Inhalation" |
           inv_type_name=="Solid Marijuana Infused Edible") %>%
  ggplot(aes(x=month, y=avg_price, color=supplystep)) +
  scale_color_manual(values=c(cbPalette[2], cbPalette[3])) +
  geom_line(size=1.5) +
  facet_wrap("inv_type_name") +
  theme(legend.position=c(0.9,0.9), legend.justification = c(1,1)) +
  labs(title="Avg Sale and Producer Prices \nfor Top Types",
       x="Month",
       y="Average Price") +
  geom_line(size=1.5) +
  facet_wrap("inv_type_name") +
  theme(legend.position=c(0.9,0.9), legend.justification = c(1,1))

## line graph of sale price and producer price, over time
## for only useable, inhalation, edibles (solid)
## ratio of retail / wholesale
avg.sale.month %>%
  left_join(avg.proc.month, by=c("monthjoin", "inventorytype" = "trans_invtype")) %>%
  # needed to drop and rename some variables because of the weird join
  select(inventorytype, month = monthyear.x, avg_saleprice, avg_processorprice) %>%
  # get ratio of retail / wholesale for each month
  group_by(inventorytype, month) %>%
  summarise(retail_wholesale_ratio = avg_saleprice / avg_processorprice) %>%
  left_join(inventory.types, by="inventorytype") %>%
  filter(inv_type_name=="Usable Marijuana" | inv_type_name=="Marijuana Extract for Inhalation" |
           inv_type_name=="Solid Marijuana Infused Edible",
         # there was a spike that hit 24 but it seemed like an anomoly
         # avg_saleprice = 63.860 while processorprice = 2.5993830
         # filtering it out
         retail_wholesale_ratio < 20) %>%
  ggplot(aes(x=month, y=retail_wholesale_ratio)) +
  geom_line(color=cbPalette[4], size=1) +
  facet_wrap("inv_type_name") +
  theme(legend.position=c(0.9,0.9), legend.justification = c(1,1)) +
  labs(title="Avg Sale Price / Avg Processor Price",
       x="Month",
       y="Retail to Wholesale Ratio")




# # connect to dispensing df
# # dispensing
# dispensing <- readr::read_csv("../data/Dec2016/biotrackthc_dispensing.csv")
# dispensing$saletime <- as.POSIXct(dispensing$sessiontime,
#                                    origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
# dispensing$saletime <- as.Date(dispensing$saletime)
# dispensing <- left_join(dispensing, inventory.types, by="inventorytype")
# dispensing$inv_type_name <- as.factor(dispensing$inv_type_name)
# #dispensing$refunded <- as.logical(dispensing$refunded)
# # no.refund.list <- as.data.frame(dispensing$dispensingid[!dispensing$refunded])
# retail.select <- dispensing %>%
#   #   # filtering out refunded for now. Make up 0.2% of df.
#   #   filter(dispensingid %in% no.refund.list) %>%
#   select(dispensingid = id, retail_invid = inventoryid, retail_saletime = saletime, 
#          retail_location = location,
#          retail_weight = weight, retail_inv_type = inv_type_name, retail_price = price)
# # get retail location names & cities
# loc.name.city <- select(locations, retail_name = name, retail_license = licensenum, location_id, 
#                         retail_city = city, retail_type = locationtypeNames)
# retail.select <- left_join(retail.select, loc.name.city, by=c("retail_location" = "location_id"))
# 
# # working through the logic:
# # dispening inventoryid gets you into the inventory
# # where teh parentid gets you the inventoryid that connecs to the
# # transfers / processors
# # 17:30 2/22: this does not work
# inv.simp <- select(inventory.select, inv_invid, inv_invparentid, inv_date)
# dispensing.history <- retail.select %>%
#   left_join(inv.simp, by=c("retail_invid" = "inv_invid")) %>%
#   rename(retail_inv_date = inv_date, retail_parentid = inv_invparentid) %>%
#   left_join(process.to.retail, by=c("retail_parentid" = "trans_invid"))
# 
# 
# # sample -------
# sample.list <- sample(dispensing.history$dispensingid, 20000, replace=F)
# sample.dispensing.history <- dplyr::filter(dispensing.history, dispensingid %in% sample.list)
# write.table(sample.dispensing.history, file="../data/Dec2016/cleaned/samples/retail_history_sample.csv", 
#             sep=",", row.names = F, col.names = T)
# 
# 
# # clean up environment --------
# rm(locations)
# rm(inventory)
# rm(transfers)
# rm(dispensing)
# rm(inventory.types)
# rm(trans.loc.name.city)
# 
# 
# 
# ########### RUN UP TO HERE ##################### -----------------
# 
# # sample --------
# #retail.list <- sample(dispensing.history$dispensingid, 20000, replace=F)
# #retail.sample <- dplyr::filter(retail, dispensingid %in% retail.list)
# #write.table(retail.sample, file="../data/Dec2016/cleaned/retail_sample.csv", sep=",", row.names = F, col.names = T)
# 
# 
# # clustering on types ------------
# 
# 
# 
# 
# # testing & anomolies --------------
# 
# # testing / exploring why some inbound locations are NA
# # still have inbound liscense number so could just be error?
# trans_in_loc_NA <- transfers.processor %>%
#   filter(is.na(transfers.processor$trans_inbound_loc)) %>%
#   select(transferid, trans_out_lisc, trans_in_lisc, trans_inbound_loc, trans_invtype,
#          trans_saleprice, trans_unitprice) %>%
#   left_join(locations.name.city, by=c("trans_out_lisc" = "licensenum")) %>%
#   rename(out_name = name, out_locid = location_id, out_city = city) %>%
#   left_join(locations.name.city, by=c("trans_in_lisc" = "licensenum")) %>%
#   rename(in_name = name, in_locid = location_id, in_city = city)
# #write.table(trans_in_loc_NA, file="../data/Dec2016/cleaned/testing/transfers_locationNA.csv", sep=",", row.names=F)
# 
# 
# # processors <- inventory.processor %>%
# #   dplyr::left_join(transfers.select, by=c("inv_inventoryparentid" = "trans_inventoryid")) %>%
# #   dplyr::filter(trans_saleprice > 0, trans_unitprice > 0)

# there are some duplicate liscense numbers -- trying to explore
dup.license <- locations %>%
  group_by(licensenum) %>%
  mutate(count = n()) %>%
  filter(count > 1) %>%
  select(name, address1, city, locationtypeNames, licensenum, location_id, status, mailaddress1, producer, processor, retail,
         # wtf is transactionid inside locations?!
         transactionid, maxDate) %>%
  arrange(licensenum, status, locationtypeNames) %>%
  # some of the pairs were one were close and one opened, so filtering down to only where they're both active
  filter(!(licensenum %in% licensenum[status=="CLOSED (PERMANENT)"]))
