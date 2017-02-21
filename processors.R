library(dplyr)
library(readr)

# set up ------------------
# locatons
locations <- read.csv("../data/Dec2016/cleaned/locations/all_locations.csv", sep=",", header=T)
locations.name.city <- select(locations, name, licensenum, location_id, city)
processor.loc <- filter(locations, processor==1)

# inventory
inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
inventory.types <- read.csv("../data/Dec2016/cleaned/inventory_type.csv", sep=",", header=T)
inventory$sessiontime <- as.POSIXct(inventory$sessiontime,
                                    origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
inventory$inv_date <- as.Date(inventory$sessiontime)
inventory <- rename(inventory, inventoryid = id)
inventory <- left_join(inventory, inventory.types, by="inventorytype")
inventory$sample_id <- as.numeric(inventory$sample_id)
inventory.select <- select(inventory, inv_inventoryid = inventoryid, inv_strain = strain, inv_weight = weight,
                           inv_location = location, inv_inv_type = inv_type_name, seized, inv_deleted = deleted,
                           inv_usableweight = usableweight, inv_inventoryparentid = inventoryparentid,
                           inv_productname = productname, inv_date)
inventory.processor <- filter(inventory.select, inv_location %in% processor.loc$location_id)

# transfers
transfers <- readr::read_csv("../data/Dec2016/biotrackthc_inventorytransfers.csv")
transfers$inventoryid <- as.numeric(transfers$inventoryid)
transfers.select <- transfers %>%
  #filter(inventoryparentid %in% dis.select$inventoryid) %>%
  select(transferid = id, trans_inventoryid = inventoryid, trans_strain = strain, trans_location = location,
         trans_parentid = parentid, trans_invtype = inventorytype, trans_usableweight = usableweight,
         trans_out_lisc = outbound_license, trans_in_lisc = inbound_license, trans_inbound_loc = inbound_location,
         trans_saleprice = saleprice, trans_unitprice = unitprice)
# get type names
transfers.select <- left_join(transfers.select, inventory.types, by=c("trans_invtype" = "inventorytype"))
# get location names
trans.loc.name.city <- select(locations, trans_name = name, trans_license = licensenum, location_id,
                              trans_city = city, trans_loc.type = locationtypeNames)
transfers.select <- transfers.select %>%
  left_join(trans.loc.name.city, by=c("trans_location" = "location_id"))
transfers.select$trans_in_lisc <- as.integer(transfers.select$trans_in_lisc)
# for transfers, get types of locations, in and outbound
locations.name.city <- select(locations, name, licensenum, location_id, city, locationtypeNames)
transfers.select <- transfers.select %>%
  left_join(locations.name.city, by=c("trans_out_lisc" = "licensenum")) %>%
  rename(out_name = name, out_locid = location_id, out_city = city, out_type = locationtypeNames) %>%
  left_join(locations.name.city, by=c("trans_in_lisc" = "licensenum")) %>%
  rename(in_name = name, in_locid = location_id, in_city = city, in_type = locationtypeNames)

# get processor transfers
transfers.processor <- filter(transfers.select, trans_out_lisc %in% processor.loc$licensenum)
transfers.processor <- inventory.select %>%
  select(inv_inventoryparentid, inv_inventoryid, inv_date) %>%
  left_join(transfers.processor, by=c("inv_inventoryparentid" = "trans_inventoryid"))

# clean transfers.processor ------------
trans.proc.clean <- transfers.processor %>%
  filter(trans_loc.type != "Producer Tier 2", 
         trans_unitprice > 0,
         trans_saleprice > 0,
         !is.na(trans_inbound_loc)) 

# processor to retail only -----------
process.to.retail <- trans.proc.clean %>%
  filter(in_type=="Retailer" | in_type=="Retailer & Medical")

# connect to dispensing df
# dispensing
dispensing <- readr::read_csv("../data/Dec2016/biotrackthc_dispensing.csv")
dispensing$monthtime <- as.POSIXct(dispensing$sessiontime,
                                   origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
dispensing$monthtime <- as.Date(dispensing$monthtime)
dispensing <- left_join(dispensing, inventory.types, by="inventorytype")
dispensing$inv_type_name <- as.factor(dispensing$dis_inv_type)
dispensing <- rename(dispensing, dispensingid = id)
#dispensing$refunded <- as.logical(dispensing$refunded)
# no.refund.list <- as.data.frame(dispensing$dispensingid[!dispensing$refunded])
retail.select <- dispensing %>%
  #   # filtering out refunded for now. Make up 0.2% of df.
  #   filter(dispensingid %in% no.refund.list) %>%
  select(dispensingid, retail_invid = inventoryid, retail_saletime = monthtime, 
         retail_location = location,
         retail_weight = weight, retail_inv.type = inv_type_name, retail_price = price)
# get retail location names & cities
loc.name.city <- select(locations, retail_name = name, retail_license = licensenum, location_id, 
                        retail_city = city, retail_type = locationtypeNames)
retail.select <- left_join(retail.select, loc.name.city, by=c("retail_location" = "location_id"))

dispensing.history <- retail.select %>%
  left_join(process.to.retail, by=c("retail_invid" = "inv_inventoryid"))

# sample -------
retail.list <- sample(dispensing.history$dispensingid, 20000, replace=F)
sample.dispensing.history <- dplyr::filter(dispensing.history, dispensingid %in% retail.list)
#write.table(sample.dispensing.history, file="../data/Dec2016/cleaned/samples/retail_history_sample.csv", 
#            sep=",", row.names = F, col.names = T)

# clean up environment --------
rm(locations)
rm(inventory)
rm(transfers)
rm(dispensing)
rm(inventory.types)
rm(trans.loc.name.city)

# sample --------
#retail.list <- sample(dispensing.history$dispensingid, 20000, replace=F)
#retail.sample <- dplyr::filter(retail, dispensingid %in% retail.list)
#write.table(retail.sample, file="../data/Dec2016/cleaned/retail_sample.csv", sep=",", row.names = F, col.names = T)


# clustering on types ------------




# testing & anomolies --------------

# testing / exploring why some inbound locations are NA
# still have inbound liscense number so could just be error?
trans_in_loc_NA <- transfers.processor %>%
  filter(is.na(transfers.processor$trans_inbound_loc)) %>%
  select(transferid, trans_out_lisc, trans_in_lisc, trans_inbound_loc, trans_invtype,
         trans_saleprice, trans_unitprice) %>%
  left_join(locations.name.city, by=c("trans_out_lisc" = "licensenum")) %>%
  rename(out_name = name, out_locid = location_id, out_city = city) %>%
  left_join(locations.name.city, by=c("trans_in_lisc" = "licensenum")) %>%
  rename(in_name = name, in_locid = location_id, in_city = city)
#write.table(trans_in_loc_NA, file="../data/Dec2016/cleaned/testing/transfers_locationNA.csv", sep=",", row.names=F)


# processors <- inventory.processor %>%
#   dplyr::left_join(transfers.select, by=c("inv_inventoryparentid" = "trans_inventoryid")) %>%
#   dplyr::filter(trans_saleprice > 0, trans_unitprice > 0)

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
