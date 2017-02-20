library(dplyr)
library(readr)

inventory.types <- read.csv("../data/Dec2016/cleaned/inventory_type.csv", sep=",", header=T)
# locatons
locations <- read.csv("../data/Dec2016/cleaned/locations/all_locations.csv", sep=",", header=T)
retail.loc <- filter(locations, retail==1)
loc.name.city <- select(locations, name, licensenum, location_id, city, locationtypeNames)

# dispensing
dispensing <- readr::read_csv("../data/Dec2016/biotrackthc_dispensing.csv")
dispensing$monthtime <- as.POSIXct(dispensing$sessiontime,
                                   origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
dispensing$monthtime <- as.Date(dispensing$monthtime)
dispensing <- left_join(dispensing, inventory.types, by="inventorytype")
dispensing$inv_type_name <- as.factor(dispensing$inv_type_name)
dispensing <- rename(dispensing, dispensingid = id)
# dispensing$refunded <- as.logical(dispensing$refunded)
# no.refund.list <- as.data.frame(dispensing$dispensingid[!dispensing$refunded])
dis.select <- dispensing %>%
  #   # filtering out refunded for now. Make up 0.2% of df.
  #   filter(dispensingid %in% no.refund.list) %>%
  select(dispensingid, inventoryid, retail_saletime = sessiontime, retail_location = location,
         retail_weight = weight, retail_inv.type = inv_type_name, retail_price = price)
# get retail location names & cities
loc.name.city <- select(locations, retail_name = name, retail_license = licensenum, location_id, 
                        retail_city = city, retail_type = locationtypeNames)
dis.select <- left_join(dis.select, loc.name.city, by=c("retail_location" = "location_id"))

# inventory
inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
inventory$sessiontime <- as.POSIXct(inventory$sessiontime,
                                    origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
inventory$inv_date <- as.Date(inventory$sessiontime)
inventory <- rename(inventory, inventoryid = id)
inventory <- left_join(inventory, inventory.types, by="inventorytype")
inventory$sample_id <- as.numeric(inventory$sample_id)
#inventory.retail <- filter(inventory, location %in% retail.loc$location_id)
inv.select <- inventory %>%
  filter(location %in% retail.loc$location_id) %>%
  # long to load but filtering to exclude those refunded
  # refunds take up 0.2% of records
  #filter(inventoryid %in% no.refund.list) %>%
  select(inventoryid, inv_strain = strain, inv_loc = location, inv_inv_type = inv_type_name, 
         inv_date, inv_usableweight = usableweight, inv_productname = productname,
         inv_inventoryparentid = inventoryparentid)

# transfers
transfers <- readr::read_csv("../data/Dec2016/biotrackthc_inventorytransfers.csv")
transfers$inventoryid <- as.numeric(transfers$inventoryid)
transfers.select <- transfers %>%
  #filter(inventoryparentid %in% dis.select$inventoryid) %>%
  select(transferid = id, trans_inventoryid = inventoryid, trans_strain = strain, trans_location = location,
         trans_parentid = parentid, trans_inventorytype = inventorytype, trans_usableweight = usableweight,
         outbound_license, inbound_license, inbound_location, trans_saleprice = saleprice, trans_unitprice = unitprice)
# get type names
transfers.select <- left_join(transfers.select, inventory.types, by=c("trans_inventorytype" = "inventorytype"))
# get location names
trans.loc.name.city <- select(locations, trans_name = name, trans_license = licensenum, location_id,
                              trans_city = city, trans_loc.type = locationtypeNames)
transfers.select <- transfers.select %>%
  left_join(trans.loc.name.city, by=c("trans_location" = "location_id"))

# without potency ----------
# first comparing price and producers without including THC / CBD numbers
# question to think about: should we try comparing something like price per THC instead
# of price? or is it ok to just compare on price, because higher THC is part of the 
# higher prestige aspect? Should try both. Initially doing without THC join

# first on inhalants only
inhalants <- dis.select %>%
  dplyr::filter(retail_inv.type==24) %>%
  dplyr::left_join(inv.select, by="inventoryid") %>%
  dplyr::left_join(transfers.select, by=c("inv_inventoryparentid" = "trans_inventoryid"))

names(inhalants)
unique(inhalants$trans_inventorytype)
#write.table(inhalants, file="../data/Dec2016/cleaned/samples/inhalants.csv", row.names=F, col.names = T, sep=",")

table(inhalants$trans_loc.type)
unique(inhalants$trans_name)

inhalants %>%
  dplyr::group_by(trans_name) %>%
  dplyr::summarise(
    mean_retail = mean(retail_price, na.rm=T),
    median_retail = median_sales(retail_price, na.rm=T),
    max_retail = max(retail_price, na.rm=T)#,
    # # transfer prices
    # mean_transsales = mean(trans_saleprice, na.rm=T),
    # median_transsales = median(trans_saleprice, na.rm=T),
    # max_transsales = max(trans_saleprice, na.rm=T),
    # mean_transunit = mean(trans_unitprice, na.rm=T),
    # median_transunit = median(trans_unitprice, na.rm=T),
    # max_transunit = max(trans_unitprice, na.rm=T)
  ) %>%
  dplyr::arrange(mean_retail) %>%
  ggplot(aes(x=trans_name, y=retail_price)) +
  geom_point(alpha=0.5) +
  labs(
    title = "Producers mean sale price",
    x= "Producers",
    y="Mean sale price"
  ) 
  
  
  
  