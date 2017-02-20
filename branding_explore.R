library(dplyr)
library(readr)

inventory.types <- read.csv("../data/Dec2016/cleaned/inventory_type.csv", sep=",", header=T)

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
  select(dispensingid, inventoryid, dis_sessiontime = sessiontime, dis_location = location,
         dis_weight = weight, inv_invtype = inventorytype, dis_inv_type_name = inv_type_name)

# locatons
locations <- read.csv("../data/Dec2016/cleaned/locations/all_locations.csv", sep=",", header=T)
retail.loc <- filter(locations, retail==1)
retailloc.name.city <- select(retail.loc, name, licensenum, location_id, city)

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
  select(inventoryid, inv_strain = strain, inv_loc = location, inv_inv_type_name = inv_type_name, 
         inv_date, inv_usableweight = usableweight, inv_productname = productname,
         inv_inventoryparentid = inventoryparentid)

# transfers
transfers <- readr::read_csv("../data/Dec2016/biotrackthc_inventorytransfers.csv")
transfers$inventoryid <- as.numeric(transfers$inventoryid)
transfers.select <- transfers %>%
  #filter(inventoryparentid %in% dis.select$inventoryid) %>%
  select(transferid = id, trans_inventoryid = inventoryid, trans_strain = strain, trans_location = location,
         trans_parentid = parentid, trans_inventorytype = inventorytype, trans_usableweight = usableweight,
         outbound_license, inbound_license, inbound_location) %>%
  left_join(retailloc.name.city, by=c("trans_location" = "location_id"))

# without potency ----------
# first comparing price and producers without including THC / CBD numbers
# question to think about: should we try comparing something like price per THC instead
# of price? or is it ok to just compare on price, because higher THC is part of the 
# higher prestige aspect? Should try both. Initially doing without THC join

# first on inhalants only
inhalants <- dis.select %>%
  dplyr::filter(inv_invtype==24) %>%
  dplyr::left_join(inv.select, by="inventoryid") %>%
  dplyr::left_join(transfers.select, by=c("inv_inventoryparentid" = "trans_inventoryid"))

names(inhalants)