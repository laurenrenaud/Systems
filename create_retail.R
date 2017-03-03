library(dplyr)
library(readr)

# pull in & clean dfs ------
# locatons
locations <- read.csv("../data/Dec2016/cleaned/locations/all_locations.csv", sep=",", header=T)
locations.name.city <- select(locations, retailname = name, licensenum, location_id, city)
retail.loc <- filter(locations, retail==1)
# inventory
inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
inventory.types <- read.csv("../data/Dec2016/cleaned/inventory_type.csv", sep=",", header=T)
inventory$sessiontime <- as.POSIXct(inventory$sessiontime,
                                    origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
inventory$inv_date <- as.Date(inventory$sessiontime)
inventory <- rename(inventory, inventoryid = id)
inventory <- left_join(inventory, inventory.types, by="inventorytype")
inventory$sample_id <- as.numeric(inventory$sample_id)
inventory.retail <- filter(inventory, location %in% retail.loc$location_id)
# dispensing
dispensing <- readr::read_csv("../data/Dec2016/biotrackthc_dispensing.csv")
dispensing$monthtime <- as.POSIXct(dispensing$sessiontime,
                                   origin = "1970-01-01", tz="America/Los_Angeles") # LA = PST
dispensing$monthtime <- as.Date(dispensing$monthtime)
dispensing <- left_join(dispensing, inventory.types, by="inventorytype")
dispensing$inv_type_name <- as.factor(dispensing$inv_type_name)
dispensing <- rename(dispensing, dispensingid = id)

# labs -------
potency <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_potency_analysis.csv")
labsamples <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_samples.csv")
labsamples$inventoryid <- as.numeric(labsamples$inventoryid)
potency_tidy <- potency %>%
  # was going to use tidyr to spread variable names but some sample_ids have up to 15 tests
  # so summarizing first, assuming max() is most accurate and that others might be missing
  # values but need to check
  dplyr::group_by(sample_id, name) %>%
  dplyr::summarise(value = max(value)) %>%
  # spread to get column each for THC, THCA, CBD, Total
  tidyr::spread(name, value) %>%
  # CBDA was a column of all null or 0
  dplyr::select(-(CBDA)) %>%
  dplyr::left_join(labsamples, by=c("sample_id" = "id")) %>%
  # select only variables for retail set
  # only including test_inventorytype and product name to confirm against inventory
  # but should be duplicative
  dplyr::select(sample_id, CBD, THC, THCA, Total, test_inventorytype = inventorytype,
                test_productname = product_name, inventoryid, inventoryparentid)

library(ggplot2)
potency_tidy %>%
  dplyr::filter(Total < 100) %>%
  ggplot(aes(x=Total)) +
  geom_density(fill="olivedrab")

hist(potency_tidy$Total[potency_tidy$Total<100])
hist(potency_tidy$Total[potency_tidy$Total<20])
hist(potency_tidy$Total[potency_tidy$Total<5])
sum(potency_tidy$Total==0, na.rm=T) / nrow(potency_tidy)
sum(is.na(potency_tidy$test_productname)) / nrow(potency_tidy)

# recreating Steve's retail df ------
retail <- dispensing %>%
  dplyr::left_join(inventory.retail, by="inventoryid") %>%
  dplyr::left_join(potency_tidy, by="inventoryparentid") %>%
  dplyr::select(dispensingid, location = location.x, price, usableweight = usableweight.x,
                inv_type_name = inv_type_name.x, inventoryid = inventoryid.x,
                strain, productname, CBD, THC, THCA, Total, weight = weight.x, saletime = monthtime,
                transactionid = transactionid.x, deleted = deleted.x, refunded, inventorytype = inventorytype.x,
                inventoryparentid) %>%
  left_join(locations.name.city, by=c("location" = "location_id"))

# sample -----
retail.list <- sample(retail$dispensingid, 20000, replace=F)
retail.sample <- dplyr::filter(retail, dispensingid %in% retail.list)
write.table(retail.sample, file="../data/Dec2016/cleaned/samples/retail_sample.csv", sep=",", row.names = F, col.names = T)
#lm(formula = price ~ usableweight + inv_type_name + CBD + THC + Total, data=retail)

#write.table(retail, file="../data/Dec2016/cleaned/retail_detail.csv", sep=",", row.names = F, col.names = T)

########################################################
######## STOP HERE TO GENERATE RETAIL SET + SAMPLE #####
########################################################


# recreating retail df for Pullman ------
retail.pullman <- dispensing %>%
  dplyr::filter(location %in% c("457","910","1751","1761","1874")) %>%
  dplyr::left_join(inventory, by="inventoryid") %>%
  dplyr::select(dispensingid, location = location.x, price, usableweight = usableweight.x,
                inv_type_name = inv_type_name.x, retail_invdate = inv_date,
                strain, sale_prodname = productname, weight = weight.x, saletime = monthtime,
                transactionid = transactionid.x, deleted = deleted.x, refunded, inventorytype = inventorytype.x,
                dis_inventoryparentid = inventoryparentid, dis_invid = inventoryid, dis_parentid = parentid) %>%
  left_join(locations.name.city, by=c("location" = "location_id"))

retail.pullman$dis_parentid <- as.numeric(retail.pullman$dis_parentid)

# trying to connect to transfers to get producers
locations <- readr::read_csv("../data/Dec2016/cleaned/locations/all_locations.csv")
loc.types <- readr::read_csv("../data/Dec2016/cleaned/locations/locationtypes.csv")
locations <- left_join(locations, loc.types, by=c("locationtype" = "loctypeCode"))
#write.table(locations, file="../data/Dec2016/cleaned/locations/all_locations.csv", sep=",", row.names=F)

loc.simp <- locations %>%
  left_join(loc.types, by=c("locationtype" = "loctypeCode")) %>%
  select(location_id, name, typesimp)

transfers <- readr::read_csv("../data/Dec2016/biotrackthc_inventorytransfers.csv")
transfers$inventoryid <- as.numeric(transfers$inventoryid)
transfers$parentid <- as.numeric(transfers$parentid)
transfers.select <- transfers %>%
  select(transferid = id, inventoryid, trans_loc = location, 
         trans_strain = strain, inventorytype, trans_parentid = parentid) %>%
  left_join(inventory.types, by="inventorytype") %>%
  select(-(inventorytype), trans_invtype = inv_type_name) %>%
  left_join(loc.simp, by=c("trans_loc" = "location_id")) %>%
  rename(processor_name = name)


# interesting -- percent of transfers from each type
transfers.select %>%
  group_by(typesimp) %>%
  summarise(perc = n() / nrow(transfers.select))

# there were some (1.8%) of dispensing IDs that came up duplicate on this join
# it appears they have multiple transferIDs that are causing the problem
# need to look into it further, but dropping them for now
dupe.disID <- retail.pullman %>%
  left_join(transfers.select, by=c("dis_invid" = "inventoryid")) %>%
  group_by(dispensingid) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  arrange(desc(count))

# now joining transfers to retail.pullman to get processor names attached to items
retail.pullman <- retail.pullman %>%
  filter(!dispensingid %in% dupe.disID$dispensingid) %>%
  # the inventory id from the dispensing file matches the inventory id in the transfers file
  # maybe we don't even need to go through transfers?
  # checked that yes, trans_loc goes one for oen with processor_name
  left_join(transfers.select, by=c("dis_invid" = "inventoryid")) %>%
  # then the parent id from the dispening file matches the inventory id of the processor's inventory
  left_join(inventory, by=c("dis_parentid" = "inventoryid")) %>%
  select(dispensingid, retail_loc = location.x, saleprice = price, saleuseweight = usableweight.x, sale_invtype = inv_type_name.x,
         sale_strain = strain.x, sale_prodname, saleweight = weight.x, saletime, retail_invdate, transactionid = transactionid.x,
         refunded, dis_invid, retailname, trans_loc, trans_strain, trans_invtype, trans_parentid,
         processor_name, processortype = typesimp, process_strain = strain.y, process_weight = weight.y, process_transctionid = transactionid.y,
         process_productname = productname, processinv_date = inv_date, processor_invtype = inv_type_name.y,
         processor_invid = dis_parentid)

#write.table(retail.pullman, file="../data/Dec2016/cleaned/samples/pullman_retailAll.csv", row.names=F, sep=",")

pullman.select <- retail.pullman %>%
  select(dispensingid, retail_loc, saleprice, saleuseweight, sale_invtype, sale_prodname, saleweight, saletime, transactionid,
         refunded, retailname, processor_name, processortype, process_productname, processinv_date, processor_invtype)

write.table(pullman.select, file="../data/Dec2016/cleaned/samples/pullman_retailSelectVariables.csv", row.names=F, sep=",")



# number of unique dispensing ids before adding others = 474435
# number of unique dis_parent ids before adding others = 7876
# nubmer of unique dis_parentids / processor_invid after adding others = 7575


## looks like most of the times that a single processor inventory ID leads to multiple 
## retail inventory productnames that they're very similar names
## off by number of grams, or some slight name change
productnames <- test %>%
  filter(!is.na(processor_invid)) %>%
  group_by(processor_invid, sale_prodname) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(processor_invid) %>%
  summarise(numofproducts = n()) %>%
  arrange(desc(numofproducts)) %>%
  filter(numofproducts > 1)

productnames2 <- test %>%
  filter(processor_invid %in% productnames$processor_invid) %>%
  group_by(processor_invid, sale_prodname) %>%
  summarise(count = n()) %>%
  arrange(processor_invid)

hist(productnames$numofproducts)

sum(productnames$numofproducts==1) / nrow(productnames)


processor.invtime <- retail.pullman %>%
  group_by(dispensingid) %>%
  mutate(process_invtime = retail_invdate - processinv_date) %>%
  select(dispensingid, retail_invdate, processinv_date, process_invtime)


# testing these joins
test_step1 <- retail.pullman %>%
  filter(!dispensingid %in% selectingdupes$dispensingid) %>%
  left_join(transfers.select, by=c("dis_invid" = "inventoryid"))

# now joining to processors' inventory
test_step2 <- test_step1 %>%
  left_join(inventory, by=c("dis_parentid" = "inventoryid")) %>%
  select(dispensingid, retail_loc = location.x, saleprice = price, saleuseweight = usableweight.x, sale_invtype = inv_type_name.x,
         sale_strain = strain.x, sale_prodname, saleweight = weight.x, saletime, transactionid = transactionid.x,
         refunded, dis_invid, retailname, trans_loc, trans_strain, trans_invtype, trans_parentid,
         processor_name, processortype = typesimp, process_strain = strain.y, process_weight = weight.y, process_transctionid = transactionid.y,
         process_productname = productname, processinv_date = inv_date, processor_invtype = inv_type_name.y)

test_step2 <- test_step1 %>%
  left_join(inventory, by=c("dis_parentid" = "inventoryid")) %>%
  select(dispensingid, sale_strain = strain.x, process_strain = strain.y, sale_prodname = productname.x, process_productname = productname.y,
         sale_invtype = inv_type_name.x, trans_invtype, processor_invtype = inv_type_name.y)

testingjoin <- test_step2 %>%
  group_by(dispensingid) %>%
  summarise(
    straintest = (sale_strain == process_strain),
    nametest = (sale_prodname == process_productname),
    typetest = (sale_invtype == processor_invtype),
    transtype = (sale_invtype == trans_invtype)
  ) 

table(testingjoin$straintest)
table(testingjoin$nametest)
table(testingjoin$typetest)
table(testingjoin$transtype)

 # how many do not match between sales and processor inventory
sum(testingjoin$typetest, na.rm=T) / nrow(testingjoin)

# checking mismatched inventory types
testing_types <- test_step2 %>%
  filter(dispensingid %in% testingjoin$dispensingid[!testingjoin$typetest]) %>%
  select(dispensingid, sale_invtype, processor_invtype) %>%
  group_by(sale_invtype, processor_invtype) %>%
  summarise(count = n())
  
# checking mismatched product names
test_step2$sale_strain <- as.factor(test_step2$sale_strain)
testing_productnames <- test_step2 %>%
  filter(dispensingid %in% testingjoin$dispensingid[!testingjoin$typetest]) %>%
  select(dispensingid, sale_prodname, process_productname) %>%
  group_by(sale_prodname, process_productname) %>%
  summarise(count = n())





testing <- test %>%
  group_by(dispensingid) %>%
  summarise(straintest = (strain == trans_strain),
            typetest = (inventorytype == trans_invtype))

selectingdupes <- test %>%
  group_by(dispensingid) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  arrange(desc(count))

# no duplicates in dispensing
# no duplicate dispensingid in retail.pullman


## checking Steve's undestanding of what 'inventory' is--------------
## the entries appear to be increasing over time, which I
## think means there's more product added, market it growing
## not that old product is not included in inventory
inventory %>%
  ggplot(aes(x=inv_date)) +
  geom_histogram()

## also, when joining on inventory from dispensing for pullman
## you don't lose a ton of entries (you lose the ones that are in the
## duplicate transfer IDs but that's it I think)
length(unique(retail.pullman$dispensingid))
#[1] 474435

length(unique(retail.pullman.checking$dispensingid))
#[1] 465835
