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

# recreating Steve's retail df ------
retail <- dispensing %>%
  dplyr::left_join(inventory, by="inventoryid") %>%
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
write.table(retail.sample, file="../data/Dec2016/cleaned/retail_sample.csv", sep=",", row.names = F, col.names = T)
#lm(formula = price ~ usableweight + inv_type_name + CBD + THC + Total, data=retail)

#write.table(retail, file="../data/Dec2016/cleaned/retail_detail.csv", sep=",", row.names = F, col.names = T)