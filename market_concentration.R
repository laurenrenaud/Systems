library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
options(scipen=20)

# pull in & clean dfs ---------
# locatons
locations <- read.csv("../data/Dec2016/cleaned/locations/all_locations.csv", sep=",", header=T)

# lists for each type
loc.retail <- locations %>%
  filter(typesimp=="Retailer") %>%
  select(location_id, licensenum)
loc.processor <- locations %>%
  filter(typesimp=="Processor") %>%
  select(location_id, licensenum)
loc.prod_proc <- locations %>%
  filter(typesimp=="Producer-Processor") %>%
  select(location_id, licensenum)
loc.producer <- locations %>%
  filter(typesimp=="Producer") %>%
  select(location_id, licensenum)

# inventory
inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
inventory.types <- read.csv("../data/Dec2016/cleaned/inventory_type.csv", sep=",", header=T)
# convert date
inventory$inv_date <- as.Date(as.POSIXct(inventory$sessiontime,
                                         origin = "1970-01-01", tz="America/Los_Angeles"))
inventory <- rename(inventory, inventoryid = id)
# bring in inventory types names
inventory <- left_join(inventory, inventory.types, by="inventorytype")

inventory$sample_id <- as.numeric(inventory$sample_id)
inventory.retail <- filter(inventory, location %in% loc.retail$location_id)
# dispensing
dispensing <- readr::read_csv("../data/Dec2016/biotrackthc_dispensing.csv")
dispensing$monthtime <- as.Date(as.POSIXct(dispensing$sessiontime,
                                           origin = "1970-01-01", tz="America/Los_Angeles"))
# editing for post July tax change
dispensing$price_x <- ifelse(dispensing$monthtime >= "2015-07-01", 
                             dispensing$price*1.37, 
                             dispensing$price)
# get inventory type names
dispensing <- left_join(dispensing, inventory.types, by="inventorytype")
dispensing$inv_type_name <- as.factor(dispensing$inv_type_name)
dispensing <- rename(dispensing, dispensingid = id)

# get generalized product groups ------
source("categorization_function.R")
# for retail
retail_producttypes <- as.data.frame(unique(dispensing$inv_type_name))
colnames(retail_producttypes) <- "inv_type_name"
# use categorization function to get grouping for each product type
retail_producttypes <- retail_producttypes %>%
  dplyr::filter(!is.na(inv_type_name)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(product_group = groupAllProductTypes(inv_type_name))
# join back to transfers df
dispensing$inv_type_name <- as.factor(dispensing$inv_type_name)
dispensing <- left_join(dispensing, retail_producttypes, by="inv_type_name")


# calculate for retailers -----
retailers_allproducts <- dispensing %>%
  dplyr::mutate(salesquarter = quarter(monthtime, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter) %>%
  dplyr::mutate(allsales = sum(price_x, na.rm=T)) %>%
  dplyr::group_by(location, salesquarter) %>%
  dplyr::summarise(storesales = sum(price_x, na.rm=T),
                   storeshare = storesales / allsales[1]) %>%
  dplyr::arrange(location, salesquarter)

retailers_byproducttype <- dispensing %>%
  dplyr::mutate(salesquarter = quarter(monthtime, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter, inv_type_name) %>%
  dplyr::mutate(allsales_type = sum(price_x, na.rm=T)) %>%
  dplyr::group_by(location, salesquarter, inv_type_name) %>%
  dplyr::summarise(storesales_type = sum(price_x, na.rm=T),
                   storeshare_type = storesales_type / allsales_type[1]) %>%
  dplyr::arrange(location, salesquarter, inv_type_name)

# use these categorize for market concentration
retailers_byproductGroup <- dispensing %>%
  dplyr::mutate(salesquarter = quarter(monthtime, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter, product_group) %>%
  dplyr::mutate(allsales_type = sum(price_x, na.rm=T)) %>%
  dplyr::group_by(location, salesquarter, product_group) %>%
  dplyr::summarise(storesales_type = sum(price_x, na.rm=T),
                   storeshare_type = storesales_type / allsales_type[1]) %>%
  dplyr::arrange(location, salesquarter, product_group)

write.table(retailers_allproducts, file="../data/Dec2016/cleaned/samples/retailers_sharebyquarter.csv",
            sep=",", row.names=FALSE)
write.table(retailers_byproducttype, file="../data/Dec2016/cleaned/samples/retailers_sharebytypebyquarter.csv",
            sep=",", row.names=FALSE)
write.table(retailers_byproductGroup, file="../data/Dec2016/cleaned/samples/retailers_sharebyGroupbyquarter.csv",
            sep=",", row.names=FALSE)

# cleaning for producers & processors -----
transfers <- readr::read_csv("../data/Dec2016/biotrackthc_inventorytransfers.csv")
transfers$inventoryid <- as.numeric(transfers$inventoryid)
transfers$parentid <- as.numeric(transfers$parentid)
transfers$inbound_license <- as.numeric(transfers$inbound_license)
transfers$outbound_license <- as.numeric(transfers$outbound_license)

# get selected variables and rename so they make sense in the joins later
trans.select <- transfers %>%
  dplyr::select(trans_id = id, trans_invid = inventoryid, trans_invtype = inventorytype,
                       trans_loc = location, trans_inloc = inbound_location, trans_parentid = parentid, 
                       trans_inlicense = inbound_license, trans_outlicense = outbound_license,
                       trans_saleprice = saleprice, trans_unitprice = unitprice) %>%
  dplyr::left_join(inventory.types, by=c("trans_invtype" = "inventorytype")) %>%
  dplyr::rename(trans_invtypename = inv_type_name) %>%
  # get labels for the transfer locations
  dplyr::left_join(select(locations, licensenum, typesimp), by=c("trans_outlicense" = "licensenum")) %>%
  dplyr::rename(trans_outloctype = typesimp) %>%
  # get labels for the inbound (retail) locations
  dplyr::left_join(select(locations, licensenum, typesimp), by=c("trans_inlicense" = "licensenum")) %>%
  dplyr::rename(trans_inloctype = typesimp) %>%
  # use inventoryid as proxy for transfer date, making assumption that it is around when it arrived
  # in new entity's inventory
  dplyr::left_join(select(inventory, inv_date, inventoryid), by=c("trans_invid" = "inventoryid"))

# categorize general groups
# for upstream
trans_producttypes <- as.data.frame(unique(trans.select$trans_invtypename))
colnames(trans_producttypes) <- "trans_invtypename"
# use categorization function to get grouping for each product type
trans_producttypes <- trans_producttypes %>%
  rowwise() %>%
  dplyr::mutate(product_group = groupAllProductTypes(trans_invtypename))
# join back to transfers df
trans.select$trans_invtypename <- as.factor(trans.select$trans_invtypename)
trans.select <- left_join(trans.select, trans_producttypes, by="trans_invtypename")



# calculate for processors -----
processors_allproducts <- trans.select %>%
  # filter to transfers from a processor, to a retailer
  dplyr::filter(trans_outloctype=="Processor", trans_inloctype=="Retailer") %>%
  dplyr::mutate(salesquarter = quarter(inv_date, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter) %>%
  # get sum of all sales for that quarter, across all processors
  dplyr::mutate(allsales = sum(trans_saleprice, na.rm=T)) %>%
  dplyr::group_by(trans_outlicense, salesquarter) %>%
  dplyr::summarise(processor_sales = sum(trans_saleprice, na.rm=T),
                   # use that sum of all sales to get market share
                   processor_share = processor_sales / allsales[1]) %>%
  dplyr::arrange(trans_outlicense, salesquarter)

processors_byproducttype <- trans.select %>%
  # filter to transfers from a processor, to a retailer
  dplyr::filter(trans_outloctype=="Processor", trans_inloctype=="Retailer") %>%
  dplyr::mutate(salesquarter = quarter(inv_date, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter, trans_invtypename) %>%
  # get sum of all sales for that quarter, that inventory type, across all processors
  dplyr::mutate(allsales = sum(trans_saleprice, na.rm=T)) %>%
  dplyr::group_by(trans_outlicense, salesquarter, trans_invtypename) %>%
  dplyr::summarise(processor_sales = sum(trans_saleprice, na.rm=T),
                   # use that sum of all sales to get market share
                   processor_share = processor_sales / allsales[1]) %>%
  dplyr::arrange(trans_outlicense, salesquarter, trans_invtypename)

# use these categorize for market concentration
processors_byproductGroup <- trans.select %>%
  # filter to transfers from a processor, to a retailer
  dplyr::filter(trans_outloctype=="Processor", trans_inloctype=="Retailer") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(salesquarter = quarter(inv_date, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter, product_group) %>%
  # get sum of all sales for that quarter, that inventory type, across all processors
  dplyr::mutate(allsales = sum(trans_saleprice, na.rm=T)) %>%
  dplyr::group_by(trans_outlicense, salesquarter, product_group) %>%
  dplyr::summarise(processor_sales = sum(trans_saleprice, na.rm=T),
                   # use that sum of all sales to get market share
                   processor_share = processor_sales / allsales[1]) %>%
  dplyr::arrange(trans_outlicense, salesquarter, product_group)

# write out
write.table(processors_allproducts, file="../data/Dec2016/cleaned/samples/processors_byquarter.csv",
            row.names = FALSE, sep=",")
write.table(processors_byproducttype, file="../data/Dec2016/cleaned/samples/processors_byquarterproducttype.csv",
            row.names = FALSE, sep=",")
write.table(processors_byproductGroup, file="../data/Dec2016/cleaned/samples/processors_byquarterproductGroup.csv",
            row.names = FALSE, sep=",")



# calculate for producer-processors -----
prodprocessor_allproducts <- trans.select %>%
  # filter to transfers from a Producer-Processor, to a retailer
  dplyr::filter(trans_outloctype=="Producer-Processor", trans_inloctype=="Retailer") %>%
  dplyr::mutate(salesquarter = quarter(inv_date, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter) %>%
  # get sum of all sales for that quarter, across all processors
  dplyr::mutate(allsales = sum(trans_saleprice, na.rm=T)) %>%
  dplyr::group_by(trans_outlicense, salesquarter) %>%
  dplyr::summarise(processor_sales = sum(trans_saleprice, na.rm=T),
                   # use that sum of all sales to get market share
                   processor_share = processor_sales / allsales[1]) %>%
  dplyr::arrange(trans_outlicense, salesquarter)

prodprocessor_byproducttype <- trans.select %>%
  # filter to transfers from a processor, to a retailer
  dplyr::filter(trans_outloctype=="Producer-Processor", trans_inloctype=="Retailer") %>%
  dplyr::mutate(salesquarter = quarter(inv_date, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter, trans_invtypename) %>%
  # get sum of all sales for that quarter, that inventory type, across all processors
  dplyr::mutate(allsales = sum(trans_saleprice, na.rm=T)) %>%
  dplyr::group_by(trans_outlicense, salesquarter, trans_invtypename) %>%
  dplyr::summarise(processor_sales = sum(trans_saleprice, na.rm=T),
                   # use that sum of all sales to get market share
                   processor_share = processor_sales / allsales[1]) %>%
  dplyr::arrange(trans_outlicense, salesquarter, trans_invtypename)

# use these categorize for market concentration
prodprocessor_byproductGroup <- trans.select %>%
  # filter to transfers from a processor, to a retailer
  dplyr::filter(trans_outloctype=="Producer-Processor", trans_inloctype=="Retailer") %>%
  dplyr::mutate(salesquarter = quarter(inv_date, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter, product_group) %>%
  # get sum of all sales for that quarter, that inventory type, across all processors
  dplyr::mutate(allsales = sum(trans_saleprice, na.rm=T)) %>%
  dplyr::group_by(trans_outlicense, salesquarter, product_group) %>%
  dplyr::summarise(processor_sales = sum(trans_saleprice, na.rm=T),
                   # use that sum of all sales to get market share
                   processor_share = processor_sales / allsales[1]) %>%
  dplyr::arrange(trans_outlicense, salesquarter, product_group)

# write out-----
write.table(prodprocessor_allproducts, file="../data/Dec2016/cleaned/samples/prod_processors_byquarter.csv",
            row.names = FALSE, sep=",")
write.table(prodprocessor_byproducttype, file="../data/Dec2016/cleaned/samples/prod_processors_byquarterproducttype.csv",
            row.names = FALSE, sep=",")
write.table(prodprocessor_byproductGroup, file="../data/Dec2016/cleaned/samples/prod_processors_byquarterproductGroup.csv",
            row.names = FALSE, sep=",")



# calculate for producers -----
producer_allproducts <- trans.select %>%
  # filter to transfers from a Producer, to a retailer
  dplyr::filter(trans_outloctype=="Producer", trans_inloctype=="Retailer") %>%
  dplyr::mutate(salesquarter = quarter(inv_date, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter) %>%
  # get sum of all sales for that quarter, across all processors
  dplyr::mutate(allsales = sum(trans_saleprice, na.rm=T)) %>%
  dplyr::group_by(trans_outlicense, salesquarter) %>%
  dplyr::summarise(processor_sales = sum(trans_saleprice, na.rm=T),
                   # use that sum of all sales to get market share
                   processor_share = processor_sales / allsales[1]) %>%
  dplyr::arrange(trans_outlicense, salesquarter)

producer_byproducttype <- trans.select %>%
  # filter to transfers from a Producer, to a retailer
  dplyr::filter(trans_outloctype=="Producer", trans_inloctype=="Retailer") %>%
  dplyr::mutate(salesquarter = quarter(inv_date, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter, trans_invtypename) %>%
  # get sum of all sales for that quarter, that inventory type, across all processors
  dplyr::mutate(allsales = sum(trans_saleprice, na.rm=T)) %>%
  dplyr::group_by(trans_outlicense, salesquarter, trans_invtypename) %>%
  dplyr::summarise(processor_sales = sum(trans_saleprice, na.rm=T),
                   # use that sum of all sales to get market share
                   processor_share = processor_sales / allsales[1]) %>%
  dplyr::arrange(trans_outlicense, salesquarter, trans_invtypename)

# use these categorize for market concentration
producer_byproductGroup <- trans.select %>%
  # filter to transfers from a Producer, to a retailer
  dplyr::filter(trans_outloctype=="Producer", trans_inloctype=="Retailer") %>%
  dplyr::mutate(salesquarter = quarter(inv_date, with_year=TRUE)) %>%
  dplyr::group_by(salesquarter, product_group) %>%
  # get sum of all sales for that quarter, that inventory type, across all processors
  dplyr::mutate(allsales = sum(trans_saleprice, na.rm=T)) %>%
  dplyr::group_by(trans_outlicense, salesquarter, product_group) %>%
  dplyr::summarise(processor_sales = sum(trans_saleprice, na.rm=T),
                   # use that sum of all sales to get market share
                   processor_share = processor_sales / allsales[1]) %>%
  dplyr::arrange(trans_outlicense, salesquarter, product_group)

# write out-----
write.table(producer_allproducts, file="../data/Dec2016/cleaned/samples/producer_byquarter.csv",
            row.names = FALSE, sep=",")
write.table(producer_byproducttype, file="../data/Dec2016/cleaned/samples/producer_byquarterproducttype.csv",
            row.names = FALSE, sep=",")
write.table(producer_byproductGroup, file="../data/Dec2016/cleaned/samples/producer_byquarterproductGroup.csv",
            row.names = FALSE, sep=",")
