library(dplyr)
library(readr)
library(ggplot2)
options(scipen=20)

# pull in & clean dfs ---------
# locatons
locations <- read.csv("../data/Dec2016/cleaned/locations/all_locations.csv", sep=",", header=T)
locations.name.city <- select(locations, retailname = name, licensenum, location_id, city)
retail.loc <- filter(locations, retail==1)
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
inventory.retail <- filter(inventory, location %in% retail.loc$location_id)
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


# Connect to dispensing to inventory to producers inventory
locations <- readr::read_csv("../data/Dec2016/cleaned/locations/all_locations.csv")
loc.simp <- select(locations, location_id, name, typesimp)
transfers <- readr::read_csv("../data/Dec2016/biotrackthc_inventorytransfers.csv")
transfers$inventoryid <- as.numeric(transfers$inventoryid)
transfers$parentid <- as.numeric(transfers$parentid)
# get selected variables and rename so they make sense in the joins later
trans.select <- select(transfers, trans_id = id, trans_invid = inventoryid, trans_invtype = inventorytype,
                       trans_loc = location, trans_inloc = inbound_location, trans_weight = weight,
                       trans_usableweight = usableweight,
                       trans_strain = strain, trans_parentid = parentid, trans_saleprice = saleprice, 
                       trans_unitprice = unitprice, trans_descr = description)
trans.select <- left_join(trans.select, inventory.types, by=c("trans_invtype" = "inventorytype"))
trans.select <- rename(trans.select, trans_invtypename = inv_type_name)

trans.select <- trans.select %>%
  # get labels and cities for the transfer locations
  left_join(loc.simp, by=c("trans_loc" = "location_id")) %>%
  rename(trans_locname = name, trans_loctype = typesimp) %>%
  # get labels and cities for the inbound (retail) locations
  left_join(loc.simp, by=c("trans_inloc" = "location_id")) %>%
  rename(trans_inlocname = name, trans_inloctype = typesimp)


inventory$parentid <- as.numeric(inventory$parentid)

# come back and use transfers in order to get price the
# retailer paid
# also need to figure out how units / weights work in here


retail <- dispensing %>%
  # first going back to retailer's inventory
  dplyr::left_join(inventory, by="inventoryid") %>%
  # then getting potency
  #dplyr::left_join(potency_tidy, by="inventoryparentid") %>%
  # renaming and dropping variables
  dplyr::select(dispensingid, retail_loc = location.x, retail_price = price, retail_price_x = price_x,
                retail_usableweight = usableweight.x, retail_prodname = productname, retail_strain = strain,
                retail_typename = inv_type_name.x, retail_inventoryid = inventoryid,
                retail_parentid = parentid, retail_weight = weight.x, saledate = monthtime,
                retail_transactionid = transactionid.x, retail_deleted = deleted.x, refunded, 
                retail_invtype = inventorytype.x, retail_invdate = inv_date)
# same number of entries in retail than in dispensing at this point

retail <- retail %>%
  # details on retailer's location
  dplyr::left_join(loc.simp, by=c("retail_loc" = "location_id")) %>%
  dplyr::rename(retail_name = name) %>%
  dplyr::select(-(typesimp))
# same number of entries in retail than in dispensing at this point

# there were some (3.5%) of dispensing IDs that came up duplicate on this join
# it appears they have multiple transferIDs that are causing the problem
# need to look into it further, but dropping them for now
#retail.id.check <- select(retail, dispensingid, retail_inventoryid)
#trans.id.check <- select(trans.select, trans_id, trans_invid)
dupe.disID <- retail %>%
  select(dispensingid, retail_inventoryid) %>%
  left_join(select(trans.select, trans_id, trans_invid), by=c("retail_inventoryid" = "trans_invid")) %>%
  group_by(dispensingid) %>%
  summarise(count = n()) %>%
  filter(count > 1) #%>%
#arrange(desc(count)) %>%
#left_join(retail, by="dispensingid")

# join to transfers to get producer prices
retail <- retail %>%
  # filter out the duplicate dispening ids
  filter(!dispensingid %in% dupe.disID$dispensingid) %>%
  # pull in transfers to get producer to retail prices (and locations)
  dplyr::left_join(trans.select, by=c("retail_inventoryid" = "trans_invid"))


# converting variable types
retail$trans_usableweight <- as.numeric(retail$trans_usableweight)

# correcting for useable weight & weight
# from Imane:
# To get wholesale price per gram:
# 1. if trans_usableweight != NA, divide trans_saleprice by Trans_weight then by trans_usable weight
# 2. else, divide trans_saleprice by trans_weight then retail_usableweight then retail_weight
# 
# To get retail price per gram:
#   divide retail_price_x by retail_usableweight

# sampling
retail.sample <- retail %>%
  # removing some values before sampling
  dplyr::filter(!is.na(trans_saleprice), trans_saleprice > 0.1, trans_saleprice < 2000, 
                retail_price_x > 0.1, retail_price_x < 150,
                retail_typename!="Marijuana Infused Topicals", retail_typename!="Marijuana Mix Package",
                retail_typename!="Marijuana Mix Infused", retail_typename!="Capsule",
                retail_typename!="Suppository", retail_typename!="Tincture", !is.na(retail_typename))

retail.list <- sample(retail.sample$retail_transactionid, 500000, replace=F)
retail.sample <- dplyr::filter(retail.sample, retail_transactionid %in% retail.list)
retail.sample <- retail.sample %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    trans_pricepergram = ifelse(!is.na(trans_usableweight),
                                # if transfer usable_weight is not NA
                                # then divide transfer sale price by transfer weight (which is quantity)
                                # and then divide that by the transfer usable weight (which is the weight per item)
                                trans_saleprice / trans_weight / trans_usableweight,
                                # if transfer usable_weight is NA
                                # transfer sale price, divided by trans_weight (quantity sold to retailer)
                                # to get price per single item
                                # but we don't know if one item in transfers and retail mean the same thing
                                # if retailer sells a four pack -- was that counted as weight of 1 in transfers, or weight of 4?
                                # retail usable_weight already sums up total weight for given number of items packaged together
                                # so we need to divide retail usableweight by retail weight to get the usable weight of single itme
                                # then we can divide the transfer price per one item (trans_saleprice / trans_weight)
                                # by the retail weight of one item (retail_usableweight / retail_weight)
                                # then divide by the (retail usable_weight / retail_weight)
                                (trans_saleprice / trans_weight) / (retail_usableweight / retail_weight)),
    retail_pricepergram = retail_price_x / retail_usableweight
  )

# getting quarters. First running on just dates, then joining because too big otherwise
retaildates <- as.data.frame(unique(retail.sample$saledate))
colnames(retaildates) <- "dates"
retaildates <- retaildates %>%
  dplyr::mutate(
    quarter = ifelse(dates <= "2014-03-31", '2014 Q1',
                     ifelse(dates <= "2014-06-30", '2014 Q2', 
                            ifelse(dates <= "2014-09-30", '2014 Q3',
                                   ifelse(dates <= "2014-12-31", '2014 Q4', 
                                          ifelse(dates <= "2015-03-31", '2015 Q1', 
                                                 ifelse(dates <= "2015-06-30", '2015 Q2', 
                                                        ifelse(dates <= "2015-09-30", '2015 Q3', 
                                                               ifelse(dates <= "2015-12-31", '2015 Q4', 
                                                                      ifelse(dates <= "2016-03-31", '2016 Q1', 
                                                                             ifelse(dates <= "2016-06-30", '2016 Q2', 
                                                                                    ifelse(dates <= "2016-09-30", '2016 Q3', 
                                                                                           ifelse(dates <= "2016-12-31", '2016 Q4', 
                                                                                                  'other'))))))))))))
  )

retail.sample <- left_join(retail.sample, retaildates, by=c("saledate" = "dates"))

# first get list/df of inhalantnames
inhalantnames <- as.data.frame(unique(retail.sample$retail_prodname[retail.sample$retail_typename=="Marijuana Extract for Inhalation"]))

# rename column so we can call it
colnames(inhalantnames) <- "retail_prodname"

# bringing in classification function
source("categorization_function.R")
inhalantnames <- inhalantnames %>%
  rowwise() %>%
  mutate(inhalant_type = categorizeNames(retail_prodname),
         inhalant_gen = groupProductTypesOilSep(inhalant_type))

# join classified inhalantnames back to dispening df
retail.sample$retail_prodname <- as.factor(retail.sample$retail_prodname)
retail.sample <- left_join(retail.sample, inhalantnames, by="retail_prodname")

# sample for checking
#samplelist <- sample(retail.sample$retail_transactionid, 20000, replace=F)
#excel.sample <- dplyr::filter(retail, retail_transactionid %in% samplelist)
# write.table(excel.sample, file="../data/Dec2016/cleaned/testing/add_multi_sample.csv",
#             sep=",", row.names = F)

# calculating avg mark ups by quarter -----
retail.byquarter.all <- retail.sample %>%
  dplyr::group_by(quarter) %>%
  dplyr::summarise(avg_retailpricecpergram = median(retail_pricepergram, na.rm=T),
                   avg_wholesalepricepergram = median(trans_pricepergram, na.rm=T)
                   )

retail.byquarter.bytype <- retail.sample %>%
  dplyr::group_by(quarter, retail_typename) %>%
  dplyr::summarise(avg_retailpricecpergram = median(retail_pricepergram, na.rm=T),
                   avg_wholesalepricepergram = median(trans_pricepergram, na.rm=T)
  )

retail.byquarter.cartridge <- retail.sample %>%
  dplyr::filter(retail_typename == "Marijuana Extract for Inhalation") %>%
  dplyr::group_by(quarter, cartridge) %>%
  dplyr::summarise(avg_retailpricecpergram = median(retail_pricepergram, na.rm=T),
                   avg_wholesalepricepergram = median(trans_pricepergram, na.rm=T)
  )

retail.byquarter.extractmethod <- retail.sample %>%
  dplyr::filter(retail_typename == "Marijuana Extract for Inhalation") %>%
  dplyr::group_by(quarter, method) %>%
  dplyr::summarise(avg_retailpricecpergram = median(retail_pricepergram, na.rm=T),
                   avg_wholesalepricepergram = median(trans_pricepergram, na.rm=T)
  )

retail.byquarter.inhaltype <- retail.sample %>%
  dplyr::filter(retail_typename == "Marijuana Extract for Inhalation") %>%
  dplyr::group_by(quarter, inhalant_type) %>%
  dplyr::summarise(avg_retailpricecpergram = median(retail_pricepergram, na.rm=T),
                   avg_wholesalepricepergram = median(trans_pricepergram, na.rm=T)
  )

retail.byquarter.inhalGen <- retail.sample %>%
  dplyr::filter(retail_typename == "Marijuana Extract for Inhalation") %>%
  dplyr::group_by(quarter, inhalant_gen) %>%
  dplyr::summarise(avg_retailpricecpergram = median(retail_pricepergram, na.rm=T),
                   avg_wholesalepricepergram = median(trans_pricepergram, na.rm=T)
  )

# plotting mark ups by quarter ------
retail.byquarter.all %>%
  dplyr::filter(quarter!="other") %>%
  ggplot(aes(x=avg_wholesalepricepergram, y=avg_retailpricecpergram)) +
  #geom_point(alpha=0.25, color="darkgreen") + 
  geom_point(aes(color=quarter), alpha=.75, size=3) + 
  #geom_smooth(color="gold2", method = "lm") +
  #facet_wrap("retail_typename") +
  scale_colour_brewer(palette = "Reds") +
  xlim(0, 8) + ylim(0, 25) +
  geom_abline(intercept = 0, slope = 1, color="darkorchid4", size=0.75, linetype="dashed") +
  geom_abline(intercept = 0, slope = 3, color="darkorchid4", size=1, linetype="dotted") +
  labs(title="Relationship Between Processor and Retail Prices \nUsable & Inhalants",
       x="Processor's Price Per Gram",
       y="Retail Price Per Gram") + 
  theme(panel.background = element_rect(fill = "darkgray"), panel.grid.major = element_line(colour = "azure2"))

retail.byquarter.bytype %>%
  dplyr::filter(quarter!="other") %>%
  dplyr::filter(retail_typename == "Usable Marijuana") %>%
  ggplot(aes(x=avg_wholesalepricepergram, y=avg_retailpricecpergram)) +
  xlim(0, 8) + ylim(0, 25) +
  geom_abline(intercept = 0, slope = 1, color="grey23", size=0.75, linetype="dashed") +
  geom_abline(intercept = 0, slope = 3, color="grey23", size=1, linetype="dotted") +
  geom_point(aes(color=quarter), alpha=.9, size=3) + 
  scale_colour_brewer(palette = "Reds") +
  annotate("text", x = 6, y = 3.5, label = "1:1 Ratio", colour="black") + 
  annotate("text", x = 5, y = 19, label = "3:1 Ratio", colour="black") + 
  labs(title="Relationship Between Processor and Retail Prices \nUsable Marijuana",
       x="Processor's Price Per Gram",
       y="Retail Price Per Gram") + 
  theme(panel.background = element_rect(fill = "darkgray"), panel.grid.major = element_line(colour = "azure2"))

retail.byquarter.bytype %>%
  dplyr::filter(retail_typename == "Marijuana Extract for Inhalation",
                !is.na(avg_retailpricecpergram), !is.na(avg_wholesalepricepergram), quarter!="other") %>%
  ggplot(aes(x=avg_wholesalepricepergram, y=avg_retailpricecpergram)) +
  #geom_point(alpha=0.25, color="darkgreen") + 
  xlim(0, 25) + ylim(0, 65) +
  geom_abline(intercept = 0, slope = 1, color="grey23", size=0.75, linetype="dashed") +
  geom_abline(intercept = 0, slope = 3, color="grey23", size=1, linetype="dotted") +
  #geom_smooth(color="gold2", method = "lm") +
  annotate("text", x = 20, y = 15, label = "1:1 Ratio", colour="black") + 
  annotate("text", x = 14, y = 55, label = "3:1 Ratio", colour="black") + 
  geom_point(aes(color=quarter), alpha=.9, size=3) + 
  #facet_wrap("retail_typename") +
  scale_colour_brewer(palette = "Reds") +
  labs(title="Relationship Between Processor and Retail Prices \nExtract for Inhalation",
       x="Processor's Price Per Gram",
       y="Retail Price Per Gram") + 
  theme(panel.background = element_rect(fill = "darkgray"), panel.grid.major = element_line(colour = "azure2"))


# inhalants by classifications
retail.byquarter.cartridge %>%
  dplyr::filter(!is.na(avg_retailpricecpergram), !is.na(avg_wholesalepricepergram), quarter!="other") %>%
  ggplot(aes(x=avg_wholesalepricepergram, y=avg_retailpricecpergram)) +
  #geom_point(alpha=0.25, color="darkgreen") + 
  geom_smooth(color="gold2", method = "lm") +
  geom_point(aes(color=quarter), alpha=.9, size=3) + 
  #facet_wrap("retail_typename") +
  scale_colour_brewer(palette = "Reds") +
  facet_wrap("cartridge") +
  xlim(0, 40) + ylim(0, 105) +
  labs(title="Relationship Between Processor and Retail Prices \nExtract for Inhalation, by Cartridge",
       x="Processor's Price Per Gram",
       y="Retail Price Per Gram") + 
  theme(panel.background = element_rect(fill = "darkgray"), panel.grid.major = element_line(colour = "azure2"))

retail.byquarter.extractmethod %>%
  dplyr::filter(!is.na(avg_retailpricecpergram), !is.na(avg_wholesalepricepergram), quarter!="other") %>%
  ggplot(aes(x=avg_wholesalepricepergram, y=avg_retailpricecpergram)) +
  #geom_point(alpha=0.25, color="darkgreen") + 
  geom_smooth(color="gold2", method = "lm") +
  geom_point(aes(color=quarter), alpha=.9, size=3) + 
  scale_colour_brewer(palette = "Reds") +
  facet_wrap("method") +
  xlim(0, 40) + ylim(0, 105) +
  labs(title="Relationship Between Processor and Retail Prices \nExtract for Inhalation, by Extraction Method",
       x="Processor's Price Per Gram",
       y="Retail Price Per Gram") + 
  theme(panel.background = element_rect(fill = "darkgray"), panel.grid.major = element_line(colour = "azure2"))


retail.byquarter.inhaltype %>%
  dplyr::filter(!is.na(avg_retailpricecpergram), !is.na(avg_wholesalepricepergram), quarter!="other") %>%
  ggplot(aes(x=avg_wholesalepricepergram, y=avg_retailpricecpergram)) +
  #geom_point(alpha=0.25, color="darkgreen") + 
  geom_smooth(color="gold2", method = "lm") +
  geom_point(aes(color=quarter), alpha=.9, size=3) + 
  scale_colour_brewer(palette = "Reds") +
  facet_wrap("inhalant_type") +
  xlim(0, 35) + ylim(0, 100) +
  geom_abline(intercept = 0, slope = 1, color="darkorchid4", size=0.75, linetype="dashed") +
  geom_abline(intercept = 0, slope = 3, color="darkorchid4", size=1, linetype="dotted") +
  labs(title="Relationship Between Processor and Retail Prices \nExtract for Inhalation, by Product Type",
       x="Processor's Price Per Gram",
       y="Retail Price Per Gram") + 
  theme(panel.background = element_rect(fill = "darkgray"), panel.grid.major = element_line(colour = "azure2"))


retail.byquarter.inhalGen %>%
  dplyr::filter(!is.na(avg_retailpricecpergram), !is.na(avg_wholesalepricepergram), 
                # dropping 2015 Q2 because of high skew
                quarter!="other", quarter!="2015 Q2") %>%
  dplyr::mutate(inhalant_gen = factor(inhalant_gen, levels = c("Cartridge", "Oil", "Wax/Shatter/Dab/Resin", 
                                               "Hash/Kief", "Uncategorized"))) %>%
  ggplot(aes(x=avg_wholesalepricepergram, y=avg_retailpricecpergram)) +
  geom_abline(intercept = 0, slope = 1, color="gray23", size=1, linetype="dashed") +
  geom_abline(intercept = 0, slope = 3, color="gray23", size=1, linetype="dotted") +
  annotate("text", x = 30, y = 15, label = "1:1 Ratio", colour="black") + 
  annotate("text", x = 18, y = 80, label = "3:1 Ratio", colour="black") + 
  #geom_smooth(color="gold2", method = "lm") +
  geom_point(aes(color=quarter), alpha=.9, size=3) + 
  scale_colour_brewer(palette = "Reds") +
  facet_wrap("inhalant_gen") +
  xlim(0, 35) + ylim(0, 100) +
  labs(title="Relationship Between Processor and Retail Prices \nExtract for Inhalation, by Inhalant Category",
       x="Processor's Price Per Gram",
       y="Retail Price Per Gram") + 
  theme(panel.background = element_rect(fill = "darkgray"), panel.grid.major = element_line(colour = "azure2"),
        legend.justification=c(1,0), legend.position=c(1,0))


# plotting mark ups colored by quarter ------
excel.sample %>%
  dplyr::filter(retail_typename == "Usable Marijuana", quarter!="other") %>%
  ggplot(aes(x=trans_pricepergram, y=retail_pricepergram)) +
  #geom_point(alpha=0.25, color="darkgreen") + 
  geom_point(aes(color=quarter), alpha=0.25) + 
  geom_smooth(color="gold2", method = "lm") +
  scale_colour_brewer(palette = "YlGnBu") +
  #facet_wrap("retail_typename") +
  xlim(0, 30) + ylim(0, 30) +
  labs(title="Relationship Between Processor and Retail Prices \nUsable Marijuana",
       x="Processor's Price Per Gram",
       y="Retail Price Per Gram")

excel.sample %>%
  dplyr::filter(retail_typename == "Marijuana Extract for Inhalation", quarter!="other") %>%
  ggplot(aes(x=trans_pricepergram, y=retail_pricepergram)) +
  #geom_point(alpha=0.25, color="darkgreen") + 
  geom_point(aes(color=quarter), alpha=0.25) + 
  geom_smooth(color="gold2", method = "lm") +
  scale_colour_brewer(palette = "YlGnBu") +
  #facet_wrap("retail_typename") +
  xlim(0, 30) + ylim(0, 30) +
  labs(title="Relationship Between Processor and Retail Prices \nUsable Marijuana",
       x="Processor's Price Per Gram",
       y="Retail Price Per Gram")

