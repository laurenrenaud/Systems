library(dplyr)
library(ggplot2)
library(lubridate)

inventory.types <- read.csv("../data/Dec2016/cleaned/inventory_type.csv", sep=",", header=T)
inventory.types <- inventory.types %>%
  rowwise() %>%
  mutate(`Product Type` = ifelse(inv_type_name=="Usable Marijuana", "Usable Flower",
                                 ifelse(inv_type_name=="Marijuana Extract for Inhalation", "Extracts",
                                        ifelse(inv_type_name=="Liquid Marijuana Infused Edible" | 
                                                 inv_type_name=="Liquid Marijuana Infused Edible", "Edibles",
                                               "Other"))))
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
dispensing$`Product Type` <- as.factor(dispensing$`Product Type`)
dispensing <- rename(dispensing, dispensingid = id)

add_commas = function(x) format(x, big.mark = ",")

# plot revenes over time by generalized product types
dispensing %>%
  dplyr::group_by(monthtime, `Product Type`) %>%
  dplyr::summarise(total_revenue = sum(price_x, na.rm=T)) %>%
  dplyr::mutate(`Product Type` = factor(`Product Type`, levels = c("Usable Flower", "Extracts", "Other", "Edibles"))) %>%
  ggplot(aes(x=monthtime, y=total_revenue, color=`Product Type`)) +
  #geom_point() +
  stat_smooth(method="loess") +
  scale_colour_brewer(palette="Dark2") +
  scale_y_continuous(labels=add_commas) +
  labs(title="Total Daily Revenue Over Time",
       x="Sale Date",
       y="Daily Sales")

week(dispensing$monthtime[999999])


tot_rev_agg <- dispensing %>%
  dplyr::group_by(monthtime, `Product Type`) %>%
  dplyr::summarise(total_revenue = sum(price_x, na.rm=T)) %>%
  dplyr::mutate(`Product Type` = factor(`Product Type`, levels = c("Usable Flower", "Extracts", "Other", "Edibles"))) 

# write.table(tot_rev_agg, file="../data/Dec2016/cleaned/samples/rev_bytype_byday.csv", sep=",", row.names = F)
# 
# dispensing$sale_month <- format(as.Date(dispensing$monthtime), "%Y-%m")
# dispensing$sale_month <- as.Date(dispensing$sale_month)

dispensing %>%
  dplyr::group_by(monthtime, `Product Type`) %>%
  dplyr::summarise(total_revenue = sum(price_x, na.rm=T)) %>%
  dplyr::mutate(`Product Type` = factor(`Product Type`, levels = c("Usable Flower", "Extracts", "Other", "Edibles"))) %>%
  ggplot(aes(x=monthtime, y=total_revenue, fill=`Product Type`)) +
  #geom_point() +
  geom_area() +
  #stat_smooth(method="loess") +
  scale_colour_brewer(palette="Dark2") +
  scale_y_continuous(labels=add_commas) +
  labs(title="Total Daily Revenue Over Time",
       x="Sale Date",
       y="Daily Sales")
