library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
options(scipen=4)

inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
inv.type <- readr::read_csv("../data/Dec2016/cleaned/inventory_type.csv")
locations <- readr::read_csv("../data/Dec2016/cleaned/locations/all_locations.csv")
loc.types <- readr::read_csv("../data/Dec2016/cleaned/locations/locationtypes.csv")
loc.simp <- locations %>%
  left_join(loc.types, by=c("locationtype" = "loctypeCode")) %>%
  select(typesimp, location_id, maxDate, months_open, status)

removereasons <- unique(inventory$removereason)
write(removereasons, file="../data/Dec2016/cleaned/testing/removereasons.txt")

# all entities, not filtered to remove zeros
removedproduct <- inventory %>%
  group_by(location) %>%
  summarise(removed_total = sum(!is.na(removereason)),
            inv_total = n(),
            percremoved_total = removed_total / inv_total) %>%
  mutate(marketsharerank = rank(inv_total)/ length(inv_total)) %>%
  #filter(percremoved > 0) %>%
  arrange(desc(percremoved_total)) %>%
  left_join(loc.simp, by=c("location" = "location_id"))
#write.table(removedproduct, file="../data/Dec2016/cleaned/samples/removed_product.csv", row.names=F, sep=",")

removedproduct %>%
  filter(!is.na(status), status != "PENDING (ISSUED)") %>%
  ggplot(aes(x=percremoved_total, color=status, fill=status)) +
  geom_density(alpha=0.45) +
  scale_fill_manual(values=brewer.pal(3,"Dark2")) +
  #scale_color_manual(values=brewer.pal(3,"Dark2")) +
  scale_color_manual(values=c("gray47", "gray47")) +
  ggtitle("Percent of Product Removed \nby Store Status") +
  ylab("Density") + xlab("Percent Removed") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

removedproduct$status <- factor(removedproduct$status, levels = c("Open", "Closed"))

# location + inventory type removed

removedproduct.bytype <- inventory %>%
  group_by(location, inventorytype) %>%
  summarise(
    removed_type = sum(!is.na(removereason))
    #count_type = n(),
    #percremoved_type = sum(!is.na(removereason)) / n()
  ) %>%
  left_join(inv.type, by="inventorytype") %>%
  select(-(inventorytype)) %>%
  left_join(removedproduct, by="location") %>% 
  mutate(percremove_type = round(removed_type / removed_total, 2)) %>%
  select(location, percremove_type, inv_type_name) %>%
  tidyr::spread(inv_type_name, percremove_type) %>%
  left_join(removedproduct, by="location") %>% 
  arrange(desc(percremoved_total))
  
# reorder columns, put listed columns first
refcols <- c("location", "percremoved_total", "marketsharerank", "typesimp", "status", "months_open")
removedproduct.bytype <- removedproduct.bytype[, c(refcols, setdiff(names(removedproduct.bytype), refcols))]

sum(!is.na(inventory$removereason)) / nrow(inventory)

# percent of removed inventory that is of given type
topremoved_types <- inventory %>%
  group_by(inventorytype) %>%
  summarise(
    removed_type = sum(!is.na(removereason)),
    removed_perc = round(removed_type / sum(!is.na(inventory$removereason))*100, 2)
  ) %>%
  left_join(inv.type, by="inventorytype") %>%
  arrange(desc(removed_perc))

# facet wrap
inventory %>%
  group_by(inventorytype) %>%
  summarise(
    removed_type = sum(!is.na(removereason)),
    removed_perc = removed_type / sum(!is.na(inventory$removereason))
  ) %>%
  left_join(inv.type, by="inventorytype") %>%
  filter(removed_perc > 0.01) %>%
  ggplot(aes(x=reorder(inv_type_name, desc(removed_perc)), y=removed_perc)) +
  geom_bar(stat="identity", fill="darkgreen") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Precent of Removed Product",
       x="Inventory Type",
       y="Percent of Removed Product")

## without waste
inventory %>%
  group_by(inventorytype) %>%
  summarise(
    removed_type = sum(!is.na(removereason)),
    removed_perc = removed_type / sum(!is.na(inventory$removereason))
  ) %>%
  left_join(inv.type, by="inventorytype") %>%
  filter(removed_perc > 0.01, inv_type_name != "Waste") %>%
  ggplot(aes(x=reorder(inv_type_name, desc(removed_perc)), y=removed_perc)) +
  geom_bar(stat="identity", fill="darkgreen") +
  scale_y_continuous(labels = scales::percent) +
  labs(title="Precent of Removed Product",
       x="Inventory Type",
       y="Percent of Removed Product")


# calculation of removed product, ignoring "waste"
removedNoWaste.bytype <- inventory %>%
  # ignore waste
  filter(inventorytype != 27) %>%
  group_by(location, inventorytype) %>%
  summarise(
    removed_type = sum(!is.na(removereason))
    #count_type = n(),
    #percremoved_type = sum(!is.na(removereason)) / n()
  ) %>%
  left_join(inv.type, by="inventorytype") %>%
  select(-(inventorytype)) %>%
  left_join(removedproduct, by="location") %>% 
  mutate(percremove_type = round(removed_type / removed_total, 2)) %>%
  select(location, percremove_type, inv_type_name) %>%
  tidyr::spread(inv_type_name, percremove_type) %>%
  left_join(removedproduct, by="location") %>% 
  arrange(desc(percremoved_total))
# reorder columns, put listed columns first
refcols <- c("location", "percremoved_total", "marketsharerank", "typesimp", "status", "months_open")
removedNoWaste.bytype <- removedNoWaste.bytype[, c(refcols, setdiff(names(removedNoWaste.bytype), refcols))]


table(removedproduct$typesimp[removedproduct$percremoved_total > 0])




table(removedproduct$typesimp)

removedproduct %>%
  group_by(typesimp) %>%
  summarise(perc = n() / nrow(locations) *100)


# entities with no removed product
noremovals <- inventory %>%
  group_by(location) %>%
  summarise(numremoved = sum(!is.na(removereason)),
            allinv = n(),
            percremoved = numremoved / allinv) %>%
  mutate(marketsharerank = rank(allinv)/ length(allinv)) %>%
  filter(percremoved == 0) %>%
  select(location, allinv, marketsharerank) %>%
  arrange(desc(marketsharerank)) %>%
  left_join(loc.simp, by=c("location" = "location_id"))

table(noremovals$typesimp)

noremovals %>%
  group_by(typesimp) %>%
  summarise(perc = n() / nrow(locations)*100)
