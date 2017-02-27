library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
options(scipen=4)

inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
locations <- readr::read_csv("../data/Dec2016/cleaned/locations/all_locations.csv")
loc.types <- readr::read_csv("../data/Dec2016/cleaned/locations/locationtypes.csv")
loc.simp <- locations %>%
  left_join(loc.types, by=c("locationtype" = "loctypeCode")) %>%
  select(typesimp, location_id, maxDate, months_open, status)

removereasons <- unique(inventory$removereason)
write(removereasons, file="../data/Dec2016/cleaned/testing/removereasons.txt")

# entities with removed product
removedproduct <- inventory %>%
  group_by(location) %>%
  summarise(numremoved = sum(!is.na(removereason)),
            allinv = n(),
            percremoved = numremoved / allinv) %>%
  mutate(marketsharerank = rank(allinv)/ length(allinv)) %>%
  filter(percremoved > 0) %>%
  arrange(desc(percremoved)) %>%
  left_join(loc.simp, by=c("location" = "location_id"))
write.table(removedproduct, file="../data/Dec2016/cleaned/samples/removed_product.csv", row.names=F, sep=",")

removedproduct$status <- factor(removedproduct$status, levels = c("Open", "Closed"))

table(removedproduct$status)

removedproduct %>%
  filter(!is.na(status)) %>%
  ggplot(aes(x=percremoved, color=status, fill=status)) +
  geom_density(alpha=0.45) +
  scale_fill_manual(values=brewer.pal(3,"Dark2")) +
  #scale_color_manual(values=brewer.pal(3,"Dark2")) +
  scale_color_manual(values=c("gray47", "gray47")) +
  ggtitle("Percent of Product Removed \nby Store Status") +
  ylab("Density") + xlab("Percent Removed") +
  theme(legend.justification=c(1,1), legend.position=c(1,1))


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
  left_join(loc.simp, by=c("location" = "location_id")) %>%
  mutate(status = ifelse(floor_date(maxDate, "month") < report_month, "Closed", "Open"))

table(noremovals$typesimp)
