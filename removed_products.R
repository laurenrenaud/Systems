library(readr)
library(dplyr)
library(lubridate)
options(scipen=4)

inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
locations <- readr::read_csv("../data/Dec2016/cleaned/locations/all_locations.csv")
loc.types <- readr::read_csv("../data/Dec2016/cleaned/locations/locationtypes.csv")
loc.simp <- locations %>%
  left_join(loc.types, by=c("locationtype" = "loctypeCode")) %>%
  select(typesimp, location_id, maxDate, months_open)

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
  left_join(loc.simp, by=c("location" = "location_id")) %>%
  # get status, if the month of the store's max date is less than
  # the month of the overall max date (aka report month) then assume closed
  mutate(status = ifelse(floor_date(maxDate, "month") < max(floor_date(locations$maxDate, "month"), na.rm=T),
                         "Closed", "Open"))

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
