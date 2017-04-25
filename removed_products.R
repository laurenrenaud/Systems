library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(topicmodels)
options(scipen=4)

# pulling in tables
inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
inv.type <- readr::read_csv("../data/Dec2016/cleaned/inventory_type.csv")
locations <- readr::read_csv("../data/Dec2016/cleaned/locations/all_locations.csv")
loc.types <- readr::read_csv("../data/Dec2016/cleaned/locations/locationtypes.csv")
loc.simp <- locations %>%
  dplyr::select(typesimp, location_id, maxDate, months_open, status)

# get list of all reasons given so we can look at these for
# text mining later
removereasons <- unique(inventory$removereason)
#write(removereasons, file="../data/Dec2016/cleaned/testing/removereasons.txt")
# same specifically for this one store
removereasons_268 <- unique(inventory$removereason[inventory$location==268])
#write(removereasons, file="../data/Dec2016/cleaned/testing/removereasons_268.txt")

# count of removed items
sum(!is.na(inventory$removereason)) / nrow
# weight of removed items
inventory$usableweight <- as.numeric(inventory$usableweight)
sum(inventory$usableweight[!is.na(inventory$removereason)], na.rm=T)

# gets summary for all entities, not filtered to remove zeros
removedproduct <- inventory %>%
  # one row per location
  dplyr::group_by(location) %>%
  # get total count of line items removed
  dplyr::summarise(removed_total = sum(!is.na(removereason)),
            # total count of inventory lines
            inv_total = n(),
            # percent of removed lines over total lines
            percremoved_total = removed_total / inv_total) %>%
  # get percentile rank using total inventory to rank
  dplyr::mutate(marketsharerank = rank(inv_total)/ length(inv_total)) %>%
  #filter(percremoved > 0) %>%
  dplyr::left_join(loc.simp, by=c("location" = "location_id")) %>%
  dplyr::arrange(status, desc(percremoved_total))
#write.table(removedproduct, file="../data/Dec2016/cleaned/samples/removed_product.csv", row.names=F, sep=",")

# plot histogram/density of % of product removed by open/clsoed status
removedproduct %>%
  # keep only those that are open or closed
  # this data has one that's pending and 2 that are NA
  filter(!is.na(status), status != "PENDING (ISSUED)") %>%
  rename(`Location Status` = status) %>%
  ggplot(aes(x=percremoved_total, color=`Location Status`, fill=`Location Status`)) +
  geom_density(alpha=0.45) +
  # fill colors
  scale_fill_manual(values=brewer.pal(3,"Dark2")) +
  # outline colors
  scale_color_manual(values=c("gray47", "gray47")) +
  labs(
    title="Percent of Product Removed \nby Location Status",
    x="Percent Removed",
    y="Density"
  ) +
  theme(legend.justification=c(1,1), legend.position=c(1,1))

# re-order the levels
removedproduct$status <- factor(removedproduct$status, levels = c("Open", "Closed"))

# get combination of location + inventory type removed
removedproduct.bytype <- inventory %>%
  # group by location and inventory type to get one line for the combination of each
  group_by(location, inventorytype) %>%
  # get the total count removed for that inventory type for that location
  summarise(
    removed_type = sum(!is.na(removereason))
    #count_type = n(),
    #percremoved_type = sum(!is.na(removereason)) / n()
  ) %>%
  # join to get list of inventory types names
  left_join(inv.type, by="inventorytype") %>%
  select(-(inventorytype)) %>%
  # joing back to removed product -- this is connecting this list of 
  # count of number removed for each inventory type name for that location
  left_join(removedproduct, by="location") %>% 
  # now use that removed by type to get percent of that type removed
  mutate(percremove_type = round(removed_type / removed_total, 2)) %>%
  select(location, percremove_type, inv_type_name) %>%
  # rearrange so that each location has one line, with multiple columns for 
  # each inventory type
  tidyr::spread(inv_type_name, percremove_type) %>%
  left_join(removedproduct, by="location") %>% 
  arrange(desc(percremoved_total))
  
# reorder columns, put listed columns first
refcols <- c("location", "percremoved_total", "marketsharerank", "typesimp", "status", "months_open")
removedproduct.bytype <- removedproduct.bytype[, c(refcols, setdiff(names(removedproduct.bytype), refcols))]

# count of how many removed reasons are in inventory (are not NA)
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

# percent of removed for each product type
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
  theme(axis.text.x = element_text(angle = 30, hjust = .9)) +
  labs(title="Precent of Removed Product",
       x="Inventory Type",
       y="Percent of Removed Product")

## same as above but excluding waste
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
  theme(axis.text.x = element_text(angle = 30, hjust = .9)) +
  labs(title="Precent of Removed Product excluding 'Waste'",
       x="Inventory Type",
       y="Percent of Removed Product")


# calculation of removed product, ignoring "waste"
removedNoWaste.bytype <- inventory %>%
  # ignore waste
  filter(inventorytype != 27) %>%
  # same methodology as above to get percentages by type
  # but excluding waste
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

# location types that have removed product
table(removedproduct$typesimp[removedproduct$percremoved_total > 0])
# all location types count
table(removedproduct$typesimp)

# what percent of removed product is coming from each location type
removedproduct %>%
  filter(percremoved_total > 0) %>%
  mutate(allremoved = n()) %>%
  group_by(typesimp) %>%
  summarise(perc = n() / allremoved[1] *100)


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

# location types with no removed product
table(noremovals$typesimp)
# what percent of places with no removed product is coming from each location type
removedproduct %>%
  filter(percremoved_total == 0) %>%
  mutate(allremoved = n()) %>%
  group_by(typesimp) %>%
  summarise(perc = n() / allremoved[1] *100)


# attemping LDA : Latent Dirichlet allocation
# http://tidytextmining.com/topicmodeling.html
# http://tidytextmining.com/tidytext.html

# 1: convert strings into dataframe
# and need to include which "line" it comes from
removed.df <- data_frame(line=1:36466, text = removereasons)
# 2: use tidytext to convert into one-token-per-document-per-row.
library(tidytext)
library(stringr)
# this then includes a variable saying which line it initially came from
# removed.df <- removed.df %>%
#   unnest_tokens(word, text)

inv_reasons <- inventory %>%
  # keep only those with removed reasons and not categorized as "waste"
  dplyr::filter(!is.na(removereason), inventorytype != 27) %>%
  dplyr::left_join(loc.simp, by = c("location" = "location_id")) %>%
  dplyr::left_join(inv.type, by="inventorytype") %>%
  dplyr::select(inv_type_name, location, removereason, status) %>%
  #group_by(inv_type_name) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()

removed.tidy <- inv_reasons %>%
  unnest_tokens(word, removereason)

# remove stop words (but should check those)
data(stop_words)
removed.tidy <- removed.tidy %>%
  anti_join(stop_words)

# create DocumentTermMatrix
removed.docmatrix <- removed.tidy %>%
  select(document = linenumber, term = word) %>%
  group_by(document, term) %>%
  summarise(count = n()) %>%
  cast_dtm(document, term, count)

ap_lda <- LDA(removed.docmatrix, k = 3, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents %>%
  filter(topic==2) %>%
  arrange(desc(gamma))
