library(dplyr)
library(ggplot2)
library(readr)

# how to build extracts file ----------
# dispensing <- readr::read_csv("../data/Dec2016/biotrackthc_dispensing.csv")
# dispensing$saledate <- as.Date(as.POSIXct(dispensing$sessiontime,
#                                          origin = "1970-01-01", tz="America/Los_Angeles"))
# inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
# 
# extracts <- dispensing %>%
#   # 24 is inventory type for extracts
#   dplyr::filter(inventorytype==24) %>%
#   dplyr::select(dispensingid = id, weight, inventoryid, price, usableweight, saledate) %>%
#   dplyr::left_join(select(inventory, inventoryid = id, strain, productname,
#                           inventoryparentid, sample_id),
#                    by="inventoryid") %>%
#   dplyr::mutate(price_x = ifelse(saledate >= "2015-07-01",
#                                  price*1.37,
#                                  price),
#                 price_per_gram = price_x/usableweight) %>%
#   dplyr::select(-(saledate), -(price))
# 
# write.table(extracts, file="../data/Dec2016/cleaned/samples/extracts.csv", sep=",", row.names=F)

# pull in data -------
extracts <- readr::read_csv("../data/Dec2016/cleaned/samples/extracts.csv")
extracts$inventoryparentid <- as.numeric(extracts$inventoryparentid)
potency <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_potency_analysis.csv")
micro <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_micro_screening.csv")
moisture <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_moisture_content.csv")
solvent <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_solvent_screening.csv")
inv_types <- readr::read_csv("../data/Dec2016/cleaned/inventory_type.csv")
# combine the non-potency tests
tests <- rbind(micro, moisture, solvent)
labkey <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_samples.csv")
labkey$inventoryid <- as.numeric(labkey$inventoryid)

potency <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_potency_analysis.csv")
potency_tidy <- potency %>%
  # was going to use tidyr to spread variable names but some sample_ids have up to 15 tests
  # so summarizing first, assuming max() is most accurate and that others might be missing
  # values but need to check
  dplyr::group_by(sample_id, name) %>%
  dplyr::summarise(value = max(value)) %>%
  # spread to get column each for THC, THCA, CBD, Total
  tidyr::spread(name, value) %>%
  # CBDA was a column of all null or 0
  dplyr::select(-(CBDA), -(THC), -(THCA)) %>%
  dplyr::left_join(select(labkey, sample_id = id, inventoryparentid), by="sample_id") %>%
  dplyr::ungroup() %>%
  dplyr::select(-(sample_id))


# get names from categorization function -----
# first get list/df of inhalantnames
inhalantnames <- as.data.frame(unique(extracts$productname))
# rename column so we can call it
colnames(inhalantnames) <- "productname"
# bringing in classification function
source("categorization_function.R")
inhalantnames <- inhalantnames %>%
  filter(!is.na(productname)) %>%
  rowwise() %>%
  mutate(inhalant_type = categorizeNames(productname),
         inhalant_gen = groupProductTypes(inhalant_type),
         inhalant_genOil = groupProductTypesOilSep(inhalant_type))
# join classified inhalantnames back to dispening df
extracts$productname <- as.factor(extracts$productname)
extracts <- left_join(extracts, inhalantnames, by="productname")

tests <- tests %>%
  dplyr::left_join(select(labkey, sample_id = id, inventoryparentid), by="sample_id") %>%
  dplyr::group_by(inventoryparentid, name) %>%
  # getting max value to handle that some have multiple tests run,
  # which may be error in the data
  dplyr::summarise(
    value = max(value)
  ) %>%
  tidyr::spread(name, value) %>%
  dplyr::group_by(inventoryparentid) %>%
  dplyr::summarise(
    bacteria = !is.na(aerobic_bacteria),
    bile = !is.na(bile_tolerant),
    coliforms = !is.na(coliforms),
    ecoli = !is.na(e_coli_and_salmonella),
    yeast = !is.na(yeast_and_mold),
    moisture = !is.na(moisture),
    solvent = !is.na(residual_solvent)
  ) %>%
  dplyr::left_join(potency_tidy, by="inventoryparentid")

# convert to numeric from TRUE / FALSE
tests_numeric <- tests * 1

# get df of lab data types to add to test df
lab_invtypes <- labkey %>%
  group_by(inventoryparentid) %>%
  # handles the 8 inventory types that have more than one type listed
  summarise(inventorytype = inventorytype[1]) %>%
  left_join(inv_types, by="inventorytype") %>%
  select(-(inventorytype), lab_invtype = inv_type_name)

# joining to the extracts df results in an increase in tests_numeric from
# ~150k entries to ~2.5 million, which makes sense because a particular 
# inventoryid may be linked to multiple retail products
tests_numeric <- tests_numeric %>%
  dplyr::left_join(select(extracts, inventoryparentid, price_x, price_per_gram,
                          inhalant_type, inhalant_gen, productname),
                   by="inventoryparentid") 
# join the lab's inventory types back to the tests df
tests_numeric <- tests_numeric %>%
  dplyr::left_join(lab_invtypes, by="inventoryparentid") 


# productname and therefore classification type are missing from about 10% of these
# it's also missing from 13% of the extracts df
# price is missing from 5%
# potency is missing from 5%
tests_numeric <- dplyr::filter(tests_numeric, !(is.na(price_per_gram)), !(is.na(CBD)), !(is.na(Total)),
                               # calculation for price per gram will be more accurate
                               price_per_gram>0)

tests_numeric$inhalant_gen <- as.factor(tests_numeric$dplyr::)
tests_numeric$inhalant_type <- as.factor(tests_numeric$inhalant_type)
tests_numeric$lab_invtype <- as.factor(tests_numeric$lab_invtype)


# exploring lab / processor types ------
labtypes_heat <- tests_numeric %>%
  dplyr::group_by(lab_invtype) %>%
  dplyr::mutate(lab_type_count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(lab_invtype, inhalant_gen) %>%
  dplyr::summarise(count = n(),
                   `% in Lab Inv Type` = count / lab_type_count[1])

# heat map of % of retail type that are of each lab inventory type
# dropping uncategorized and flower to focus on testing if
# these will help us categorize other things
labtypes_heat %>%
  dplyr::filter(lab_invtype!="Flower Lot", !is.na(lab_invtype), !is.na(inhalant_gen),
                inhalant_gen!="Uncategorized", lab_invtype!="Marijuana Extract for Inhalation") %>%
  ggplot(aes(x = inhalant_gen, y = lab_invtype)) + 
  geom_tile(aes(fill = `% in Lab Inv Type`), colour = "white") +
  scale_fill_gradient(low = "lightcyan2", high = "turquoise4") +
  labs(title="Retail Type by Lab Type",
       y="Lab Type",
       x="Retail Type") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "gray99"))

#limited to distribution of uncatergorized
labtypes_heat %>%
  dplyr::filter(inhalant_gen=="Uncategorized") %>%
  ggplot(aes(x = inhalant_gen, y = lab_invtype)) + 
  geom_tile(aes(fill = `% in Lab Inv Type`), colour = "white") +
  scale_fill_gradient(low = "lightcyan2", high = "turquoise4") +
  labs(title="Retail Type by Lab Type",
       y="Lab Type",
       x="Retail Type") +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "gray99"))

# test and train -------
# Randomly select 20% of the data to be held out for model validation
train <- sample(1:nrow(tests_numeric), 
                round(0.2 * nrow(tests_numeric)))
test <- setdiff(1:nrow(tests_numeric), train)

tests.train <- tests_numeric[train,]
tests.test <- tests_numeric[test,]
# train.def <- tests_numeric$inhalant_gen[train]
# test.def <- tests_numeric$inhalant_gen[test]

# k means -----
set.seed(427)
km.out <- kmeans(tests.train[,c(2:10, 12)], 5, nstart = 20)
# join clusters back to df
tests.train$cluster <- km.out$cluster

table(tests.train$cluster, tests.train$inhalant_gen)
table(tests.train$cluster, tests.train$inhalant_type)

# very small groups in Groups 1 & 2, re-running without those
outliers <- filter(tests.train, 
                   #inhalant_type=="Uncategorized", 
                   cluster==3)
tests.train <- filter(tests.train, !(inventoryparentid %in% outliers$inventoryparentid))







km.out <- kmeans(tests.train[, 2:8], 4, nstart = 20)
# join clusters back to df
tests.train$cluster <- km.out$cluster
table(tests.train$cluster, tests.train$inhalant_gen)
table(tests.train$cluster, tests.train$inhalant_type)
table(tests.train$cluster, tests.train$lab_invtype)




# k nearest neighbors
library(class)
# Randomly select 20% of the data to be held out for model validation
train <- sample(1:nrow(tests_numeric), 
                round(0.2 * nrow(tests_numeric)))
test <- setdiff(1:nrow(tests_numeric), train)

tests.train <- tests_numeric[train, c(2:10, 12)]
tests.test <- tests_numeric[test, c(2:10, 12)]
train.def <- tests_numeric$inhalant_gen[train]
test.def <- tests_numeric$inhalant_gen[test]
knn.pred <- knn(tests.train, tests.test, train.def, k=5)
100 * sum(test.def == knn.pred)/100 













# random forests -------
library(randomForest)
extracts.rf <- randomForest(inhalant_gen ~ bacteria + bile + coliforms + ecoli + yeast + moisture +
                              solvent + price + CBD + Total, ntree=30, 
                            data=tests_numeric, importance=TRUE)
plot(road.rf)
var.imp.road <- varImpPlot(road.rf)
rownames(var.imp.road)[1:4]

table(tests_numeric$inhalant_gen)













# Randomly select 20% of the data to be held out for model validation -------
train <- sample(1:nrow(extract_results.sample), 
                round(0.2 * nrow(extract_results.sample)))
test <- setdiff(1:nrow(extract_results.sample), train)

extract.train <- extract_results.sample[train,]
extract.test <- extract_results.sample[test,]
train.def <- extract_results.sample$inhalant_gen[train]
test.def <- extract_results.sample$inhalant_gen[test]





# k means -----
km.out <- kmeans(extract.train, 5, nstart = 20)


# mclust ----
library(mclust)
extract.clust = Mclust(extract.train[, 2:10], G = 5)

# random forests -------
library(randomForest)
extracts.rf <- randomForest(inhalant_gen ~ name + received_test + failure + lab_invname + price + 
                              retail_invname + strain + CBD + Total, ntree=30, 
                            data=extract.train, importance=TRUE)
plot(road.rf)
var.imp.road <- varImpPlot(road.rf)
rownames(var.imp.road)[1:4]

