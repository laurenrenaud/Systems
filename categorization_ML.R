library(dplyr)
library(ggplot2)
library(readr)

# dispensing <- readr::read_csv("../data/Dec2016/biotrackthc_dispensing.csv")
# dispensing$saledate <- as.Date(as.POSIXct(dispensing$sessiontime,
#                                          origin = "1970-01-01", tz="America/Los_Angeles"))
# inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")

extracts <- dispensing %>%
  # 24 is inventory type for extracts
  dplyr::filter(inventorytype==24) %>%
  dplyr::select(dispensingid = id, weight, inventoryid, price, usableweight, saledate) %>%
  dplyr::left_join(select(inventory, inventoryid = id, strain, productname, 
                          inventoryparentid, sample_id),
                   by="inventoryid") %>%
  dplyr::mutate(price_x = ifelse(saledate >= "2015-07-01", 
                                 price*1.37,
                                 price),
                price_per_gram = price_x/usableweight) %>%
  dplyr::select(-(saledate), -(price))

# write.table(extracts, file="../data/Dec2016/cleaned/samples/extracts.csv", sep=",", row.names=T)

# pull in data -------
extracts <- readr::read_csv("../data/Dec2016/cleaned/samples/inahlants.csv")

potency <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_potency_analysis.csv")
micro <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_micro_screening.csv")
moisture <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_moisture_content.csv")
solvent <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_solvent_screening.csv")
inv_types <- readr::read_csv("../data/Dec2016/cleaned/inventory_type.csv")
# combine the non-potency tests
tests <- rbind(micro, moisture, solvent)
labkey <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_samples.csv")
labkey$inventoryid <- as.numeric(labkey$inventoryid)

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
         inhalant_gen = groupProductTypesOilSep(inhalant_type))
# join classified inhalantnames back to dispening df
extracts$productname <- as.factor(extracts$productname)
extracts <- left_join(extracts, inhalantnames, by="productname")

tests <- tests %>%
  dplyr::left_join(select(labkey, id, inventoryparentid), by=c("sample_id" = "id")) %>%
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
  ) 


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
  dplyr::left_join(select(extracts, inventoryparentid, price, CBD, Total,
                          inhalant_type, inhalant_gen, productname),
                   by="inventoryparentid") 
# join the lab's inventory types back to the tests df
tests_numeric <- tests_numeric %>%
  dplyr::left_join(lab_invtypes, by="inventoryparentid") 


# productname and therefore classification type are missing from about 10% of these
# it's also missing from 13% of the extracts df
# price is missing from 5%
# potency is missing from 5%
tests_numeric <- dplyr::filter(tests_numeric, !(is.na(price)), !(is.na(CBD)), !(is.na(Total)),
                               # calculation for price per gram will be more accurate
                               price>0, price<200)

tests_numeric$inhalant_gen <- as.factor(tests_numeric$inhalant_gen)
tests_numeric$inhalant_type <- as.factor(tests_numeric$inhalant_type)
tests_numeric$lab_invtype <- as.factor(tests_numeric$lab_invtype)

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
km.out <- kmeans(tests.train[, 2:11], 5, nstart = 20)
# join clusters back to df
tests.train$cluster <- km.out$cluster

table(tests.train$cluster, tests.train$inhalant_gen)
table(tests.train$cluster, tests.train$inhalant_type)

# very small groups in Groups 1 & 2, re-running without those
outliers <- filter(tests.train, 
                   inhalant_type=="Uncategorized", 
                   cluster==1)
tests.train <- filter(tests.train, !(inventoryparentid %in% outliers$inventoryparentid))

km.out <- kmeans(tests.train[, 2:8], 4, nstart = 20)
# join clusters back to df
tests.train$cluster <- km.out$cluster
table(tests.train$cluster, tests.train$inhalant_gen)
table(tests.train$cluster, tests.train$inhalant_type)
table(tests.train$cluster, tests.train$lab_invtype)







# random forests -------
library(randomForest)
extracts.rf <- randomForest(inhalant_gen ~ bacteria + bile + coliforms + ecoli + yeast + moisture +
                              solvent + price + CBD + Total, ntree=30, 
                            data=tests_numeric, importance=TRUE)
plot(road.rf)
var.imp.road <- varImpPlot(road.rf)
rownames(var.imp.road)[1:4]

table(tests_numeric$inhalant_gen)












extract_results <- labkey %>%
  # filter to kief, bubble hash, hash, hydrocarbon wax, co2 hash oil, extracts for inhalation
  dplyr::filter(inventorytype==5 | inventorytype==15 | inventorytype==16 | inventorytype==17 |
                  inventorytype==18 | inventorytype==24) %>%
  dplyr::select(sample_id = id, lab_invid = inventoryid, lab_invtype = inventorytype, lab_prodname = product_name, 
                inventoryparentid) %>%
  dplyr::left_join(tests, by=c("sample_id")) %>%
  dplyr::left_join(inv_types, by=c("lab_invtype" = "inventorytype")) %>%
  dplyr::rename(lab_invname = inv_type_name) %>%
  dplyr::left_join(select(extracts, dispensingid, price, retail_invname = inv_type_name, 
                          retail_invid = inventoryid, strain, retail_prodname = productname,
                          CBD, Total, inventoryparentid, inhalant_type, inhalant_gen), 
                   by="inventoryparentid")

extract_results.list <- sample(extract_results$inventoryparentid, 1000, replace=F)

extract_results.sample <- extract_results %>%
  dplyr::filter(inventoryparentid %in% extract_results.list, !is.na(name),
                inhalant_type!="Uncategorized", !is.na(inhalant_type), !is.na(lab_invname),
                !is.na(price), !is.na(retail_invname), !is.na(strain), !is.na(CBD), !is.na(Total)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(received_test = !is.na(value)) %>%
  dplyr::select(dispensingid, name, received_test, failure, lab_invname, price, retail_invname, 
                strain, CBD, Total,
                inhalant_type, inhalant_gen)


# Randomly select 20% of the data to be held out for model validation -------
train <- sample(1:nrow(extract_results.sample), 
                round(0.2 * nrow(extract_results.sample)))
test <- setdiff(1:nrow(extract_results.sample), train)

extract.train <- extract_results.sample[train,]
extract.test <- extract_results.sample[test,]
train.def <- extract_results.sample$inhalant_gen[train]
test.def <- extract_results.sample$inhalant_gen[test]


# k nearest neighbors
library(class)
train.def <- tests_numeric$inhalant_gen[train]
test.def <- tests_numeric$inhalant_gen[test]
knn.pred <- knn(tests.train, tests.test, train.def, k=5)
100 * sum(test.def == knn.pred)/100 


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

