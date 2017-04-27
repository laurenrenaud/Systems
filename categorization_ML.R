library(dplyr)
library(ggplot2)
library(readr)

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

knn.pred <- knn(extract.train, extract.test, train.def, k=5)
100 * sum(test.def == knn.pred)/100 


# k means -----
km.out <- kmeans(extract.train, 5, nstart = 20)


# mclust ----
library(mclust)
extract.clust = Mclust(extract.train[, 2:10], G = 5)

