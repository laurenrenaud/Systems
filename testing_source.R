library(ggplot2)
library(dplyr)

retail_oct <- readr::read_csv("../data/Dec2016/cleaned/testing/oct_inhalant_prodnamesonly.csv")

retail_oct <- select(retail_oct, retail_prodname = productname)


# pull in category function
source("testing_partitionFunction.R")
# use categeories
retail_oct$category <- as.character(sapply(as.character(retail_oct$retail_prodname), categorizeProductName))

table(retail_oct$category)
