library(ggplot2)
library(dplyr)

retail_oct <- readr::read_csv("../data/Dec2016/cleaned/testing/oct_inhalant_prodnamesonly.csv")

retail_oct <- select(retail_oct, retail_prodname = productname)


# pull in category function
source("categorization_function.R")
# use categeories
retail_oct$category <- as.factor(sapply(as.character(retail_oct$retail_prodname), categorizeNames))

table(retail_oct$category)

# uncategorized is higher now, can think later about why, how much of a difference this "strict"
# classification makes
sum(retail_oct$category=="Uncategorized") / nrow(retail_oct)
