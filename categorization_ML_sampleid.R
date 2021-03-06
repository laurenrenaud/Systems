library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(topicmodels)
options(scipen=4)

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
# # get names from categorization function -----
# # first get list/df of inhalantnames
# inhalantnames <- as.data.frame(unique(extracts$productname))
# # rename column so we can call it
# colnames(inhalantnames) <- "productname"
# # bringing in classification function
# source("categorization_function.R")
# inhalantnames <- inhalantnames %>%
#   filter(!is.na(productname)) %>%
#   rowwise() %>%
#   mutate(inhalant_type = categorizeNames(productname),
#          inhalant_gen = groupProductTypes(inhalant_type),
#          inhalant_genOil = groupProductTypesOilSep(inhalant_type))
# # join classified inhalantnames back to dispening df
# extracts$productname <- as.factor(extracts$productname)
# extracts <- left_join(extracts, inhalantnames, by="productname")
# 
# write.table(extracts, file="../data/Dec2016/cleaned/samples/extracts.csv", sep=",", row.names=F)

# pull in data -------
extracts <- readr::read_csv("../data/Dec2016/cleaned/samples/extracts.csv")
extracts$inventoryparentid <- as.numeric(extracts$inventoryparentid)
extracts$sample_id <- as.numeric(extracts$sample_id)
potency <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_potency_analysis.csv")
micro <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_micro_screening.csv")
moisture <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_moisture_content.csv")
solvent <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_solvent_screening.csv")
inv_types <- readr::read_csv("../data/Dec2016/cleaned/inventory_type.csv")
# combine the non-potency tests
tests <- rbind(micro, moisture, solvent)
labkey <- readr::read_csv("../data/Dec2016/biotrackthc_labresults_samples.csv")
labkey$inventoryid <- as.numeric(labkey$inventoryid)
labkey$id <- as.numeric(labkey$id)
labkey <- rename(labkey, sample_id = id)

# using sample_id instead of inventoryparentid
sum(is.na(extracts$sample_id)) / nrow(extracts)
# about 2% of sample_id's are missing

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
  dplyr::left_join(select(labkey, sample_id), by="sample_id") %>%
  dplyr::ungroup()


tests <- tests %>%
  dplyr::left_join(select(labkey, sample_id), by="sample_id") %>%
  dplyr::group_by(sample_id, name) %>%
  # getting max value to handle that some have multiple tests run,
  # which may be error in the data
  dplyr::summarise(
    value = max(value)
  ) %>%
  tidyr::spread(name, value) %>%
  dplyr::group_by(sample_id) %>%
  dplyr::summarise(
    bacteria = !is.na(aerobic_bacteria),
    bile = !is.na(bile_tolerant),
    coliforms = !is.na(coliforms),
    ecoli = !is.na(e_coli_and_salmonella),
    yeast = !is.na(yeast_and_mold),
    moisture = !is.na(moisture),
    solvent = !is.na(residual_solvent)
  ) %>%
  dplyr::left_join(potency_tidy, by="sample_id")

# convert to numeric from TRUE / FALSE
tests_numeric <- tests * 1

# how many have multiple inventorytypes per one sample_id
lab_multi_type <- labkey %>%
  group_by(sample_id, inventorytype) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(sample_id) %>%
  summarise(num_of_types = n())
# all only have 1
summary(lab_multi_type$num_of_types)

# get df of lab data types to add to test df
lab_invtypes <- labkey %>%
  group_by(sample_id) %>%
  summarise(inventorytype = inventorytype) %>%
  left_join(inv_types, by="inventorytype") %>%
  select(-(inventorytype), lab_invtype = inv_type_name)

# joining to the extracts df results in an increase in tests_numeric from
# ~150k entries to ~2.5 million, which makes sense because a particular 
# inventoryid may be linked to multiple retail products
tests_numeric <- tests_numeric %>%
  dplyr::left_join(select(extracts, sample_id, price_x, price_per_gram,
                          inhalant_type, inhalant_gen, productname),
                   by="sample_id") 
# join the lab's inventory types back to the tests df
tests_numeric <- tests_numeric %>%
  dplyr::left_join(lab_invtypes, by="sample_id") 


# productname and therefore classification type are missing from about 10% of these
# it's also missing from 13% of the extracts df
# price is missing from 5%
# potency is missing from 5%
tests_numeric <- dplyr::filter(tests_numeric, !(is.na(price_per_gram)), !(is.na(CBD)), 
                               !(is.na(Total)),
                               # calculation for price per gram will be more accurate
                               price_per_gram>0)

tests_numeric$inhalant_gen <- as.factor(tests_numeric$inhalant_gen)
tests_numeric$inhalant_type <- as.factor(tests_numeric$inhalant_type)
tests_numeric$lab_invtype <- as.factor(tests_numeric$lab_invtype)

# uncategorized df -----
uncategorized <- filter(tests_numeric, inhalant_gen=="Uncategorized")
classified <- filter(tests_numeric, inhalant_gen!="Uncategorized", !is.na(inhalant_gen))


# lab types heat maps ------
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
        panel.background = element_rect(fill = "gray99"), 
        axis.text.x = element_text(angle = 5, hjust = 0.8, vjust = 0.9))

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

# types per product name --------
# how many have multiple inventorytypes per one sample_id
extracts$sample_id <- as.numeric(extracts$sample_id)
extracts_multi_type <- extracts %>%
  left_join(lab_invtypes, by="sample_id") %>%
  group_by(productname, lab_invtype) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(productname) %>%
  summarise(num_of_types = n())
# all only have 1
summary(extracts_multi_type$num_of_types)

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




# k nearest neighbors ---------
library(class)
# Randomly select 20% of the data to be held out for model validation
train <- sample(1:nrow(classified), 
                round(0.2 * nrow(classified)))
test <- setdiff(1:nrow(classified), train)

tests.train <- classified[train, c(2:10, 12)]
tests.test <- classified[test, c(2:10, 12)]
train.def <- classified$inhalant_gen[train]
test.def <- classified$inhalant_gen[test]
knn.pred <- knn(tests.train, tests.test, train.def, k=4)

# joining back to definitions
classifed_predictions <- cbind(tests.test, knn.pred, test.def)
table(classifed_predictions$knn.pred, classifed_predictions$test.def)

# using model on unclassified
uncat <- uncategorized[, c(2:10, 12)]
knn.pred <- knn(tests.train, uncat, train.def, k=5)
uncat_predictions <- cbind(uncat, knn.pred, uncategorized$productname)
head(uncat_predictions)
table(uncat_predictions$knn.pred)

# trying to find variable importance
#knn.pred$results$Rsquared

# trying to plot accuracy
# from: https://rpubs.com/potentialwjy/MachineLearning3
# Load the class package, define range and accs
library(class)
range <- 1:round(0.2 * nrow(tests.train))
accs <- rep(0, length(range))

for (k in range) {
  # Make predictions using knn: pred
  pred <- knn(tests.train, tests.test, train.def, k=k)
  # Construct the confusion matrix: conf
  conf <- table(test.def, pred)
  # Calculate the accuracy and store it in accs[k]
  accs[k] <- sum(diag(conf)) / sum(conf)
  }

# Plot the accuracies. Title of x-axis is "k".
plot(range, accs, xlab="k")



# features <- function(df, m = 1){
#   nf <- ncol(df) -1 # number of input features
#   idx <- 1: nf  # column indices of input features
#   output <- df[, ncol(df)]  # output column
#   outputH <- entropy(output) # entropy for output
#   idx.list <- combn(idx, m) # matrix storing all combinations of size m from idx
#   IG.res <-NULL # output data frame
#   # iterate through all combinations of index
#   for (ii in 1:ncol(idx.list)){
#     this.idx <- idx.list[, ii]
#     input.df <- data.frame(df[,this.idx])
#     # create a vector where each element is a concatenation of all values of a row of a data frame
#     this.input <- apply(input.df, 1, paste, collapse='')
#     # create a new feature name which is a concatenation of feature names in a feature set
#     this.input.names <- paste(names(df)[this.idx], collapse=' ')
#     this.IG <-info.gain(this.input,output) # information gain
#     this.RIG <- this.IG / outputH # relative information gain
#     this.res <- data.frame(feature = this.input.names, IG = this.IG, RIG = this.RIG) #assemble a df
#     IG.res <- rbind(IG.res, this.res) # concatenate the results
#   }
#   sorted <- IG.res[order(-IG.res$IG), ] # sort the result by information gain in a descending order
#   return (sorted)
# }
# 
# features(classifed_predictions, m=12)

# 
# 
# #returns IG for categorical variables.
# IG_cat<-function(data,feature,target){
#   #Strip out rows where feature is NA
#   data<-data[!is.na(data[,feature]),] 
#   #use ddply to compute e and p for each value of the feature
#   dd_data<-ddply(data, feature, here(summarise), e=entropy(get(target)), N=length(get(target)))
#   #compute entropy for the parent
#   e0<-entropy(data[,target])
#   #calculate p for each value of feature
#   dd_data$p<-dd_data$N/nrow(data)
#   #compute IG
#   IG<-e0-sum(dd_data$p*dd_data$e)
#   
#   return(IG)
# }








# random forests -------
library(randomForest)
extracts.rf <- randomForest(inhalant_gen ~ bacteria + bile + coliforms + ecoli + yeast + moisture +
                              solvent + price_per_gram + CBD + Total, ntree=30, 
                            data=classified, importance=TRUE)

extracts.rf <- randomForest(inhalant_gen ~ bacteria + bile + coliforms + ecoli + yeast + moisture +
                              solvent + price_per_gram + CBD + Total, ntree=30, 
                            data=classified, importance=TRUE)
plot(road.rf)
var.imp.road <- varImpPlot(road.rf)
rownames(var.imp.road)[1:4]

table(tests_numeric$inhalant_gen)



# mclust ----
library(mclust)
extract.clust = Mclust(extract.train[, 2:10], G = 5)


# LDA text -------

# attemping LDA : Latent Dirichlet allocation
# http://tidytextmining.com/topicmodeling.html
# http://tidytextmining.com/tidytext.html

# 1: convert strings into dataframe
# and need to include which "line" it comes from
#productnames.df <- data_frame(line=1:nrow(extracts), text = extracts$productname)
# 2: use tidytext to convert into one-token-per-document-per-row.
library(tidytext)
library(stringr)
# this then includes a variable saying which line it initially came from
# removed.df <- removed.df %>%
#   unnest_tokens(word, text)

names.df <- extracts %>%
  dplyr::filter(!is.na(productname)) %>%
  dplyr::mutate(linenumber = row_number())
# sampling to run on smaller df
# sample.list <- sample(names.df$dispensingid, 200000, replace=F)
# names.sample <- dplyr::filter(names.df, dispensingid %in% sample.list)
# names.sample$productname <- as.character(names.sample$productname)

# or sampling only uncategorized
sample.list <- sample(names.df$dispensingid[names.df$inhalant_gen=="Uncategorized"],
                      200000, replace=F)
names.sample <- dplyr::filter(names.df, dispensingid %in% sample.list)
names.sample$productname <- as.character(names.sample$productname)

names.tidy <- names.sample %>%
  unnest_tokens(word, productname)

# remove stop words (but should check those)
stop_words <- as.data.frame(c("gram", "grams", "g", "mg", "oz", "0.5g", "1g", "5g",
                              "500mg", 0:9))
colnames(stop_words) <- "word"
names.tidy <- names.tidy %>%
  anti_join(stop_words)

# create DocumentTermMatrix
names.docmatrix <- names.tidy %>%
  select(document = linenumber, term = word) %>%
  group_by(document, term) %>%
  summarise(count = n()) %>%
  cast_dtm(document, term, count)

ap_lda <- LDA(names.docmatrix, k = 6, control = list(seed = 1234))
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

# ap_documents <- tidy(ap_lda, matrix = "gamma")
# ap_documents %>%
#   filter(topic==2) %>%
#   arrange(desc(gamma))

# word cloud uncategroized -----
library(tm)
library(SnowballC)
library(wordcloud)
# Load the data as a corpus
inhal_names <- unique(uncategorized$productname)
docs <- Corpus(VectorSource(inhal_names))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=300, random.order=TRUE, rot.per=0.4, 
          colors=brewer.pal(12, "Paired"))

# # without product "type" keywords
# d.2 <- dplyr::filter(d, word !="cartridge" & word!="wax" & word!="vape" & word!="shatter" &
#                        word!="cart" & word!="gram")


# sankey of classification --------

sankey.df <- filter(extracts, !is.na(inhalant_gen))
sankey.df$inhalant_gen <- as.character(sankey.df$inhalant_gen)
sankey.df$inhalant_type <- as.character(sankey.df$inhalant_type)
edges <- sankey.df %>%
  # renaming uncategorized to unknown
  # because the riverplot can't handle matching names
  dplyr::mutate(inhalant_type2 = ifelse(inhalant_type=="Uncategorized", "Unknown", inhalant_type)) %>%
  dplyr::group_by(inhalant_gen, inhalant_type2) %>%
  dplyr::summarise(
    #total_products = "All Products",
    count_products = n()
  ) %>%
  as.data.frame()

colnames(edges) <- c("N1", "N2", "Value")

nodes <- data.frame(
  ID = c(unique(edges$N1), unique(edges$N2)), stringsAsFactors= FALSE,
  col = c(rep("#e8008077", length(unique(sankey.df$inhalant_type))), 
          rep("#4a7aff77", length(unique(sankey.df$inhalant_gen))))
)

riv <- makeRiver(nodes, edges, 
                 node_xpos=c(rep.int(1,length(unique(edges$N1))), 
                             rep.int(2,length(unique(edges$N2)))))

# style the riverplot
riv.style <- riverplot::default.style()
riv.style$textcex <- 0.7
riv.style$srt <- 0
riv.style$nodestyle <- "regular"
riv.style$textcol <- "#39383a"
#riv.style$edgecol$col <- "#FF0000"
#riv.style$colorRampPaletteAlpha( c( "#FF000033", "#00FF0099" ) )( 5 )
plot(riv, default_style=riv.style, node_margin=0.2, plot_area=1, xscale=0.92)
text(-.1, .5, "Product Groupings", col= "#333333", cex=0.8, srt = 90)
text(1, .5, "Product Types", col= "#333333", srt = 270, cex=0.8)



# # adding more edges and nodes
# sankey.df <- filter(extracts, !is.na(inhalant_gen))
# sankey.df$inhalant_gen <- as.character(sankey.df$inhalant_gen)
# sankey.df$inhalant_type <- as.character(sankey.df$inhalant_type)
# edges <- sankey.df %>%
#   # renaming uncategorized to unknown
#   # because the riverplot can't handle matching names
#   dplyr::mutate(inhalant_type2 = ifelse(inhalant_type=="Uncategorized", "Unknown", inhalant_type)) %>%
#   dplyr::group_by(inhalant_gen) %>%
#   dplyr::mutate(count_groupings = n()) %>%
#   dplyr::group_by(inhalant_gen, inhalant_type2) %>%
#   dplyr::summarise(
#     all_products = "all_products",
#     count_groupings = count_groupings[1],
#     count_products = n()
#   ) %>%
#   as.data.frame()
# 
# colnames(edges) <- c("N2", "N3", "N1", "Value_1", "Value_2")
# # reorder columns
# edges <- edges[, c("N1", "N2", "Value_1", "N3", "Value_2")]
# 
# nodes <- data.frame(
#   ID = c(unique(edges$N1), unique(edges$N2), unique(edges$N3)), stringsAsFactors= FALSE,
#   col = c(rep("blue"),
#           rep("#e8008077", length(unique(sankey.df$inhalant_type))), 
#           rep("#4a7aff77", length(unique(sankey.df$inhalant_gen))))
# )
# 
# riv <- makeRiver(nodes, edges, 
#                  node_xpos=c(rep.int(1,length(unique(edges$N1))), 
#                              rep.int(2,length(unique(edges$N2))), 
#                              rep.int(3,length(unique(edges$N3)))))
# 
# # style the riverplot
# riv.style <- riverplot::default.style()
# riv.style$textcex <- 0.7
# riv.style$srt <- 0
# riv.style$nodestyle <- "regular"
# riv.style$textcol <- "#39383a"
# #riv.style$edgecol$col <- "#FF0000"
# #riv.style$colorRampPaletteAlpha( c( "#FF000033", "#00FF0099" ) )( 5 )
# plot(riv, default_style=riv.style, node_margin=0.2, plot_area=1, xscale=0.92)
# text(-.18, .5, "Product Groupings", col= "#333333", cex=0.8, srt = 90)
# text(1, .5, "Product Types", col= "#333333", srt = 270, cex=0.8)




