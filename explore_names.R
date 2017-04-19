library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)

# for inhalants --------

inhalants <- readr::read_csv("../data/Dec2016/cleaned/samples/inhalants.csv")

inhal_names <- unique(inhalants$inv_productname)

# word cloud
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

# Load the data as a corpus
docs <- Corpus(VectorSource(inhal_names))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
#docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=250, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Paired"))

# without product "type" keywords
d.2 <- dplyr::filter(d, word !="cartridge" & word!="wax" & word!="vape" & word!="shatter" &
                       word!="cart" & word!="gram")
wordcloud(words = d.2$word, freq = d.2$freq, min.freq = 1,
          max.words=300, random.order=TRUE, rot.per=0.4, 
          colors=brewer.pal(12, "Paired"))



# for edibles -------

edibles <- readr::read_csv("../data/Dec2016/cleaned/Frequently Requested Lists/MJ-Infused-Producs-Jan-2017.csv")

edibles_names <- unique(edibles$`Marijuana-Infused Product, Label, Packaging`)

# word cloud
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

# Load the data as a corpus
docs <- Corpus(VectorSource(edibles_names))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
#docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

d.2 <- dplyr::filter(d, word !="servings" & word!="serving" & word!="adn")
wordcloud(words = d.2$word, freq = d.2$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.45, 
          colors=brewer.pal(12, "Dark2"), width=12, height=8)



# all product names ------

inventory <- readr::read_csv("../data/Dec2016/biotrackthc_inventory.csv")
locations <- readr::read_csv("../data/Dec2016/cleaned/locations/all_locations.csv")
retailloc <- filter(locations, typesimp=="Retailer")

inv_retail <- filter(inventory, location %in% retailloc$location_id)
rm(inventory)

prodnames <- as.data.frame(unique(inv_retail$productname))
colnames(prodnames) <- "names"

prodnames$names <- gsub('[[:digit:]]+', '', prodnames$names)
prodnames$names <- gsub('-','',prodnames$names) 

names.df <- data_frame(row.names=1:nrow(prodnames), text = as.character(prodnames$names))

names.df <- names.df %>%
  unnest_tokens(word, text)

words.count <- names.df %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# dropping measurements, package words
words.count <- dplyr::filter(words.count, word !="gram" & word!="grams" & word!="g" & word!="mg" &
                               word!="pk", word!="pp", word!="oz", word!="pack", word!="prepack",
                             word!="preroll", word!="gr", word!="h", word!="j", word!="a",
                             word!="p", word!="b", word!="c", word!="s", word!="sample",
                             word!="usable", word!="w", word!="fl", word!="i", word!="d")

wordcloud(words = words.count$word, freq = words.count$count, min.freq = 50,
          max.words=250, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(10, "Set2"))

