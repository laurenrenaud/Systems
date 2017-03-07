library(ggplot2)
library(knitr)
library(dplyr)

setwd("~/DrugData/samples")

Weed_Sales <- read.csv("retail_sample.csv", sep=",", header=T)
Weed_Sales$saletime <- as.Date(Weed_Sales$saletime)

Extracts <- read.csv("inahlants.csv")

summary(Weed_Sales)

#basics on potency
summary(Weed_Sales$THC)


#distribution of potency for products and lastly total------- 
ggplot(subset(Weed_Sales, inv_type_name == "Marijuana Extract for Inhalation"), aes(Total)) + geom_density(fill = "chartreuse4") 

ggplot(subset(Weed_Sales, inv_type_name == "Liquid Marijuana Infused Edible" & inv_type_name == "Solid Marijuana Infused Edible"), aes(Total)) + geom_density(fill = "chartreuse4")

ggplot(subset(Weed_Sales, inv_type_name == "Usable Marijuana"), aes(Total)) + geom_density(fill = "chartreuse4") 

ggplot(Weed_Sales, aes(Total)) + geom_density(fill = "chartreuse3")








#breakdown by product type-----------


ggplot(subset(Weed_Sales, Total < 100), aes(x= inv_type_name, y = Total,)) + geom_boxplot()





#ignore this, go to next section in outline

#Price and Potency
ggplot(subset(Weed_Sales, price > 0 & price < 200 & THC > 0), aes(x=price, y=THC)) 
  + geom_point() + geom_smooth(method = "lm")
                                                               
ggplot(subset(Weed_Sales, price > 0 & price < 200 & THC > 0), aes(x=THC, y=price)) +
  geom_point() + geom_smooth(method = "lm") + facet_wrap(~ inv_type_name)

#potency and time
ggplot(subset(Weed_Sales, price > 0 & price < 500 & THC > 0), aes(x=saletime, y=THC)) 
+ geom_line()

#Summary statistics
inhailaints <- Weed_Sales %>%
  dplyr::filter(unitPrice > 0, THC > 0, inv_type_name == "Marijuana Extract for Inhalation") %>%
  dplyr::group_by(productname) %>%
  dplyr::summarise(avg_potency = median(Total, na.rm=T), avg_price = median(unitPrice, na.rm=T) )

#look at carts
carts <- grep("vap|cart|VC|Cartridge|Vape|Cart", inhailaints$productname, value =T)
carts <- data_frame(carts)
carts$productname <- carts$carts

all.carts <- inner_join(carts, inhailaints, by = "productname")

#look at wax
matches <- grep("wax|WAX|Wax", inhailaints$productname, value =T)
wax <- data_frame(matches)
wax$productname <- wax$matches

all.wax <- inner_join(wax, inhailaints, by = "productname")

#look at oil
matches <- grep("oil|OIL|Oil", inhailaints$productname, value =T)
oil <- data_frame(matches)
oil$productname <- oil$matches

all.oil <- inner_join(oil, inhailaints, by = "productname")

#look at hash
matches <- grep("hash|Hash|HASH|Kief|kief|KIEF", inhailaints$productname, value =T)
hash <- data_frame(matches)
hash$productname <- hash$matches

all.hash <- inner_join(hash, inhailaints, by = "productname")

#look at shatter
matches <- grep("shatter|Shatter|SHATTER", inhailaints$productname, value =T)
shat <- data_frame(matches)
shat$productname <- shat$matches

all.shat <- inner_join(shat, inhailaints, by = "productname")







Weed_Sales %>%
  dplyr::filter(price > 0, price < 500, THC > 0) %>%
  dplyr::group_by(saletime) %>%
  dplyr::summarise(avg_daily_thc = median(THC, na.rm=T)) %>%
  ggplot(aes(x=saletime, y=avg_daily_thc)) + 
  geom_point() +
  geom_smooth(method="loess", color="darkgreen")

Weed_Sales %>%
  dplyr::filter(price > 0, price < 500, THC > 0) %>%
  ggplot(aes(x=saletime, y=THC))


topCity <- Weed_Sales$city == "SEATTLE" | Weed_Sales$city == "TACOMA" | Weed_Sales$city == "SPOKANE" | Weed_Sales$city == "VANCOUVER" | Weed_Sales$city == "EVERETT"

topStrain <- Weed_Sales$strain == "Mixed" | Weed_Sales$strain == "Blue Dream" | Weed_Sales$strain == "Dutch Treat" | Weed_Sales$strain == "Super Lemon Haze" | Weed_Sales$strain == "Golden Pineapple"
 
unitPrice <- Extracts$price/Extracts$weight

Pratio <- Weed_Sales$THC/Weed_Sales$CBD

high.THC <- PriceData$avgTHC >20

Weed_Sales$topCity <- topCity
Weed_Sales$topStrain <- topStrain
Weed_Sales$pratio <- Pratio
Extracts$unitPrice <- unitPrice
PriceData$high.THC <- high.THC


#------regression and scatter for price and potency-------------------
  
PriceData <- Extracts %>%
  dplyr::filter(as.Date(saletime) > "2015-09-1" & as.Date(saletime) < "2015-9-30") %>%
  dplyr::group_by(productname, location) %>%
  dplyr::summarise( avgPrice = mean(unitPrice, na.rm=T), 
                    sdPrice = sd(unitPrice, na.rm=T),
                    avgTHC = mean(Total, na.rm=T), sdTHC = sd(Total, na.rm=T), avgCBD = mean(CBD, na.rm=T), sdCBD = sd(CBD, na.rm=T))




summary(reg)


ggplot (PriceData, aes(x=avgTHC, y=avgPrice)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess", color="green")

ggplot(subset(Weed_Sales, location == 2007),  aes(x = saletime, y = unitPrice)) + geom_point() + geom_smooth(method = "lm")

ggplot(subset(Weed_Sales, city == "SPOKANE"),  aes(x = saletime, y = unitPrice)) + geom_point() + geom_smooth(method = "lm")

ggplot(subset(Weed_Sales, city == "SEATTLE"),  aes(x = saletime, y = unitPrice)) + geom_point() + geom_smooth(method = "lm")
