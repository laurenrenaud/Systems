#create search terms for each product type
cartridge.strings <- "cart|vap|vc|pen|refill|juju|joint"
oil.strings <- "oil|rso|eso"
hash.strings <- "hash"
kief.strings <- "kief|keif"
wax.strings <- "wax"
shatter.strings <- "shatter"
dab.strings <-"dab"
resin.strings <- "resin|rosin"

categorizeNames <- function(productName){
  #' Takes product name and categorizes it into a 
  #' product category type
  #' @param productName  A string of inhalant product names
  #' @return A categorized usage of the productname as a string.
  
  # first check for cartridges. allow for oil to be in product name so that 
  # for example "oil cartridge" will be classified as a cartridge
  if(grepl( cartridge.strings, productName, ignore.case = T) == TRUE & 
     grepl(paste(hash.strings, kief.strings, wax.strings,shatter.strings, dab.strings, 
                 resin.strings, sep = "|"), productName, ignore.case = T) == FALSE) {
    return("Cartridge")
  }
  # now check for oil products. allow for hash to also be in product name so that "hash oil" is classified as oil
  else if(grepl(oil.strings, productName, ignore.case = T) == TRUE & 
     grepl(paste(hash.strings, kief.strings, wax.strings,shatter.strings, dab.strings, resin.strings, sep = "|"), 
           productName, ignore.case = T) == FALSE) {
    return("Oil")
  }
  # check for hash products. Allow for no overlap with other products
  else if(grepl( hash.strings, productName, ignore.case = T) == TRUE & 
          grepl(paste(cartridge.strings, kief.strings,oil.strings, wax.strings,shatter.strings, 
                      dab.strings, resin.strings, sep = "|"), productName, ignore.case = T) == FALSE) {
    return("Hash")
  }
  # check for kief products. Allow for no overlap with other products
  else if(grepl( kief.strings, productName, ignore.case = T) == TRUE & 
          grepl(paste(cartridge.strings, hash.strings,oil.strings, wax.strings,shatter.strings, dab.strings, 
                      resin.strings, sep = "|"), productName, ignore.case = T) == FALSE) {
    return("Kief")
  }
  # check for wax products. Allow for no overlap with other products
  else if (grepl( wax.strings, productName, ignore.case = T) == TRUE & 
           grepl(paste(cartridge.strings, kief.strings,oil.strings, hash.strings,shatter.strings, dab.strings, 
                       resin.strings, sep = "|"), productName, ignore.case = T) == FALSE) {
    return("Wax")
  }
  #check for shatter products. Allow for no overlap with other products
  else if (grepl( shatter.strings, productName, ignore.case = T) == TRUE & 
           grepl(paste(cartridge.strings, kief.strings,oil.strings, wax.strings, hash.strings, dab.strings, 
                       resin.strings, sep = "|"), productName, ignore.case = T) == FALSE) {
    return("Shatter")
  }
  #check for dab products. Allow for no overlap with other products
  else if (grepl( dab.strings, productName, ignore.case = T) == TRUE & 
           grepl(paste(cartridge.strings, kief.strings,oil.strings, wax.strings,shatter.strings, hash.strings, 
                       resin.strings, sep = "|"), productName, ignore.case = T) == FALSE) {
    return("Dab")
  }
  #check for resin products. Allow for no overlap with other products
  else if (grepl( resin.strings, productName, ignore.case = T) == TRUE & 
           grepl(paste(cartridge.strings, kief.strings,oil.strings, wax.strings,shatter.strings, dab.strings, 
                       hash.strings, sep = "|"), productName, ignore.case = T) == FALSE) {
    return("Resin")
  }
  else return("Uncategorized")
}


groupProductTypes <- function(productType){
  #' Takes product type and categorizes it into a 
  #' product category grouping
  #' @param productType  A string of inhalant product type
  #' @return A grouped usage of the product type as a string.
  
  if(productType=="Cartridge" | productType=="Oil") {
    return("Cartridge/Oil")
  }
  
  else if(productType=="Hash" | productType=="Kief") {
    return("Hash/Kief")
  }
  
  else if(productType=="Wax" | productType=="Shatter" | productType=="Dab" | productType=="Resin") {
    return("Wax/Shatter/Dab/Resin")
  }
  
  else return("Uncategorized")
}


groupProductTypesOilSep <- function(productType){
  #' Takes product type and categorizes it into a 
  #' product category grouping
  #' @param productType  A string of inhalant product type
  #' @return A grouped usage of the product type as a string.
  
  if(productType=="Cartridge") {
    return("Cartridge")
  }
  
  else if(productType=="Oil") {
    return("Oil")
  }
  
  else if(productType=="Hash" | productType=="Kief") {
    return("Hash/Kief")
  }
  
  else if(productType=="Wax" | productType=="Shatter" | productType=="Dab" | productType=="Resin") {
    return("Wax/Shatter/Dab/Resin")
  }
  
  else return("Uncategorized")
}



groupProcessorProductTypes <- function(productType){
  #' Takes product type and categorizes it into a 
  #' product category grouping
  #' @param productType  A string of  product type
  #' @return A grouped usage of the product type as a string.
  
  if(productType=="Clone" | productType=="Mature Plant" | productType=="Flower Lot" | 
     productType=="Other Plant Material Lot" | productType=="Seed" | productType=="Plant Tissue" |
     productType=="Wet Flower") {
    return("Plant/Material")
  }
  else if(productType=="Sample Jar") {
    return("Sample")
  }
  else if(productType=="Usable Marijuana" | productType=="Flower") {
    return("Usable Marijuana")
  }
  else if(productType=="Marijuana Extract for Inhalation" | productType=="CO2 Hash Oil") {
    return("Extracts for Inhalation")
  }
  else if(productType=="Kief" | productType=="Hash") {
    return("Hash/Kief")
  }
  else if(productType=="Solid Marijuana Infused Edible" | productType=="Liquid Marijuana Infused Edible") {
    return("Edible")
  }
  else if(productType=="Food Grade Solvent Extract" | productType=="Infused Cooking Oil" | 
          productType=="Infused Dairy Butter or Fat in Solid Form") {
    return("Edibles Ingredients")
  }
  else if(productType=="Bubble Hash" | productType=="Hydrocarbon Wax") {
    return("Wax/BubbleHash")
  }
  else if(productType=="Marijuana Infused Topicals" | productType=="Capsule" | productType=="Tincture" | productType=="Suppository") {
    return("Topical/Capsule/Tincture/Suppository")
  }
  else if(productType=="Marijuana Mix" | productType=="Marijuana Mix Package" | productType=="Marijuana Mix Infused") {
    return("Topical/Capsule/Tincture/Suppository")
  }
  
  else return("Uncategorized")
}
