cartridge_string <- "cart|vc|vap|refill|juju|joint"
oil_string <- "oil|RSO|ESO"

categorizeProductName <- function(productName){
  #' Takes product name and categorizes it into a 
  #' product category type
  #' @param productName  A string of inhalant product names
  #' @return A categorized usage of the productname as a string.
  
  # cartridge
  if(grepl(cartridge_string, productName, ignore.case=TRUE)){
    return("Cartridges")
  }
  # oil
  else if(grepl(oil_string, productName, ignore.case=TRUE)){
    return("Oil")
  }
  else{
    return("Uncategorized")
  }
}
